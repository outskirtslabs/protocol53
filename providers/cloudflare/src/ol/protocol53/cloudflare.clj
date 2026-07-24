(ns ol.protocol53.cloudflare
  "Cloudflare DNS provider for protocol53.

  The provider uses scoped Cloudflare API tokens and implements every
  capability in [[ol.protocol53.protocols]]."
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.string :as str]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.io ByteArrayOutputStream IOException]
   [java.nio.charset StandardCharsets]
   [java.time Duration]
   [java.util Locale]))

(def ^:private base-url "https://api.cloudflare.com/client/v4")
(def ^:private max-page-size 100)
(def ^:private txt-chunk-size 255)

(defn- fail!
  ([type message retryable?]
   (fail! type message retryable? nil))
  ([type message retryable? cause]
   (throw (ex-info message
                   {::failure   true
                    ::type      type
                    ::retryable retryable?}
                   cause))))

(defn- invalid-record! []
  (fail! :invalid-record "Invalid Cloudflare record data" false))

(defn- invalid-response! []
  (fail! :provider-response "Cloudflare returned an invalid response" false))

(defn- operation-outcome
  [operation result-key zone mutated? f]
  (try
    {:ol.protocol53/result {result-key (f)}}
    (catch clojure.lang.ExceptionInfo cause
      (if-not (::failure (ex-data cause))
        (throw cause)
        (let [{type      ::type
               retryable ::retryable} (ex-data cause)]
          {:ol.protocol53/error
           (cond-> {:type       type
                    :message    (ex-message cause)
                    :operation  operation
                    :provider   :cloudflare
                    :retryable? retryable}
             zone (assoc :zone zone)
             mutated? (assoc :zone-state
                             (if @mutated? :unknown :unchanged)))})))))

(defn- timeout-millis [operation-deadline]
  (let [^Duration remaining (deadline/remaining operation-deadline)]
    (quot (.toNanos remaining) 1000000)))

(defn- ensure-time! [operation-deadline]
  (when (deadline/expired? operation-deadline)
    (fail! :deadline-exceeded "Deadline exceeded during Cloudflare request" false)))

(defn- response-body [response]
  (try
    (let [body (json/read-str (:body response))]
      (when-not (map? body)
        (invalid-response!))
      body)
    (catch Exception cause
      (if (::failure (ex-data cause))
        (throw cause)
        (fail! :provider-response
               "Cloudflare returned malformed JSON"
               false
               cause)))))

(defn- request!
  [provider operation-deadline {:keys [token method path query body]}]

  (let [headers  (cond-> {"Authorization" (str "Bearer " token)}
                   body (assoc "Content-Type" "application/json"))
        request  (cond-> {:uri     (str base-url path)
                          :method  method
                          :headers headers
                          :throw   false}
                   (seq query) (assoc :query-params query)
                   body (assoc :body (json/write-str body))
                   (:http-client provider) (assoc :client
                                                  (:http-client provider)))
        timeout  (timeout-millis operation-deadline)
        _        (when-not (pos? timeout)
                   (fail! :deadline-exceeded "Deadline exceeded during Cloudflare request" false))
        response (try
                   (http/request (assoc request :timeout timeout))
                   (catch InterruptedException cause
                     (.interrupt (Thread/currentThread))
                     (fail! :provider-request "Cloudflare request failed" true cause))
                   (catch Exception cause
                     (fail! :provider-request "Cloudflare request failed" (instance? IOException cause) cause)))]
    (ensure-time! operation-deadline)
    (let [status         (long (or (:status response) 0))
          request-failed (str "Cloudflare request failed with HTTP " status)]
      (when (>= status 400)
        (fail! :provider-request request-failed (or (= status 429) (>= status 500))))
      (let [parsed (response-body response)]
        (cond
          (seq (:errors parsed))
          (fail! :provider-request request-failed false)

          (not (true? (:success parsed)))
          (fail! :provider-response "Cloudflare returned an unsuccessful response" false)

          :else
          parsed)))))

(defn- normalized-zone [zone]
  (str/replace zone #"\.+$" ""))

(defn- absolute-name [name zone]
  (let [zone (normalized-zone zone)]
    (cond
      (or (= name "@") (empty? name)) zone
      (str/ends-with? name ".") (normalized-zone name)
      :else (str name "." zone))))

(defn- relative-name [name zone]
  (let [name   (normalized-zone name)
        zone   (normalized-zone zone)
        suffix (str "." zone)]
    (cond
      (= name zone) "@"
      (str/ends-with? name suffix)
      (subs name 0 (- (count name) (count suffix)))
      :else name)))

(defn- trailing-dot [target]
  (if (or (empty? target) (str/ends-with? target "."))
    target
    (str target ".")))

(defn- strip-trailing-dot [target]
  (if (str/ends-with? target ".")
    (subs target 0 (dec (count target)))
    target))

(defn- byte-value [b]
  (bit-and (int b) 0xff))

(defn- encoded-txt-chunk [octets]
  (let [result (StringBuilder.)]
    (.append result \")
    (doseq [octet octets]
      (cond
        (= octet 34) (.append result "\\\"")
        (= octet 92) (.append result "\\\\")
        (<= 32 octet 126) (.append result (char octet))
        :else (.append result (format "\\%03d" octet))))
    (.append result \")
    (str result)))

(defn- encode-txt [text]
  (let [octets (mapv byte-value
                     (.getBytes ^String text StandardCharsets/UTF_8))]
    (if (empty? octets)
      "\"\""
      (->> octets
           (partition-all txt-chunk-size)
           (map encoded-txt-chunk)
           (str/join " ")))))

(defn- digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn- write-character! [^ByteArrayOutputStream output c]
  (let [bytes (.getBytes (str c) StandardCharsets/UTF_8)]
    (.write output bytes 0 (alength bytes))))

(defn- decoded-txt [^String content]
  (if-not (str/starts-with? content "\"")
    content
    (let [length (count content)
          output (ByteArrayOutputStream.)]
      (loop [i (identity 0)]
        (let [i (loop [i i]
                  (if (and (< i length)
                           (or (= \space (.charAt content i))
                               (= \tab (.charAt content i))))
                    (recur (inc i))
                    i))]
          (if (= i length)
            (String. (.toByteArray output) StandardCharsets/UTF_8)
            (do
              (when-not (= \" (.charAt content i))
                (invalid-response!))
              (recur
               (loop [j (inc i)]
                 (when (= j length)
                   (invalid-response!))
                 (let [c (.charAt content j)]
                   (cond
                     (= c \")
                     (inc j)

                     (= c \\)
                     (let [next-index (inc j)]
                       (when (= next-index length)
                         (invalid-response!))
                       (let [next-char (.charAt content next-index)]
                         (if (digit? next-char)
                           (do
                             (when (>= (+ j 3) length)
                               (invalid-response!))
                             (let [second-digit (.charAt content (+ j 2))
                                   third-digit  (.charAt content (+ j 3))]
                               (when-not (and (digit? second-digit)
                                              (digit? third-digit))
                                 (invalid-response!))
                               (let [octet (+ (* 100 (- (int next-char)
                                                        (int \0)))
                                              (* 10 (- (int second-digit)
                                                       (int \0)))
                                              (- (int third-digit) (int \0)))]
                                 (when (> octet 255)
                                   (invalid-response!))
                                 (.write output octet)
                                 (recur (+ j 4)))))
                           (do
                             (write-character! output next-char)
                             (recur (+ j 2))))))

                     :else
                     (do
                       (write-character! output c)
                       (recur (inc j))))))))))))))

(defn- parsed-unsigned [value maximum]
  (try
    (let [number (Long/parseLong value)]
      (when-not (<= 0 number maximum)
        (invalid-record!))
      number)
    (catch NumberFormatException _
      (invalid-record!))))

(defn- parsed-mx [data]
  (when-let [[_ priority target]
             (re-matches #"^(\d+)\s+(.+)$" data)]
    {:priority (parsed-unsigned priority 65535)
     :target   target}))

(defn- parsed-srv [data]
  (when-let [[_ priority weight port target]
             (re-matches #"^(\d+)\s+(\d+)\s+(\d+)\s+(.+)$" data)]
    {:priority (parsed-unsigned priority 65535)
     :weight   (parsed-unsigned weight 65535)
     :port     (parsed-unsigned port 65535)
     :target   target}))

(defn- parsed-caa [data]
  (when-let [[_ flags tag value]
             (or (re-matches #"^(\d+)\s+(\S+)\s+\"(.*)\"$" data)
                 (re-matches #"^(\d+)\s+(\S+)\s+(.+)$" data))]
    {:flags (parsed-unsigned flags 255)
     :tag   tag
     :value value}))

(defn- parsed-service-binding [data]
  (when-let [[_ priority target value]
             (re-matches #"^(\d+)\s+(\S+)(?:\s+(.*))?$" data)]
    {:priority (parsed-unsigned priority 65535)
     :target   target
     :value    (or value "")}))

(defn- srv-owner [name zone]
  (let [[service proto & owner-parts] (str/split name #"\.")]
    (when-not (and (seq service) (seq proto))
      (invalid-record!))
    {:service service
     :proto   proto
     :name    (absolute-name (str/join "." owner-parts) zone)}))

(defn- canonical-type [type]
  (.toUpperCase ^String type Locale/ROOT))

(defn- record-payload [record zone]
  (let [{:keys [name ttl type data]} record
        type (canonical-type type)
        common (cond-> {:name    (absolute-name name zone)
                        :type    type
                        :content data}
                 (pos? ttl) (assoc :ttl ttl))]
    (case type
      "TXT"
      (assoc common :content (encode-txt data))

      "CNAME"
      (let [target (strip-trailing-dot data)]
        (cond-> (assoc common :content target)
          (str/ends-with? target ".cfargotunnel.com")
          (assoc :proxied true)))

      "NS"
      (assoc common :content (strip-trailing-dot data))

      "MX"
      (if-let [{:keys [priority target]} (parsed-mx data)]
        (assoc common
               :content (strip-trailing-dot target)
               :priority priority)
        (invalid-record!))

      "CAA"
      (if-let [caa (parsed-caa data)]
        (assoc common :data caa)
        (invalid-record!))

      "SRV"
      (if-let [srv (parsed-srv data)]
        (-> common
            (dissoc :content)
            (assoc :data (merge (srv-owner name zone) srv)))
        (invalid-record!))

      ("HTTPS" "SVCB")
      (if-let [service-binding (parsed-service-binding data)]
        (-> common
            (dissoc :content)
            (assoc :data service-binding))
        (invalid-record!))

      common)))

(defn- canonical-service-binding-content [content]
  (str/replace content #"=\"([^\"\\\s]*)\"" "=$1"))

(defn- response-data [record type]
  (let [content (:content record)]
    (case type
      "TXT"
      (if (string? content)
        (decoded-txt content)
        (invalid-response!))

      ("CNAME" "NS")
      (if (string? content)
        (trailing-dot content)
        (invalid-response!))

      "MX"
      (if (and (string? content) (number? (:priority record)))
        (str (:priority record) " " (trailing-dot content))
        (invalid-response!))

      "SRV"
      (let [{:keys [priority weight port target]} (:data record)]
        (if (and (every? number? [priority weight port])
                 (string? target))
          (str priority " " weight " " port " " (trailing-dot target))
          (invalid-response!)))

      "CAA"
      (cond
        (string? content) content
        (map? (:data record))
        (let [{:keys [flags tag value]} (:data record)]
          (if (and (number? flags) (string? tag) (string? value))
            (str flags " " tag " \"" value "\"")
            (invalid-response!)))
        :else (invalid-response!))

      ("HTTPS" "SVCB")
      (if (string? content)
        (canonical-service-binding-content content)
        (invalid-response!))

      (if (string? content)
        content
        (invalid-response!)))))

(defn- portable-record [record zone]
  (let [{:keys [name ttl type]} record]
    (when-not (and (string? name)
                   (integer? ttl)
                   (not (neg? ttl))
                   (string? type)
                   (seq type))
      (invalid-response!))
    (let [type (canonical-type type)]
      {:name (relative-name name zone)
       :ttl  ttl
       :type type
       :data (response-data record type)})))

(defn- read-token [provider]
  (if (seq (:zone-token provider))
    (:zone-token provider)
    (:api-token provider)))

(defn- more-pages? [response page records]
  (let [{:keys [total_pages total_count per_page]} (:result_info response)]
    (and (seq records)
         (cond
           (pos-int? total_pages) (< page total_pages)
           (and (pos-int? total_count) (pos-int? per_page))
           (< page (quot (+ total_count per_page -1) per_page))
           :else false))))

(defn- paged-results!
  [provider operation-deadline {:keys [token path query]}]
  (loop [page 1
         all  []]
    (let [response (request!
                    provider
                    operation-deadline
                    {:token  token
                     :method :get
                     :path   path
                     :query  (assoc query
                                    :page page
                                    :per_page max-page-size)})
          records  (:result response)]
      (when-not (vector? records)
        (invalid-response!))
      (let [all (into all records)]
        (ensure-time! operation-deadline)
        (if (more-pages? response page records)
          (recur (inc page) all)
          all)))))

(defn- zone-info! [provider operation-deadline zone]
  (let [response (request!
                  provider
                  operation-deadline
                  {:token  (read-token provider)
                   :method :get
                   :path   "/zones"
                   :query  {:name (normalized-zone zone)}})
        zones    (:result response)]
    (when-not (and (vector? zones) (= 1 (count zones)))
      (invalid-response!))
    (let [{:keys [id name] :as zone-info} (first zones)]
      (when-not (and (string? id) (seq id)
                     (string? name) (seq name))
        (invalid-response!))
      zone-info)))

(defn- list-zones* [provider operation-deadline]
  (mapv (fn [{:keys [name]}]
          (when-not (and (string? name) (seq name))
            (invalid-response!))
          {:name (trailing-dot name)})
        (paged-results! provider
                        operation-deadline
                        {:token (read-token provider)
                         :path  "/zones"
                         :query {}})))

(defn- get-records* [provider operation-deadline zone]
  (let [zone-id (:id (zone-info! provider operation-deadline zone))]
    (mapv #(portable-record % zone)
          (paged-results! provider
                          operation-deadline
                          {:token (:api-token provider)
                           :path  (str "/zones/" zone-id "/dns_records")
                           :query {}}))))

(defn- create-record!
  [provider operation-deadline zone zone-id payload mutated?]
  (vreset! mutated? true)
  (let [response (request!
                  provider
                  operation-deadline
                  {:token  (:api-token provider)
                   :method :post
                   :path   (str "/zones/" zone-id "/dns_records")
                   :body   payload})]
    (portable-record (:result response) zone)))

(defn- delete-record!
  [provider operation-deadline zone-id record-id mutated?]
  (when-not (and (string? record-id) (seq record-id))
    (invalid-response!))
  (vreset! mutated? true)
  (request! provider
            operation-deadline
            {:token  (:api-token provider)
             :method :delete
             :path   (str "/zones/" zone-id "/dns_records/" record-id)})
  nil)

(defn- matching-records!
  [provider operation-deadline zone zone-id name type]
  (let [query (cond-> {:name (absolute-name name zone)}
                type (assoc :type (canonical-type type)))]
    (paged-results! provider
                    operation-deadline
                    {:token (:api-token provider)
                     :path  (str "/zones/" zone-id "/dns_records")
                     :query query})))

(defn- append-records*
  [provider operation-deadline zone records mutated?]
  (let [payloads (mapv #(record-payload % zone) records)
        zone-id  (:id (zone-info! provider operation-deadline zone))]
    (mapv #(create-record! provider
                           operation-deadline
                           zone
                           zone-id
                           %
                           mutated?)
          payloads)))

(defn- rrset-key [record]
  [(.toLowerCase ^String (:name record) Locale/ROOT)
   (canonical-type (:type record))])

(defn- selected-rrsets [records]
  (mapv (fn [key]
          (filterv #(= key (rrset-key %)) records))
        (distinct (map rrset-key records))))

(defn- set-records*
  [provider operation-deadline zone records mutated?]
  (let [payloads-by-record (zipmap records
                                   (map #(record-payload % zone) records))
        zone-id            (:id (zone-info! provider operation-deadline zone))]
    (reduce
     (fn [set-records rrset]
       (let [[name type] (rrset-key (first rrset))
             existing    (matching-records! provider
                                            operation-deadline
                                            zone
                                            zone-id
                                            name
                                            type)]
         (doseq [record existing]
           (delete-record! provider
                           operation-deadline
                           zone-id
                           (:id record)
                           mutated?))
         (into set-records
               (map #(create-record! provider
                                     operation-deadline
                                     zone
                                     zone-id
                                     (payloads-by-record %)
                                     mutated?)
                    rrset))))
     []
     (selected-rrsets records))))

(defn- selector-match? [selector record]
  (and (or (not (contains? selector :type))
           (= (canonical-type (:type selector)) (:type record)))
       (or (not (contains? selector :ttl))
           (= (:ttl selector) (:ttl record)))
       (or (not (contains? selector :data))
           (= (:data selector) (:data record)))))

(defn- delete-records*
  [provider operation-deadline zone selectors mutated?]
  (let [zone-id (:id (zone-info! provider operation-deadline zone))]
    (reduce
     (fn [deleted selector]
       (let [candidates (matching-records! provider
                                           operation-deadline
                                           zone
                                           zone-id
                                           (:name selector)
                                           (:type selector))
             matches    (->> candidates
                             (map (fn [record]
                                    {:id     (:id record)
                                     :record (portable-record record zone)}))
                             (filter #(selector-match? selector (:record %))))]
         (doseq [{:keys [id]} matches]
           (delete-record! provider
                           operation-deadline
                           zone-id
                           id
                           mutated?))
         (into deleted (map :record matches))))
     []
     selectors)))

(defn- mutation-outcome [operation zone f]
  (let [mutated? (volatile! false)]
    (operation-outcome
     operation :records zone mutated?
     #(f mutated?))))

(defrecord Provider [api-token zone-token http-client]
  protocols/RecordGetter
  (-get-records! [this zone opts]
    (operation-outcome
     :get-records :records zone nil
     #(get-records* this (:deadline opts) zone)))

  protocols/RecordAppender
  (-append-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (let [operation-deadline (:deadline opts)]
        (mutation-outcome
         :append-records zone
         #(append-records* this operation-deadline zone records %)))))

  protocols/RecordSetter
  (-set-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (let [operation-deadline (:deadline opts)]
        (mutation-outcome
         :set-records zone
         #(set-records* this operation-deadline zone records %)))))

  protocols/RecordDeleter
  (-delete-records! [this zone selectors opts]
    (if (empty? selectors)
      {:ol.protocol53/result {:records []}}
      (let [operation-deadline (:deadline opts)]
        (mutation-outcome
         :delete-records zone
         #(delete-records* this operation-deadline zone selectors %)))))

  protocols/ZoneLister
  (-list-zones! [this opts]
    (operation-outcome
     :list-zones :zones nil nil
     #(list-zones* this (:deadline opts)))))

(defn provider
  "Returns a Cloudflare provider from `config`.

  Options:

  | key            | description
  | -------------- | -----------
  | `:api-token`   | Scoped token used for DNS record requests.
  | `:zone-token`  | Optional scoped token used for zone reads.
  | `:http-client` | Optional client accepted by `babashka.http-client`."
  [config]
  (map->Provider
   (assoc config :ol.protocol53/provider :cloudflare)))