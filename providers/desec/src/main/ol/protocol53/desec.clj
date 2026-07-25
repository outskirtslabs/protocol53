(ns ol.protocol53.desec
  "deSEC DNS provider for protocol53.

  deSEC stores DNS data as complete RRsets. Mutations therefore use its atomic
  bulk RRset endpoint, and requested TTLs below 3600 seconds are stored as
  3600 seconds."
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.io ByteArrayOutputStream IOException]
   [java.nio.charset StandardCharsets]
   [java.time Duration]
   [java.util Locale]))

(def ^:private base-url "https://desec.io/api/v1")
(def ^:private minimum-ttl 3600)

(defn- fail!
  ([type message retryable?]
   (fail! type message retryable? nil))
  ([type message retryable? cause]
   (throw (ex-info message
                   {::failure   true
                    ::type      type
                    ::retryable retryable?}
                   cause))))

(defn- invalid-response! []
  (fail! :provider-response "deSEC returned an invalid response" false))

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
                    :provider   :desec
                    :retryable? retryable}
             zone (assoc :zone zone)
             mutated? (assoc :zone-state
                             (if @mutated? :unknown :unchanged)))})))))

(defn- timeout-millis [operation-deadline]
  (let [^Duration remaining (deadline/remaining operation-deadline)]
    (quot (.toNanos remaining) 1000000)))

(defn- ensure-time! [operation-deadline]
  (when (deadline/expired? operation-deadline)
    (fail! :deadline-exceeded
           "Deadline exceeded during deSEC request"
           false)))

(defn- header-value [response header-name]
  (some (fn [[k v]]
          (let [k (if (keyword? k) (name k) (str k))]
            (when (= (str/lower-case k) (str/lower-case header-name))
              (if (sequential? v) (first v) v))))
        (:headers response)))

(defn- retry-after-seconds [response]
  (let [value   (header-value response "Retry-After")
        seconds (try
                  (Long/parseLong (str value))
                  (catch NumberFormatException _
                    nil))]
    (when-not (and seconds (not (neg? seconds)))
      (fail! :provider-response
             "deSEC returned an invalid Retry-After header"
             false))
    seconds))

(defn- wait-for-retry! [operation-deadline seconds]
  (when (pos? seconds)
    (let [wait      (Duration/ofSeconds seconds)
          remaining (deadline/remaining operation-deadline)]
      (when (pos? (.compareTo ^Duration wait ^Duration remaining))
        (fail! :deadline-exceeded
               "Deadline exceeded while waiting for deSEC rate limit"
               false))
      (try
        (Thread/sleep (.toMillis ^Duration wait))
        (catch InterruptedException cause
          (.interrupt (Thread/currentThread))
          (fail! :provider-request "deSEC request failed" true cause))))))

(defn- parsed-body [response]
  (try
    (let [body (->> (json/read-str (:body response))
                    (walk/postwalk #(if (sequential? %) (vec %) %)))]
      (when (nil? body)
        (invalid-response!))
      body)
    (catch Exception cause
      (if (::failure (ex-data cause))
        (throw cause)
        (fail! :provider-response
               "deSEC returned malformed JSON"
               false
               cause)))))

(defn- request!
  [provider operation-deadline {:keys [method path uri query body]}]
  (loop []
    (let [headers  (cond-> {"Authorization" (str "Token " (:token provider))
                            "Accept"        "application/json; charset=utf-8"}
                     body (assoc "Content-Type"
                                 "application/json; charset=utf-8"))
          request  (cond-> {:uri     (or uri (str base-url path))
                            :method  method
                            :headers headers
                            :throw   false}
                     (seq query) (assoc :query-params query)
                     body (assoc :body (json/write-str body))
                     (:http-client provider) (assoc :client
                                                    (:http-client provider)))
          timeout  (timeout-millis operation-deadline)
          _        (when-not (pos? timeout)
                     (fail! :deadline-exceeded
                            "Deadline exceeded during deSEC request"
                            false))
          response (try
                     (http/request (assoc request :timeout timeout))
                     (catch InterruptedException cause
                       (.interrupt (Thread/currentThread))
                       (fail! :provider-request
                              "deSEC request failed"
                              true
                              cause))
                     (catch Exception cause
                       (fail! :provider-request
                              "deSEC request failed"
                              (instance? IOException cause)
                              cause)))
          status   (long (or (:status response) 0))]
      (ensure-time! operation-deadline)
      (cond
        (= status 429)
        (do
          (wait-for-retry! operation-deadline
                           (retry-after-seconds response))
          (recur))

        (not (<= 200 status 299))
        (fail! :provider-request
               (str "deSEC request failed with HTTP " status)
               (>= status 500))

        :else
        (assoc response :body (parsed-body response))))))

(defn- normalized-zone [zone]
  (-> zone
      (str/replace #"\.+$" "")
      (.toLowerCase Locale/ROOT)))

(defn- canonical-type [type]
  (.toUpperCase ^String type Locale/ROOT))

(defn- record-subname [name zone]
  (let [name   (-> name
                   (str/replace #"\.+$" "")
                   (.toLowerCase Locale/ROOT))
        zone   (normalized-zone zone)
        suffix (str "." zone)]
    (cond
      (= name "@") ""
      (= name zone) ""
      (str/ends-with? name suffix)
      (subs name 0 (- (count name) (count suffix)))
      :else name)))

(defn- portable-name [subname]
  (if (empty? subname) "@" subname))

(defn- byte-value [b]
  (bit-and (int b) 0xff))

(defn- encode-txt [text]
  (let [result (StringBuilder.)]
    (.append result \")
    (doseq [octet (map byte-value
                       (.getBytes ^String text StandardCharsets/UTF_8))]
      (cond
        (= octet 34) (.append result "\\\"")
        (= octet 92) (.append result "\\\\")
        (<= 32 octet 126) (.append result (char octet))
        :else (.append result (format "\\%03d" octet))))
    (.append result \")
    (str result)))

(defn- digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn- write-codepoint! [^ByteArrayOutputStream output codepoint]
  (let [bytes (.getBytes (String. (Character/toChars codepoint))
                         StandardCharsets/UTF_8)]
    (.write output bytes 0 (alength bytes))))

(defn- decode-txt [^String content]
  (let [length (count content)
        output (ByteArrayOutputStream.)]
    (loop [i          0
           quoted?    false
           saw-token? false]
      (cond
        (= i length)
        (if (and saw-token? (not quoted?))
          (String. (.toByteArray output) StandardCharsets/UTF_8)
          (invalid-response!))

        quoted?
        (let [c (.charAt content i)]
          (cond
            (= c \")
            (recur (inc i) false true)

            (= c \\)
            (let [next-index (inc i)]
              (when (= next-index length)
                (invalid-response!))
              (let [next-char (.charAt content next-index)]
                (if (digit? next-char)
                  (do
                    (when (>= (+ i 3) length)
                      (invalid-response!))
                    (let [second-digit (.charAt content (+ i 2))
                          third-digit  (.charAt content (+ i 3))]
                      (when-not (and (digit? second-digit)
                                     (digit? third-digit))
                        (invalid-response!))
                      (let [octet (+ (* 100 (- (int next-char) (int \0)))
                                     (* 10 (- (int second-digit) (int \0)))
                                     (- (int third-digit) (int \0)))]
                        (when (> octet 255)
                          (invalid-response!))
                        (.write output octet)
                        (recur (+ i 4) true saw-token?))))
                  (let [codepoint (.codePointAt content next-index)]
                    (write-codepoint! output codepoint)
                    (recur (+ next-index (Character/charCount codepoint))
                           true
                           saw-token?)))))

            :else
            (let [codepoint (.codePointAt content i)]
              (write-codepoint! output codepoint)
              (recur (+ i (Character/charCount codepoint))
                     true
                     saw-token?))))

        (Character/isWhitespace (.charAt content i))
        (recur (inc i) false saw-token?)

        (= \" (.charAt content i))
        (recur (inc i) true saw-token?)

        :else
        (invalid-response!)))))

(defn- response-data [type data]
  (case type
    "TXT" (decode-txt data)
    ("HTTPS" "SVCB") (str/replace data #"=\"([^\"\\\s]*)\"" "=$1")
    data))

(defn- checked-rrset [rrset]
  (let [{:keys [subname ttl type records]} rrset]
    (when-not (and (map? rrset)
                   (string? subname)
                   (integer? ttl)
                   (not (neg? ttl))
                   (string? type)
                   (seq type)
                   (vector? records)
                   (every? string? records))
      (invalid-response!))
    rrset))

(defn- rrset-key [rrset]
  [(.toLowerCase ^String (:subname rrset) Locale/ROOT)
   (canonical-type (:type rrset))])

(defn- portable-records [rrset]
  (let [{:keys [subname ttl type records]} (checked-rrset rrset)
        type (canonical-type type)
        name (portable-name (.toLowerCase ^String subname Locale/ROOT))]
    (mapv (fn [data]
            {:name name
             :ttl  ttl
             :type type
             :data (response-data type data)})
          records)))

(defn- normalized-record [record zone]
  (let [subname (record-subname (:name record) zone)]
    {:name    (portable-name subname)
     :subname subname
     :ttl     (max minimum-ttl (:ttl record))
     :type    (canonical-type (:type record))
     :data    (:data record)}))

(defn- record-groups [records zone]
  (let [records (mapv #(normalized-record % zone) records)
        keys    (distinct (map #(select-keys % [:subname :type]) records))]
    (mapv (fn [key]
            (filterv #(= key (select-keys % [:subname :type])) records))
          keys)))

(defn- encoded-data [type data]
  (if (= type "TXT") (encode-txt data) data))

(defn- group-payload [group]
  (let [{:keys [subname ttl type]} (first group)]
    {:subname subname
     :type    type
     :ttl     ttl
     :records (->> group
                   (map #(encoded-data type (:data %)))
                   distinct
                   vec)}))

(defn- rrset-payload [rrset records]
  (let [{:keys [subname ttl type]} (checked-rrset rrset)]
    {:subname (.toLowerCase ^String subname Locale/ROOT)
     :type    (canonical-type type)
     :ttl     ttl
     :records records}))

(defn- next-link [response]
  (let [header (header-value response "Link")]
    (some (fn [[_ url relation]]
            (when (= relation "next") url))
          (re-seq #"<([^>]+)>;\s*rel=\"([^\"]+)\""
                  (or header "")))))

(defn- paged-results! [provider operation-deadline path]
  (loop [uri  nil
         seen #{}
         all  []]
    (let [response (request!
                    provider
                    operation-deadline
                    (cond-> {:method :get}
                      uri (assoc :uri uri)
                      (nil? uri) (assoc :path path
                                        :query {:cursor ""})))
          items    (:body response)]
      (when-not (vector? items)
        (invalid-response!))
      (let [all  (into all items)
            next (next-link response)]
        (if-not next
          all
          (do
            (when-not (and (str/starts-with? next
                                             (str base-url "/domains/"))
                           (not (contains? seen next)))
              (invalid-response!))
            (recur next (conj seen next) all)))))))

(defn- rrsets-path [zone]
  (str "/domains/" (normalized-zone zone) "/rrsets/"))

(defn- list-rrsets! [provider operation-deadline zone]
  (mapv checked-rrset
        (paged-results! provider
                        operation-deadline
                        (rrsets-path zone))))

(defn- list-zones* [provider operation-deadline]
  (mapv (fn [domain]
          (let [name (:name domain)]
            (when-not (and (map? domain) (string? name) (seq name))
              (invalid-response!))
            {:name (str (str/replace name #"\.+$" "") ".")}))
        (paged-results! provider operation-deadline "/domains/")))

(defn- get-records* [provider operation-deadline zone]
  (into [] (mapcat portable-records)
        (list-rrsets! provider operation-deadline zone)))

(defn- put-rrsets!
  [provider operation-deadline zone payloads mutated?]
  (if (empty? payloads)
    []
    (do
      (vreset! mutated? true)
      (let [stored (:body (request! provider
                                    operation-deadline
                                    {:method :put
                                     :path   (rrsets-path zone)
                                     :body   payloads}))]
        (when-not (vector? stored)
          (invalid-response!))
        (mapv checked-rrset stored)))))

(defn- set-records*
  [provider operation-deadline zone records mutated?]
  (->> (record-groups records zone)
       (mapv group-payload)
       (#(put-rrsets! provider operation-deadline zone % mutated?))
       (mapcat portable-records)
       vec))

(defn- indexed-rrsets [rrsets]
  (let [indexed (into {} (map (juxt rrset-key identity)) rrsets)]
    (when-not (= (count rrsets) (count indexed))
      (invalid-response!))
    indexed))

(defn- missing-group-data [group existing]
  (let [existing-data (set (map :data (mapcat portable-records
                                              (if existing [existing] []))))]
    (->> group
         (map :data)
         distinct
         (remove existing-data)
         vec)))

(defn- appended-payload [group existing new-data]
  (let [{:keys [subname ttl type]} (first group)]
    {:subname subname
     :type    type
     :ttl     (if existing (:ttl existing) ttl)
     :records (into (if existing (:records existing) [])
                    (map #(encoded-data type %) new-data))}))

(defn- records-minus [records previous]
  (first
   (reduce (fn [[added counts] record]
             (if (pos? (get counts record 0))
               [added (update counts record dec)]
               [(conj added record) counts]))
           [[] (frequencies previous)]
           records)))

(defn- append-records*
  [provider operation-deadline zone records mutated?]
  (let [groups   (record-groups records zone)
        existing (indexed-rrsets
                  (list-rrsets! provider operation-deadline zone))
        changes  (into []
                       (keep (fn [group]
                               (let [current  (get existing
                                                   [(:subname (first group))
                                                    (:type (first group))])
                                     new-data (missing-group-data group current)]
                                 (when (seq new-data)
                                   {:before  current
                                    :payload (appended-payload group
                                                               current
                                                               new-data)}))))
                       groups)
        stored   (put-rrsets! provider
                              operation-deadline
                              zone
                              (mapv :payload changes)
                              mutated?)
        before   (into [] (mapcat portable-records)
                       (keep :before changes))]
    (records-minus (into [] (mapcat portable-records) stored)
                   before)))

(defn- normalized-selector [selector zone]
  (cond-> {:name (portable-name (record-subname (:name selector) zone))}
    (contains? selector :type) (assoc :type
                                      (canonical-type (:type selector)))
    (contains? selector :ttl) (assoc :ttl
                                     (max minimum-ttl (:ttl selector)))
    (contains? selector :data) (assoc :data (:data selector))))

(defn- selector-match? [selector record]
  (and (= (:name selector) (:name record))
       (or (not (contains? selector :type))
           (= (:type selector) (:type record)))
       (or (not (contains? selector :ttl))
           (= (:ttl selector) (:ttl record)))
       (or (not (contains? selector :data))
           (= (:data selector) (:data record)))))

(defn- delete-plan [rrsets selectors]
  (reduce
   (fn [{:keys [payloads deleted]} rrset]
     (let [records (portable-records rrset)
           pairs   (mapv vector (:records rrset) records)
           removed (filterv (fn [[_ record]]
                              (some #(selector-match? % record) selectors))
                            pairs)]
       (if (empty? removed)
         {:payloads payloads :deleted deleted}
         {:payloads (conj payloads
                          (rrset-payload rrset
                                         (mapv first (remove (set removed)
                                                             pairs))))
          :deleted  (into deleted (map second removed))})))
   {:payloads [] :deleted []}
   rrsets))

(defn- delete-records*
  [provider operation-deadline zone selectors mutated?]
  (let [{:keys [payloads deleted]}
        (delete-plan (list-rrsets! provider operation-deadline zone)
                     (mapv #(normalized-selector % zone) selectors))]
    (put-rrsets! provider operation-deadline zone payloads mutated?)
    deleted))

(defn- mutation-outcome [operation zone f]
  (let [mutated? (volatile! false)]
    (operation-outcome operation :records zone mutated?
                       #(f mutated?))))

(defrecord Provider [token http-client]
  protocols/RecordGetter
  (-get-records! [this zone opts]
    (operation-outcome
     :get-records :records zone nil
     #(get-records* this (:deadline opts) zone)))

  protocols/RecordAppender
  (-append-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :append-records zone
       #(append-records* this (:deadline opts) zone records %))))

  protocols/RecordSetter
  (-set-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :set-records zone
       #(set-records* this (:deadline opts) zone records %))))

  protocols/RecordDeleter
  (-delete-records! [this zone selectors opts]
    (if (empty? selectors)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :delete-records zone
       #(delete-records* this (:deadline opts) zone selectors %))))

  protocols/ZoneLister
  (-list-zones! [this opts]
    (operation-outcome
     :list-zones :zones nil nil
     #(list-zones* this (:deadline opts)))))

(defn provider
  "Returns a deSEC provider from `config`.

  Options:

  | key            | description
  | -------------- | -----------
  | `:token`        | deSEC API token used for domain and RRset requests.
  | `:http-client`  | Optional client accepted by `babashka.http-client`."
  [config]
  (map->Provider
   (assoc config :ol.protocol53/provider :desec)))
