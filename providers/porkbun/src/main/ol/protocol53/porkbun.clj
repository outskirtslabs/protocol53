(ns ol.protocol53.porkbun
  "Porkbun DNS provider for protocol53."
  (:require
   [babashka.json :as json]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.http :as http]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.io IOException]
   [java.time Duration Instant]
   [java.util Locale]))

(def ^:private base-url "https://api.porkbun.com/api/json/v3")
(def ^:private minimum-ttl 600)
(def ^:private domain-page-size 1000)
(def ^:private max-mutation-attempts 3)
(def ^:private default-retry-seconds 1)
(def ^:private dns-access-denial-codes
  #{"DOMAIN_NOT_ALLOWED" "INVALID_DOMAIN"})

(defn- fail!
  ([type message retryable?]
   (fail! type message retryable? nil nil))
  ([type message retryable? cause]
   (fail! type message retryable? cause nil))
  ([type message retryable? cause details]
   (throw (ex-info message
                   (merge {::failure   true
                           ::type      type
                           ::retryable retryable?}
                          details)
                   cause))))

(defn- invalid-record! []
  (fail! :invalid-record "Invalid Porkbun record data" false))

(defn- invalid-response! []
  (fail! :provider-response "Porkbun returned an invalid response" false))

(defn- operation-outcome
  [operation result-key zone mutation? f]
  (try
    {:ol.protocol53/result {result-key (f)}}
    (catch clojure.lang.ExceptionInfo cause
      (if-not (::failure (ex-data cause))
        (throw cause)
        (let [{type       ::type
               retryable  ::retryable
               zone-state ::zone-state} (ex-data cause)]
          {:ol.protocol53/error
           (cond-> {:type       type
                    :message    (ex-message cause)
                    :operation  operation
                    :provider   :porkbun
                    :retryable? retryable}
             zone (assoc :zone zone)
             mutation? (assoc :zone-state (or zone-state :unchanged)))})))))

(defn- timeout-millis [operation-deadline]
  (let [^Duration remaining (deadline/remaining operation-deadline)]
    (quot (.toNanos remaining) 1000000)))

(defn- ensure-time! [operation-deadline]
  (when (deadline/expired? operation-deadline)
    (fail! :deadline-exceeded
           "Deadline exceeded during Porkbun request"
           false)))

(defn- parsed-body [response]
  (try
    (let [body (json/read-str (:body response))]
      (when-not (map? body)
        (invalid-response!))
      body)
    (catch Exception cause
      (if (::failure (ex-data cause))
        (throw cause)
        (fail! :provider-response
               "Porkbun returned malformed JSON"
               false
               cause)))))

(defn- credentials [provider]
  {:apikey       (:api-key provider)
   :secretapikey (:secret-key provider)})

(defn- header-value [response header-name]
  (some (fn [[header values]]
          (when (= (str/lower-case (if (keyword? header)
                                     (name header)
                                     (str header)))
                   (str/lower-case header-name))
            (if (sequential? values) (first values) values)))
        (:headers response)))

(defn- maybe-parsed-body [response]
  (try
    (let [body (json/read-str (:body response))]
      (when (map? body) body))
    (catch Exception _
      nil)))

(defn- nonnegative-seconds [value]
  (when (some? value)
    (try
      (let [seconds (Long/parseLong (str value))]
        (when-not (neg? seconds) seconds))
      (catch NumberFormatException _
        nil))))

(defn- epoch-reset-seconds [response]
  (when-let [reset (some-> (header-value response "X-RateLimit-Reset")
                           nonnegative-seconds)]
    (max 0 (- reset (.getEpochSecond (Instant/now))))))

(defn- retry-seconds [response body]
  (or (some-> (header-value response "Retry-After") nonnegative-seconds)
      (some-> (:ttlRemaining body) nonnegative-seconds)
      (epoch-reset-seconds response)
      default-retry-seconds))

(defn- wait-for-retry! [operation-deadline response body]
  (let [wait      (Duration/ofSeconds (retry-seconds response body))
        remaining (deadline/remaining operation-deadline)]
    (when-not (neg? (.compareTo ^Duration wait ^Duration remaining))
      (fail! :deadline-exceeded
             "Deadline exceeded while waiting to retry Porkbun request"
             false))
    (when-not (.isZero ^Duration wait)
      (try
        (Thread/sleep (.toMillis ^Duration wait))
        (catch InterruptedException cause
          (.interrupt (Thread/currentThread))
          (fail! :provider-request "Porkbun request failed" true cause))))))

(defn- mutation-path? [path]
  (or (str/starts-with? path "/dns/create/")
      (str/starts-with? path "/dns/edit/")
      (str/starts-with? path "/dns/delete/")))

(defn- zone-unknown [cause]
  (if (::failure (ex-data cause))
    (ex-info (ex-message cause)
             (assoc (ex-data cause) ::zone-state :unknown)
             cause)
    cause))

(defn- request!
  ([provider operation-deadline path body]
   (request! provider operation-deadline path body false))
  ([provider operation-deadline path body write?]
   (let [mutation? (mutation-path? path)
         headers   (cond-> {"Content-Type" "application/json"}
                     mutation? (assoc "Idempotency-Key" (str (random-uuid))))
         request   (cond-> {:uri     (str base-url path)
                            :method  :post
                            :headers headers
                            :body    (json/write-str
                                      (merge (credentials provider) body))
                            :throw   false}
                     (:http-client provider) (assoc :client
                                                    (:http-client provider)))]
     (loop [attempt           1
            write-dispatched? false]
       (let [timeout (timeout-millis operation-deadline)]
         (when-not (pos? timeout)
           (try
             (fail! :deadline-exceeded
                    "Deadline exceeded during Porkbun request"
                    false)
             (catch clojure.lang.ExceptionInfo cause
               (throw (if write-dispatched?
                        (zone-unknown cause)
                        cause)))))
         (let [result   (try
                          (try
                            {:response (http/request
                                        (assoc request :timeout timeout))}
                            (catch InterruptedException cause
                              (.interrupt (Thread/currentThread))
                              (fail! :provider-request
                                     "Porkbun request failed"
                                     true
                                     cause))
                            (catch IOException cause
                              {:transport-failure cause})
                            (catch Exception cause
                              (fail! :provider-request
                                     "Porkbun request failed"
                                     false
                                     cause)))
                          (catch clojure.lang.ExceptionInfo cause
                            (throw (if write?
                                     (zone-unknown cause)
                                     cause))))
               decision (try
                          (if-let [cause (:transport-failure result)]
                            (if (and mutation?
                                     (< attempt max-mutation-attempts))
                              {:retry [nil nil]}
                              (fail! :provider-request
                                     "Porkbun request failed"
                                     true
                                     cause))
                            (let [response   (:response result)
                                  status     (long (or (:status response) 0))
                                  error-body (when-not (<= 200 status 299)
                                               (maybe-parsed-body response))
                                  error-code (:code error-body)
                                  replay?    (and (= status 409)
                                                  (= "IDEMPOTENCY_KEY_IN_USE"
                                                     error-code))
                                  retryable? (or (= status 429)
                                                 (>= status 500)
                                                 replay?)]
                              (ensure-time! operation-deadline)
                              (cond
                                (and mutation?
                                     retryable?
                                     (< attempt max-mutation-attempts))
                                {:retry [response error-body]}

                                (not (<= 200 status 299))
                                (fail! :provider-request
                                       (str "Porkbun request failed with HTTP "
                                            status)
                                       retryable?
                                       nil
                                       {::code error-code})

                                :else
                                (let [response-body (parsed-body response)]
                                  (case (:status response-body)
                                    "SUCCESS" {:body response-body}
                                    "ERROR" (if (and mutation?
                                                     (< attempt
                                                        max-mutation-attempts)
                                                     (= "RATE_LIMIT_EXCEEDED"
                                                        (:code response-body)))
                                              {:retry [response response-body]}
                                              (fail! :provider-request
                                                     "Porkbun returned an unsuccessful response"
                                                     (= "RATE_LIMIT_EXCEEDED"
                                                        (:code response-body))
                                                     nil
                                                     {::code (:code response-body)}))
                                    (invalid-response!))))))
                          (catch clojure.lang.ExceptionInfo cause
                            (throw (if write?
                                     (zone-unknown cause)
                                     cause))))]
           (if-let [[response response-body] (:retry decision)]
             (do
               (try
                 (wait-for-retry! operation-deadline response response-body)
                 (catch clojure.lang.ExceptionInfo cause
                   (throw (if write?
                            (zone-unknown cause)
                            cause))))
               (recur (inc attempt) (or write-dispatched? write?)))
             (:body decision))))))))

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

(defn- strip-trailing-dot [target]
  (if (and (not= target ".")
           (str/ends-with? target "."))
    (subs target 0 (dec (count target)))
    target))

(defn- trailing-dot [target]
  (if (or (= target ".")
          (str/ends-with? target "."))
    target
    (str target ".")))

(defn- parsed-unsigned [value maximum invalid!]
  (try
    (let [number (Long/parseLong (str value))]
      (when-not (<= 0 number maximum)
        (invalid!))
      number)
    (catch NumberFormatException _
      (invalid!))))

(defn- canonical-service-binding [content]
  (str/replace content #"=\"([^\"\\\s]*)\"" "=$1"))

(defn- portable-data [type content priority invalid!]
  (case type
    ("CNAME" "NS")
    (trailing-dot content)

    "MX"
    (str (parsed-unsigned priority 65535 invalid!)
         " "
         (trailing-dot content))

    "SRV"
    (if-let [[_ weight port target]
             (re-matches #"^(\d+)\s+(\d+)\s+(.+)$" content)]
      (str (parsed-unsigned priority 65535 invalid!)
           " "
           (parsed-unsigned weight 65535 invalid!)
           " "
           (parsed-unsigned port 65535 invalid!)
           " "
           (trailing-dot target))
      (invalid!))

    ("HTTPS" "SVCB")
    (canonical-service-binding content)

    content))

(defn- priority-payload [data pattern content-fn]
  (if-let [parts (re-matches pattern data)]
    (let [priority (parsed-unsigned (second parts) 65535 invalid-record!)]
      {:content (content-fn (drop 2 parts))
       :prio    priority})
    (invalid-record!)))

(defn- record-payload [record zone]
  (let [{:keys [name ttl type data]} record
        type (canonical-type type)
        common {:content data
                :name    (record-subname name zone)
                :ttl     (max minimum-ttl ttl)
                :type    type}
        special (case type
                  ("CNAME" "NS")
                  {:content (strip-trailing-dot data)}

                  "MX"
                  (priority-payload data
                                    #"^(\d+)\s+(.+)$"
                                    #(strip-trailing-dot (first %)))

                  "SRV"
                  (priority-payload
                   data
                   #"^(\d+)\s+(\d+)\s+(\d+)\s+(.+)$"
                   (fn [[weight port target]]
                     (parsed-unsigned weight 65535 invalid-record!)
                     (parsed-unsigned port 65535 invalid-record!)
                     (str weight " " port " "
                          (strip-trailing-dot target))))

                  {})]
    (merge common special)))

(defn- normalized-record [record zone]
  (let [{:keys [content name prio ttl type]} (record-payload record zone)]
    {:name (portable-name name)
     :ttl  ttl
     :type type
     :data (portable-data type content prio invalid-record!)}))

(defn- checked-api-record [record]
  (let [{:keys [content id name prio ttl type]} record
        ttl (try
              (Long/parseLong (str ttl))
              (catch NumberFormatException _
                (invalid-response!)))]
    (when-not (and (map? record)
                   (string? id)
                   (re-matches #"\d+" id)
                   (string? name)
                   (seq name)
                   (string? type)
                   (seq type)
                   (string? content)
                   (not (neg? ttl))
                   (or (nil? prio) (string? prio) (integer? prio)))
      (invalid-response!))
    (assoc record :ttl ttl :type (canonical-type type))))

(defn- relative-name [name zone]
  (let [name   (-> name
                   (str/replace #"\.+$" "")
                   (.toLowerCase Locale/ROOT))
        zone   (normalized-zone zone)
        suffix (str "." zone)]
    (cond
      (= name zone) "@"
      (str/ends-with? name suffix)
      (subs name 0 (- (count name) (count suffix)))
      :else name)))

(defn- portable-record [record zone]
  (let [{:keys [content name prio ttl type]} (checked-api-record record)]
    {:name (relative-name name zone)
     :ttl  ttl
     :type type
     :data (portable-data type content prio invalid-response!)}))

(defn- api-record-entries! [provider operation-deadline zone]
  (let [response (request! provider
                           operation-deadline
                           (str "/dns/retrieve/" (normalized-zone zone))
                           {})
        records  (:records response)]
    (when-not (vector? records)
      (invalid-response!))
    (mapv (fn [record]
            (let [record (checked-api-record record)]
              {:id     (:id record)
               :record (portable-record record zone)}))
          records)))

(defn- dns-access?
  [provider operation-deadline domain]
  (try
    (api-record-entries! provider operation-deadline domain)
    true
    (catch clojure.lang.ExceptionInfo cause
      (let [{type ::type
             code ::code} (ex-data cause)]
        (if (and (::failure (ex-data cause))
                 (= :provider-request type)
                 (contains? dns-access-denial-codes code))
          false
          (throw cause))))))

(defn- list-zones* [provider operation-deadline]
  (loop [start 0
         zones []]
    (let [response (request! provider
                             operation-deadline
                             "/domain/listAll"
                             {:start start})
          domains  (:domains response)
          count    (:count response)]
      (when-not (and (vector? domains)
                     (nat-int? count)
                     (= count (clojure.core/count domains)))
        (invalid-response!))
      (let [page  (into []
                        (keep (fn [domain]
                                (let [name       (:domain domain)
                                      api-access (:apiAccess domain)]
                                  (when-not (and (map? domain)
                                                 (string? name)
                                                 (seq name)
                                                 (#{0 1} api-access))
                                    (invalid-response!))
                                  (when (dns-access? provider operation-deadline name)
                                    {:name (str (normalized-zone name) ".")})))
                              domains))
            zones (into zones page)]
        (if (= domain-page-size count)
          (recur (long (+ start count)) zones)
          zones)))))

(defn- get-records* [provider operation-deadline zone]
  (mapv :record
        (api-record-entries! provider operation-deadline zone)))

(defn- create-record!
  [provider operation-deadline zone payload]
  (request! provider
            operation-deadline
            (str "/dns/create/" (normalized-zone zone))
            payload
            true))

(defn- edit-record!
  [provider operation-deadline zone id payload]
  (when-not (and (string? id) (re-matches #"\d+" id))
    (invalid-response!))
  (request! provider
            operation-deadline
            (str "/dns/edit/" (normalized-zone zone) "/" id)
            payload
            true))

(defn- delete-record!
  [provider operation-deadline zone id]
  (when-not (and (string? id) (re-matches #"\d+" id))
    (invalid-response!))
  (request! provider
            operation-deadline
            (str "/dns/delete/" (normalized-zone zone) "/" id)
            {}
            true)
  nil)

(defn- record-identity [record]
  (select-keys record [:name :type :data]))

(defn- remove-at [values index]
  (into (subvec values 0 index)
        (subvec values (inc index))))

(defn- take-first [pred values]
  (when-let [index (first (keep-indexed #(when (pred %2) %1) values))]
    [(nth values index) (remove-at values index)]))

(defn- records-as-stored!
  [provider operation-deadline zone desired]
  (loop [desired   desired
         available (mapv :record
                         (api-record-entries! provider operation-deadline zone))
         result    []]
    (if-let [wanted (first desired)]
      (if-let [[actual remaining]
               (take-first #(= (record-identity wanted)
                               (record-identity %))
                           available)]
        (recur (subvec desired 1) remaining (conj result actual))
        (invalid-response!))
      result)))

(defn- rrset-key [record]
  [(.toLowerCase ^String (:name record) Locale/ROOT)
   (canonical-type (:type record))])

(defn- validate-record!
  [provider operation-deadline zone payload]
  (let [response (request! provider
                           operation-deadline
                           (str "/dns/create/" (normalized-zone zone))
                           (assoc payload :dryRun true))]
    (when-not (true? (:wouldSucceed response))
      (invalid-response!))))

(defn- unmatched-rrset [existing desired]
  (loop [existing  existing
         desired   desired
         unmatched []]
    (if-let [wanted (first desired)]
      (if-let [[_ remaining]
               (take-first #(= (:record wanted) (:record %)) existing)]
        (recur remaining (subvec desired 1) unmatched)
        (recur existing (subvec desired 1) (conj unmatched wanted)))
      {:existing existing :desired unmatched})))

(defn- rrset-actions [existing desired]
  (let [{unmatched-existing :existing
         unmatched-desired  :desired} (unmatched-rrset existing desired)
        pair-count                    (min (count unmatched-existing)
                                           (count unmatched-desired))]
    (vec
     (concat
      (map (fn [existing desired]
             {:action  :edit
              :id      (:id existing)
              :payload (:payload desired)})
           (take pair-count unmatched-existing)
           (take pair-count unmatched-desired))
      (map (fn [{:keys [payload]}]
             {:action :create :payload payload})
           (drop pair-count unmatched-desired))
      (map (fn [{:keys [id]}]
             {:action :delete :id id})
           (drop pair-count unmatched-existing))))))

(defn- execute-action!
  [provider operation-deadline zone {:keys [action id payload]}]
  (case action
    :create (create-record! provider operation-deadline zone payload)
    :edit (edit-record! provider operation-deadline zone id payload)
    :delete (delete-record! provider operation-deadline zone id)))

(defn- execute-actions!
  [provider operation-deadline zone actions]
  (loop [remaining         actions
         write-dispatched? false]
    (if-let [action (first remaining)]
      (do
        (try
          (execute-action! provider operation-deadline zone action)
          (catch clojure.lang.ExceptionInfo cause
            (throw (if write-dispatched?
                     (zone-unknown cause)
                     cause))))
        (recur (subvec remaining 1) true))
      write-dispatched?)))

(defn- records-after-actions!
  [provider operation-deadline zone actions desired]
  (let [write-dispatched? (execute-actions! provider
                                            operation-deadline
                                            zone
                                            actions)]
    (try
      (records-as-stored! provider operation-deadline zone desired)
      (catch clojure.lang.ExceptionInfo cause
        (throw (if write-dispatched?
                 (zone-unknown cause)
                 cause))))))

(defn- append-records*
  [provider operation-deadline zone records]
  (let [payloads (mapv #(record-payload % zone) records)
        desired  (mapv #(normalized-record % zone) records)
        actions  (mapv #(hash-map :action :create :payload %) payloads)]
    (records-after-actions! provider
                            operation-deadline
                            zone
                            actions
                            desired)))

(defn- set-records*
  [provider operation-deadline zone records]
  (let [desired  (mapv (fn [record]
                         {:payload (record-payload record zone)
                          :record  (normalized-record record zone)})
                       records)
        existing (api-record-entries! provider operation-deadline zone)
        keys     (distinct (map (comp rrset-key :record) desired))]
    (doseq [{:keys [payload]} desired]
      (validate-record! provider operation-deadline zone payload))
    (let [actions (into []
                        (mapcat (fn [key]
                                  (rrset-actions
                                   (filterv #(= key (rrset-key (:record %)))
                                            existing)
                                   (filterv #(= key (rrset-key (:record %)))
                                            desired))))
                        keys)]
      (records-after-actions! provider
                              operation-deadline
                              zone
                              actions
                              (mapv :record desired)))))

(defn- normalized-selector [selector zone]
  (let [selector (cond-> {:name (portable-name
                                 (record-subname (:name selector) zone))}
                   (contains? selector :type)
                   (assoc :type (canonical-type (:type selector)))

                   (contains? selector :ttl)
                   (assoc :ttl (max minimum-ttl (:ttl selector)))

                   (contains? selector :data)
                   (assoc :data (:data selector)))]
    (if (and (contains? selector :type)
             (contains? selector :data))
      (let [normalized (normalized-record
                        {:name (:name selector)
                         :ttl  (get selector :ttl minimum-ttl)
                         :type (:type selector)
                         :data (:data selector)}
                        zone)]
        (assoc selector :data (:data normalized)))
      selector)))

(defn- selector-match? [selector record]
  (and (= (:name selector) (:name record))
       (or (not (contains? selector :type))
           (= (:type selector) (:type record)))
       (or (not (contains? selector :ttl))
           (= (:ttl selector) (:ttl record)))
       (or (not (contains? selector :data))
           (= (:data selector) (:data record)))))

(defn- delete-records*
  [provider operation-deadline zone selectors]
  (let [selectors (mapv #(normalized-selector % zone) selectors)
        matches   (filterv (fn [{:keys [record]}]
                             (some #(selector-match? % record) selectors))
                           (api-record-entries! provider
                                                operation-deadline
                                                zone))
        actions   (mapv (fn [{:keys [id]}]
                          {:action :delete :id id})
                        matches)]
    (execute-actions! provider operation-deadline zone actions)
    (mapv :record matches)))

(defn- mutation-outcome [operation zone f]
  (operation-outcome operation :records zone true f))

(defrecord Provider [api-key secret-key http-client]
  protocols/RecordGetter
  (-get-records! [this zone opts]
    (operation-outcome
     :get-records :records zone false
     #(get-records* this (:deadline opts) zone)))

  protocols/RecordAppender
  (-append-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :append-records zone
       #(append-records* this (:deadline opts) zone records))))

  protocols/RecordSetter
  (-set-records! [this zone records opts]
    (if (empty? records)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :set-records zone
       #(set-records* this (:deadline opts) zone records))))

  protocols/RecordDeleter
  (-delete-records! [this zone selectors opts]
    (if (empty? selectors)
      {:ol.protocol53/result {:records []}}
      (mutation-outcome
       :delete-records zone
       #(delete-records* this (:deadline opts) zone selectors))))

  protocols/ZoneLister
  (-list-zones! [this opts]
    (operation-outcome
     :list-zones :zones nil false
     #(list-zones* this (:deadline opts)))))

(def ^:private ^String redacted-provider
  "#ol.protocol53.porkbun.Provider{:api-key \"<redacted>\", :secret-key \"<redacted>\"}")

(defmethod print-method Provider [_ ^java.io.Writer writer]
  (.write writer redacted-provider))

(defmethod print-dup Provider [_ ^java.io.Writer writer]
  (.write writer redacted-provider))

(defmethod pprint/simple-dispatch Provider [_]
  (.write ^java.io.Writer *out* redacted-provider))

(defn provider
  "Returns a Porkbun provider from `config`.

  Options:

  | key            | description                                                     |
  |----------------|-----------------------------------------------------------------|
  | `:api-key`     | Porkbun API key.                                                |
  | `:secret-key`  | Porkbun secret API key.                                         |
  | `:http-client` | Optional [[java.net.http.HttpClient]] for advanced HTTP policy. |"
  [config]
  (map->Provider
   (assoc config :ol.protocol53/provider :porkbun)))
