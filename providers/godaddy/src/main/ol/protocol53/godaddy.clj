(ns ol.protocol53.godaddy
  "GoDaddy DNS provider for protocol53."
  (:require
   [babashka.json :as json]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.http :as http]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.io IOException]
   [java.time Duration]
   [java.util Locale]))

(def ^:private base-url "https://api.godaddy.com")
(def ^:private max-page-size 500)
(def ^:private minimum-ttl 600)

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
  (fail! :invalid-record "Invalid GoDaddy record data" false))

(defn- invalid-response! []
  (fail! :provider-response "GoDaddy returned an invalid response" false))

(defn- operation-outcome [operation result-key zone mutation? f]
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
                    :provider   :godaddy
                    :retryable? retryable}
             zone (assoc :zone zone)
             mutation? (assoc :zone-state (or zone-state :unchanged)))})))))

(defn- zone-unknown [cause]
  (if (::failure (ex-data cause))
    (ex-info (ex-message cause)
             (assoc (ex-data cause) ::zone-state :unknown)
             cause)
    cause))

(defn- timeout-millis [operation-deadline]
  (let [^Duration remaining (deadline/remaining operation-deadline)]
    (quot (.toNanos remaining) 1000000)))

(defn- ensure-time! [operation-deadline]
  (when (deadline/expired? operation-deadline)
    (fail! :deadline-exceeded
           "Deadline exceeded during GoDaddy request"
           false)))

(defn- normalized-zone [zone]
  (-> zone
      (str/replace #"\.+$" "")
      (.toLowerCase Locale/ROOT)))

(defn- records-path [zone]
  (str "/v1/domains/" (normalized-zone zone) "/records"))

(defn- rrset-path [zone type name]
  (str (records-path zone) "/" type "/" name))

(defn- request!
  [provider operation-deadline {:keys [body method mutation? path query]}]
  (let [timeout (timeout-millis operation-deadline)]
    (when-not (pos? timeout)
      (fail! :deadline-exceeded
             "Deadline exceeded during GoDaddy request"
             false))
    (let [headers (cond-> {"Accept"        "application/json"
                           "Authorization" (str "sso-key "
                                                (:api-key provider)
                                                ":"
                                                (:api-secret provider))}
                    (:shopper-id provider) (assoc "X-Shopper-Id"
                                                  (:shopper-id provider))
                    (some? body) (assoc "Content-Type" "application/json"))
          request (cond-> {:uri     (str base-url path)
                           :method  method
                           :headers headers
                           :timeout timeout
                           :throw   false}
                    (seq query) (assoc :query-params query)
                    (some? body) (assoc :body (json/write-str body))
                    (:http-client provider) (assoc :client (:http-client provider)))]
      (try
        (let [response (try
                         (http/request request)
                         (catch InterruptedException cause
                           (.interrupt (Thread/currentThread))
                           (fail! :provider-request
                                  "GoDaddy request failed"
                                  true
                                  cause))
                         (catch IOException cause
                           (fail! :provider-request
                                  "GoDaddy request failed"
                                  true
                                  cause))
                         (catch Exception cause
                           (fail! :provider-request
                                  "GoDaddy request failed"
                                  false
                                  cause)))]
          (ensure-time! operation-deadline)
          (when-not (and (map? response)
                         (integer? (:status response))
                         (<= 100 (:status response) 599))
            (invalid-response!))
          response)
        (catch clojure.lang.ExceptionInfo cause
          (throw (if mutation?
                   (zone-unknown cause)
                   cause)))))))

(defn- request-failure! [status]
  (fail! :provider-request
         (str "GoDaddy request failed with HTTP " status)
         (or (= status 429) (>= status 500))))

(defn- parsed-page [response]
  (try
    (let [body (json/read-str (:body response))]
      (when-not (sequential? body)
        (invalid-response!))
      (vec body))
    (catch Exception cause
      (if (::failure (ex-data cause))
        (throw cause)
        (fail! :provider-response
               "GoDaddy returned malformed JSON"
               false
               cause)))))

(defn- relative-name [name zone]
  (let [name   (-> name
                   (str/replace #"\.+$" "")
                   (.toLowerCase Locale/ROOT))
        zone   (normalized-zone zone)
        suffix (str "." zone)]
    (cond
      (or (empty? name) (= name "@") (= name zone)) "@"
      (str/ends-with? name suffix) (subs name 0 (- (count name) (count suffix)))
      :else name)))

(defn- canonical-type [type]
  (.toUpperCase ^String type Locale/ROOT))

(defn- portable-target [target]
  (when-not (and (string? target) (seq target))
    (invalid-response!))
  (if (or (= target ".") (str/ends-with? target "."))
    target
    (str target ".")))

(defn- checked-unsigned [value]
  (when-not (and (integer? value) (<= 0 value 65535))
    (invalid-response!))
  value)

(defn- srv-label [value]
  (when-not (and (string? value) (not (str/blank? value)))
    (invalid-response!))
  (let [label (if (str/starts-with? value "_") (subs value 1) value)]
    (when (empty? label)
      (invalid-response!))
    (str "_" (.toLowerCase ^String label Locale/ROOT))))

(defn- portable-name [record zone]
  (let [name (relative-name (:name record) zone)]
    (if (= "SRV" (:type record))
      (let [prefix (str (srv-label (:service record))
                        "."
                        (srv-label (:protocol record)))]
        (cond
          (= name "@") prefix
          (or (= name prefix)
              (str/starts-with? name (str prefix "."))) name
          :else (str prefix "." name)))
      name)))

(defn- portable-data [record]
  (let [{:keys [data port priority type weight]} record]
    (case type
      ("CNAME" "NS")
      (portable-target data)

      "MX"
      (str (checked-unsigned priority) " " (portable-target data))

      "SRV"
      (let [port (checked-unsigned port)]
        (when (zero? port)
          (invalid-response!))
        (str (checked-unsigned priority)
             " "
             (checked-unsigned weight)
             " "
             port
             " "
             (portable-target data)))

      data)))

(defn- portable-record [record zone]
  (let [{:keys [data name ttl type]} record]
    (when-not (and (map? record)
                   (string? name)
                   (not (str/blank? name))
                   (nat-int? ttl)
                   (string? type)
                   (not (str/blank? type))
                   (string? data))
      (invalid-response!))
    (let [record (assoc record :type (canonical-type type))]
      {:name (portable-name record zone)
       :ttl  ttl
       :type (:type record)
       :data (portable-data record)})))

(defn- get-records-at-path* [provider operation-deadline zone path]
  (loop [offset  0
         records []]
    (let [response (request! provider
                             operation-deadline
                             {:method :get
                              :path   path
                              :query  {:offset offset
                                       :limit  max-page-size}})
          status   (:status response)]
      (cond
        (= status 200)
        (let [page    (mapv #(portable-record % zone) (parsed-page response))
              records (into records page)]
          (if (< (count page) max-page-size)
            records
            (recur (+ offset (count page)) records)))

        (and (= status 422) (pos? offset))
        records

        :else
        (request-failure! status)))))

(defn- get-records* [provider operation-deadline zone]
  (get-records-at-path* provider
                        operation-deadline
                        zone
                        (records-path zone)))

(defn- strip-trailing-dot [target]
  (when-not (and (string? target) (seq target))
    (invalid-record!))
  (if (or (= target ".") (not (str/ends-with? target ".")))
    target
    (subs target 0 (dec (count target)))))

(defn- parsed-unsigned [value maximum]
  (try
    (let [value (Long/parseLong value)]
      (when-not (<= 0 value maximum)
        (invalid-record!))
      value)
    (catch NumberFormatException _
      (invalid-record!))))

(defn- mx-fields [data]
  (if-let [[_ priority target] (re-matches #"^(\d+)\s+(\S+)$" data)]
    {:data     (strip-trailing-dot target)
     :priority (parsed-unsigned priority 65535)}
    (invalid-record!)))

(defn- srv-fields [name data]
  (if-let [[_ service protocol subname]
           (re-matches #"^_([^.\s]+)\._([^.\s]+)(?:\.(.+))?$" name)]
    (if-let [[_ priority weight port target]
             (re-matches #"^(\d+)\s+(\d+)\s+(\d+)\s+(\S+)$" data)]
      (let [port (parsed-unsigned port 65535)]
        (when (zero? port)
          (invalid-record!))
        {:data     (strip-trailing-dot target)
         :name     (or subname "@")
         :port     port
         :priority (parsed-unsigned priority 65535)
         :protocol (str "_" protocol)
         :service  (str "_" service)
         :weight   (parsed-unsigned weight 65535)})
      (invalid-record!))
    (invalid-record!)))

(defn- record-payload [record zone]
  (let [{:keys [data name ttl type]} record
        name (relative-name name zone)
        type (canonical-type type)
        common {:data data
                :name name
                :ttl  (max minimum-ttl ttl)
                :type type}]
    (when (or (str/blank? name) (str/blank? type))
      (invalid-record!))
    (case type
      ("CNAME" "NS")
      (assoc common :data (strip-trailing-dot data))

      "MX"
      (merge common (mx-fields data))

      "SRV"
      (merge common (srv-fields name data))

      common)))

(defn- rrset-key [record]
  [(.toLowerCase ^String (:name record) Locale/ROOT)
   (canonical-type (:type record))])

(defn- records-minus [records previous]
  (first
   (reduce (fn [[added counts] record]
             (if (pos? (get counts record 0))
               [added (update counts record dec)]
               [(conj added record) counts]))
           [[] (frequencies previous)]
           records)))

(defn- missing-additions [payloads desired existing]
  (first
   (reduce (fn [[additions counts] [payload record]]
             (if (pos? (get counts record 0))
               [additions (update counts record dec)]
               [(conj additions payload) counts]))
           [[] (frequencies existing)]
           (map vector payloads desired))))

(defn- records-in-rrsets [records desired]
  (let [selected (set (map rrset-key desired))]
    (filterv #(contains? selected (rrset-key %)) records)))

(defn- checked-mutation-response! [response]
  (when-not (#{200 204} (:status response))
    (request-failure! (:status response)))
  (let [body (:body response)]
    (when-not (or (nil? body)
                  (and (string? body) (str/blank? body)))
      (try
        (json/read-str body)
        (catch Exception cause
          (fail! :provider-response
                 "GoDaddy returned malformed JSON"
                 false
                 cause)))))
  nil)

(defn- mutation-request! [provider operation-deadline request]
  (let [response (request! provider
                           operation-deadline
                           (assoc request :mutation? true))]
    (try
      (checked-mutation-response! response)
      (catch clojure.lang.ExceptionInfo cause
        (throw (zone-unknown cause))))))

(defn- append-records* [provider operation-deadline zone records]
  (let [payloads  (mapv #(record-payload % zone) records)
        desired   (mapv #(portable-record % zone) payloads)
        existing  (get-records* provider operation-deadline zone)
        additions (missing-additions payloads desired existing)]
    (if (empty? additions)
      []
      (do
        (mutation-request! provider
                           operation-deadline
                           {:body   additions
                            :method :patch
                            :path   (records-path zone)})
        (try
          (let [stored (get-records* provider operation-deadline zone)]
            (records-minus (records-in-rrsets stored desired)
                           (records-in-rrsets existing desired)))
          (catch clojure.lang.ExceptionInfo cause
            (throw (zone-unknown cause))))))))

(defn- set-plan [records zone]
  (let [entries (mapv (fn [record]
                        (let [payload (record-payload record zone)
                              desired (portable-record payload zone)]
                          {:key     (rrset-key desired)
                           :payload payload
                           :record  desired
                           :scope   [(:name payload) (:type payload)]}))
                      records)
        scopes  (distinct (map :scope entries))]
    (mapv (fn [scope]
            (let [group   (filterv #(= scope (:scope %)) entries)
                  payload (:payload (first group))]
              {:body         (mapv #(dissoc (:payload %) :name :type) group)
               :desired      (mapv :record group)
               :desired-keys (set (map :key group))
               :name         (:name payload)
               :type         (:type payload)}))
          scopes)))

(defn- stored-record-payload [record zone]
  (try
    (record-payload record zone)
    (catch clojure.lang.ExceptionInfo cause
      (if (= :invalid-record (::type (ex-data cause)))
        (invalid-response!)
        (throw cause)))))

(defn- complete-set-plan! [provider operation-deadline zone plan]
  (mapv
   (fn [{:keys [desired desired-keys name type] :as action}]
     (if-not (= "SRV" type)
       (assoc action :expected desired)
       (let [existing (get-records-at-path*
                       provider
                       operation-deadline
                       zone
                       (rrset-path zone type name))
             entries  (mapv (fn [record]
                              {:payload (stored-record-payload record zone)
                               :record  record})
                            existing)]
         (when-not (every? #(= [name type]
                               [(:name (:payload %)) (:type (:payload %))])
                           entries)
           (invalid-response!))
         (let [preserved (filterv #(not (contains? desired-keys
                                                   (rrset-key (:record %))))
                                  entries)]
           (-> action
               (update :body into
                       (map #(dissoc (:payload %) :name :type) preserved))
               (assoc :expected
                      (into desired (map :record preserved))))))))
   plan))

(defn- replace-rrsets! [provider operation-deadline zone plan]
  (loop [remaining         plan
         write-dispatched? false]
    (when-let [{:keys [body name type]} (first remaining)]
      (try
        (mutation-request! provider
                           operation-deadline
                           {:body   body
                            :method :put
                            :path   (rrset-path zone type name)})
        (catch clojure.lang.ExceptionInfo cause
          (throw (if write-dispatched?
                   (zone-unknown cause)
                   cause))))
      (recur (subvec remaining 1) true))))

(defn- set-records* [provider operation-deadline zone records]
  (let [plan (complete-set-plan! provider
                                 operation-deadline
                                 zone
                                 (set-plan records zone))]
    (replace-rrsets! provider operation-deadline zone plan)
    (try
      (into []
            (mapcat (fn [{:keys [desired-keys expected name type]}]
                      (let [stored (get-records-at-path*
                                    provider
                                    operation-deadline
                                    zone
                                    (rrset-path zone type name))]
                        (when-not (= (frequencies (map #(dissoc % :ttl) expected))
                                     (frequencies (map #(dissoc % :ttl) stored)))
                          (invalid-response!))
                        (filterv #(contains? desired-keys (rrset-key %))
                                 stored))))
            plan)
      (catch clojure.lang.ExceptionInfo cause
        (throw (zone-unknown cause))))))

(defn- mutation-outcome [operation zone f]
  (operation-outcome operation :records zone true f))

(defrecord Provider [api-key api-secret shopper-id http-client]
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
       #(set-records* this (:deadline opts) zone records)))))

(def ^:private ^String redacted-provider
  (str "#ol.protocol53.godaddy.Provider"
       "{:api-key \"<redacted>\", "
       ":api-secret \"<redacted>\", "
       ":shopper-id \"<redacted>\"}"))

(defmethod print-method Provider [_ ^java.io.Writer writer]
  (.write writer redacted-provider))

(defmethod print-dup Provider [_ ^java.io.Writer writer]
  (.write writer redacted-provider))

(defmethod pprint/simple-dispatch Provider [_]
  (.write ^java.io.Writer *out* redacted-provider))

(defn provider
  "Returns a GoDaddy provider from `config`.

  Options:

  | key            | description                                                     |
  |----------------|-----------------------------------------------------------------|
  | `:api-key`     | GoDaddy API key.                                                |
  | `:api-secret`  | GoDaddy API secret paired with `:api-key`.                      |
  | `:shopper-id`  | Optional reseller shopper ID sent as `X-Shopper-Id`.            |
  | `:http-client` | Optional [[java.net.http.HttpClient]] for advanced HTTP policy. |"
  [config]
  (map->Provider
   (assoc config :ol.protocol53/provider :godaddy)))
