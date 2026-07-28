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
  (fail! :provider-response "GoDaddy returned an invalid response" false))

(defn- operation-outcome [operation result-key zone f]
  (try
    {:ol.protocol53/result {result-key (f)}}
    (catch clojure.lang.ExceptionInfo cause
      (if-not (::failure (ex-data cause))
        (throw cause)
        (let [{type      ::type
               retryable ::retryable} (ex-data cause)]
          {:ol.protocol53/error
           {:type       type
            :message    (ex-message cause)
            :operation  operation
            :provider   :godaddy
            :zone       zone
            :retryable? retryable}})))))

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

(defn- request! [provider operation-deadline zone offset]
  (let [timeout (timeout-millis operation-deadline)]
    (when-not (pos? timeout)
      (fail! :deadline-exceeded
             "Deadline exceeded during GoDaddy request"
             false))
    (let [headers  (cond-> {"Accept"        "application/json"
                            "Authorization" (str "sso-key "
                                                 (:api-key provider)
                                                 ":"
                                                 (:api-secret provider))}
                     (:shopper-id provider) (assoc "X-Shopper-Id"
                                                   (:shopper-id provider)))
          request  (cond-> {:uri          (str base-url
                                               "/v1/domains/"
                                               (normalized-zone zone)
                                               "/records")
                            :method       :get
                            :headers      headers
                            :query-params {:offset offset
                                           :limit  max-page-size}
                            :timeout      timeout
                            :throw        false}
                     (:http-client provider) (assoc :client (:http-client provider)))
          response (try
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
      response)))

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

(defn- get-records* [provider operation-deadline zone]
  (loop [offset  0
         records []]
    (let [response (request! provider operation-deadline zone offset)
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

(defrecord Provider [api-key api-secret shopper-id http-client]
  protocols/RecordGetter
  (-get-records! [this zone opts]
    (operation-outcome
     :get-records :records zone
     #(get-records* this (:deadline opts) zone))))

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
