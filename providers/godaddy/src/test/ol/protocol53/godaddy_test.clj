(ns ol.protocol53.godaddy-test
  (:require
   [babashka.json :as json]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53 :as p53]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.godaddy :as godaddy]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.net URI URLDecoder]
   [java.nio.charset StandardCharsets]
   [java.time Duration]))

(defn- opts
  ([]
   (opts 5))
  ([seconds]
   {:timeout (Duration/ofSeconds seconds)}))

(defn- query-map [^URI uri]
  (if-let [query (.getRawQuery uri)]
    (->> (str/split query #"&")
         (map #(str/split % #"=" 2))
         (map (fn [[k v]]
                [(URLDecoder/decode (str k) StandardCharsets/UTF_8)
                 (URLDecoder/decode (str (or v "")) StandardCharsets/UTF_8)]))
         (into {}))
    {}))

(defn- request-view [request]
  (let [^URI uri (:uri request)]
    {:method            (:method request)
     :path              (.getPath uri)
     :query             (query-map uri)
     :accept            (get-in request [:headers "Accept"])
     :authorization     (get-in request [:headers "Authorization"])
     :shopper-id        (get-in request [:headers "X-Shopper-Id"])
     :timeout-positive? (pos? (:timeout request))}))

(defn- response
  ([body]
   (response 200 body))
  ([status body]
   {:status  status
    :headers {}
    :body    (if (string? body) body (json/write-str body))}))

(defn- scripted-client [steps calls]
  (let [remaining (atom steps)]
    (fn [request]
      (swap! calls conj (request-view request))
      (let [step (first @remaining)]
        (swap! remaining #(vec (rest %)))
        (when-not step
          (throw (IllegalStateException. "Unexpected GoDaddy request")))
        (if-let [failure (:throw step)]
          (throw failure)
          step)))))

(defn- api-record
  ([name ttl type data]
   (api-record name ttl type data {}))
  ([name ttl type data fields]
   (merge {:name name :ttl ttl :type type :data data} fields)))

(def credentials
  {:api-key    "api-key"
   :api-secret "api-secret"})

(defn- provider
  ([client]
   (provider client {}))
  ([client config]
   (godaddy/provider
    (merge credentials config {:http-client client}))))

(specification "The GoDaddy provider"
  (behavior "gets and normalizes portable Domains API v1 records"
    (let [calls   (atom [])
          records [(api-record "Example.COM." 600 "a" "192.0.2.1")
                   (api-record "V6" 600 "AAAA" "2001:db8::1")
                   (api-record "WWW.Example.COM." 600 "CNAME" "target.example.net")
                   (api-record "txt" 600 "TXT" "")
                   (api-record "caa" 600 "CAA" "0 issue \"letsencrypt.org\"")
                   (api-record "@" 900 "MX" "mail.example.net" {:priority 10})
                   (api-record "delegated" 600 "NS" "ns1.example.net")
                   (api-record "VoIP" 600 "SRV" "service.example.net"
                               {:port     5060
                                :priority 20
                                :protocol "_TCP"
                                :service  "_SIP"
                                :weight   5})
                   (api-record "@" 600 "SOA" "ns1.example.net hostmaster.example.net 1 2 3 4 5")
                   (api-record "opaque" 600 "future" "provider-defined")]
          client  (scripted-client [(response records)] calls)
          result  (p53/get-records!
                   (provider client {:shopper-id "shopper-id"})
                   "Example.COM."
                   (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "@" :ttl 600 :type "A" :data "192.0.2.1"}
                       {:name "v6" :ttl 600 :type "AAAA" :data "2001:db8::1"}
                       {:name "www" :ttl 600 :type "CNAME" :data "target.example.net."}
                       {:name "txt" :ttl 600 :type "TXT" :data ""}
                       {:name "caa" :ttl 600 :type "CAA"
                        :data "0 issue \"letsencrypt.org\""}
                       {:name "@" :ttl 900 :type "MX" :data "10 mail.example.net."}
                       {:name "delegated" :ttl 600 :type "NS" :data "ns1.example.net."}
                       {:name "_sip._tcp.voip"                 :ttl 600 :type "SRV"
                        :data "20 5 5060 service.example.net."}
                       {:name "@" :ttl 600 :type "SOA"
                        :data "ns1.example.net hostmaster.example.net 1 2 3 4 5"}
                       {:name "opaque" :ttl 600 :type "FUTURE" :data "provider-defined"}]}}
        @calls
        => [{:method            :get
             :path              "/v1/domains/example.com/records"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        "shopper-id"
             :timeout-positive? true}])))

  (behavior "omits reseller scope when no shopper ID is configured"
    (let [calls  (atom [])
          client (scripted-client [(response [])] calls)]
      (p53/get-records! (provider client) "example.com." (opts))
      (assertions
        (select-keys (first @calls) [:authorization :shopper-id])
        => {:authorization "sso-key api-key:api-secret"
            :shopper-id    nil})))

  (behavior "paginates by the true skipped-Record offset"
    (let [calls   (atom [])
          remote  (mapv #(api-record (str "record-" %) 600 "A" "192.0.2.1")
                        (range 1003))
          pages   (mapv #(response (vec %)) (partition-all 500 remote))
          client  (scripted-client pages calls)
          result  (p53/get-records! (provider client) "example.com." (opts))
          records (get-in result [:ol.protocol53/result :records])]
      (assertions
        {:count   (count records)
         :first   (first records)
         :last    (last records)
         :queries (mapv :query @calls)}
        => {:count   1003
            :first   {:name "record-0" :ttl 600 :type "A" :data "192.0.2.1"}
            :last    {:name "record-1002" :ttl 600 :type "A" :data "192.0.2.1"}
            :queries [{"offset" "0" "limit" "500"}
                      {"offset" "500" "limit" "500"}
                      {"offset" "1000" "limit" "500"}]})))

  (behavior "finishes exact-size pagination with an empty page or follow-up 422"
    (let [page (mapv #(api-record (str "record-" %) 600 "A" "192.0.2.1")
                     (range 500))
          run  (fn [last-response]
                 (let [calls  (atom [])
                       client (scripted-client [(response page) last-response] calls)
                       result (p53/get-records! (provider client) "example.com." (opts))]
                   {:count   (count (get-in result [:ol.protocol53/result :records]))
                    :queries (mapv :query @calls)}))]
      (assertions
        [(run (response []))
         (run (response 422 "pagination complete"))]
        => [{:count   500
             :queries [{"offset" "0" "limit" "500"}
                       {"offset" "500" "limit" "500"}]}
            {:count   500
             :queries [{"offset" "0" "limit" "500"}
                       {"offset" "500" "limit" "500"}]}])))

  (behavior "rejects 422 before any complete page"
    (let [calls  (atom [])
          client (scripted-client [(response 422 "invalid domain")] calls)]
      (assertions
        (p53/get-records! (provider client) "example.com." (opts))
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "GoDaddy request failed with HTTP 422"
             :operation  :get-records
             :provider   :godaddy
             :zone       "example.com."
             :retryable? false}}
        (count @calls) => 1)))

  (behavior "redacts configuration and advertises only RecordGetter"
    (let [sut     (godaddy/provider {:api-key    "visible-api-key"
                                     :api-secret "visible-api-secret"
                                     :shopper-id "visible-shopper-id"})
          outputs [(pr-str sut)
                   (binding [*print-dup* true] (pr-str sut))
                   (with-out-str (pprint/pprint sut))]]
      (assertions
        {:all-redacted?  (every? #(str/includes? % "<redacted>") outputs)
         :leaked?        (boolean
                          (some #(or (str/includes? % "visible-api-key")
                                     (str/includes? % "visible-api-secret")
                                     (str/includes? % "visible-shopper-id"))
                                outputs))
         :record-getter? (satisfies? protocols/RecordGetter sut)
         :zone-lister?   (satisfies? protocols/ZoneLister sut)
         :list-outcome   (p53/list-zones! sut (opts))}
        => {:all-redacted?  true
            :leaked?        false
            :record-getter? true
            :zone-lister?   false
            :list-outcome
            {:ol.protocol53/error
             {:type       :unsupported-operation
              :message    "Provider does not support list-zones"
              :operation  :list-zones
              :provider   :godaddy
              :retryable? false}}})))

  (behavior "rejects malformed JSON, page shapes, and Record fields"
    (let [outcome (fn [body]
                    (p53/get-records!
                     (provider (scripted-client [(response body)] (atom [])))
                     "example.com."
                     (opts)))
          invalid {:ol.protocol53/error
                   {:type       :provider-response
                    :message    "GoDaddy returned an invalid response"
                    :operation  :get-records
                    :provider   :godaddy
                    :zone       "example.com."
                    :retryable? false}}]
      (assertions
        [(outcome "not-json")
         (outcome {})
         (outcome [{:name "www" :type "A" :data "192.0.2.1"}])
         (outcome [(api-record "www" -1 "A" "192.0.2.1")])
         (outcome [(api-record "@" 600 "MX" "mail.example.net" {:priority "ten"})])
         (outcome [(api-record "@" 600 "MX" "" {:priority 10})])
         (outcome [(api-record "voip" 600 "SRV" "service.example.net"
                               {:port 5060 :priority 20 :service "_sip" :weight 5})])]
        => [{:ol.protocol53/error
             {:type       :provider-response
              :message    "GoDaddy returned malformed JSON"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? false}}
            invalid
            invalid
            invalid
            invalid
            invalid
            invalid])))

  (behavior "classifies failures without leaking credentials or response bodies"
    (let [api-key    "visible-api-key"
          api-secret "visible-api-secret"
          shopper-id "visible-shopper-id"
          config     {:api-key api-key :api-secret api-secret :shopper-id shopper-id}
          outcome    (fn [step]
                       (p53/get-records!
                        (provider (scripted-client [step] (atom [])) config)
                        "example.com."
                        (opts)))
          outcomes   [(outcome {:throw (java.io.IOException. api-secret)})
                      (outcome {:throw (Exception. api-key)})
                      (outcome (response 401 (str api-key api-secret shopper-id)))
                      (outcome (response 429 "slow down"))
                      (outcome (response 503 "unavailable"))]]
      (assertions
        outcomes
        => [{:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 401"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 429"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 503"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? true}}]
        (some #(or (str/includes? (pr-str %) api-key)
                   (str/includes? (pr-str %) api-secret)
                   (str/includes? (pr-str %) shopper-id))
              outcomes)
        => nil)))

  (behavior "honors sub-millisecond and post-response deadline boundaries"
    (let [before-calls (atom [])
          after-calls  (atom [])
          before       (with-redefs [deadline/remaining
                                     (constantly (Duration/ofNanos 999999))]
                         (p53/get-records!
                          (provider (scripted-client [(response [])] before-calls))
                          "example.com."
                          (opts)))
          checks       (atom 0)
          after        (with-redefs [deadline/expired?
                                     (fn [_]
                                       (> (swap! checks inc) 1))]
                         (p53/get-records!
                          (provider (scripted-client [(response [])] after-calls))
                          "example.com."
                          (opts)))]
      (assertions
        {:before       before
         :before-calls @before-calls
         :after        after
         :after-count  (count @after-calls)}
        => {:before
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? false}}
            :before-calls []
            :after
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :get-records
              :provider   :godaddy
              :zone       "example.com."
              :retryable? false}}
            :after-count  1}))))
