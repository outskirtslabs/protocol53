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
  (let [^URI uri (:uri request)
        body?    (some? (:body request))
        body     (when body?
                   (let [parsed (json/read-str (:body request) {:key-fn keyword})]
                     (if (sequential? parsed) (vec parsed) parsed)))]
    (cond-> {:method            (:method request)
             :path              (.getPath uri)
             :query             (query-map uri)
             :accept            (get-in request [:headers "Accept"])
             :authorization     (get-in request [:headers "Authorization"])
             :shopper-id        (get-in request [:headers "X-Shopper-Id"])
             :timeout-positive? (pos? (:timeout request))}
      body? (assoc :body body
                   :content-type (get-in request [:headers "Content-Type"])))))

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

  (behavior "redacts configuration and advertises Record capabilities"
    (let [sut     (godaddy/provider {:api-key    "visible-api-key"
                                     :api-secret "visible-api-secret"
                                     :shopper-id "visible-shopper-id"})
          outputs [(pr-str sut)
                   (binding [*print-dup* true] (pr-str sut))
                   (with-out-str (pprint/pprint sut))]]
      (assertions
        {:all-redacted?    (every? #(str/includes? % "<redacted>") outputs)
         :leaked?          (boolean
                            (some #(or (str/includes? % "visible-api-key")
                                       (str/includes? % "visible-api-secret")
                                       (str/includes? % "visible-shopper-id"))
                                  outputs))
         :record-getter?   (satisfies? protocols/RecordGetter sut)
         :record-appender? (satisfies? protocols/RecordAppender sut)
         :record-setter?   (satisfies? protocols/RecordSetter sut)
         :record-deleter?  (satisfies? protocols/RecordDeleter sut)
         :zone-lister?     (satisfies? protocols/ZoneLister sut)
         :list-outcome     (p53/list-zones! sut (opts))}
        => {:all-redacted?    true
            :leaked?          false
            :record-getter?   true
            :record-appender? true
            :record-setter?   true
            :record-deleter?  true
            :zone-lister?     false
            :list-outcome
            {:ol.protocol53/error
             {:type       :unsupported-operation
              :message    "Provider does not support list-zones"
              :operation  :list-zones
              :provider   :godaddy
              :retryable? false}}})))

  (behavior "appends one preserving batch and returns the Stored Record multiset delta"
    (let [calls    (atom [])
          existing [(api-record "keep" 600 "TXT" "unrelated")
                    (api-record "www" 600 "A" "192.0.2.1")
                    (api-record "www" 600 "A" "192.0.2.9")]
          stored   (into existing
                         [(api-record "www" 600 "A" "192.0.2.2")
                          (api-record "www" 1200 "A" "192.0.2.3")])
          records  [{:name "www" :ttl 300 :type "A" :data "192.0.2.1"}
                    {:name "WWW.Example.COM." :ttl 300 :type "a" :data "192.0.2.2"}
                    {:name "www" :ttl 900 :type "A" :data "192.0.2.3"}]
          client   (scripted-client [(response existing)
                                     (response 204 "")
                                     (response stored)]
                                    calls)
          result   (p53/append-records!
                    (provider client {:shopper-id "shopper-id"})
                    "Example.COM."
                    records
                    (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www" :ttl 600 :type "A" :data "192.0.2.2"}
                       {:name "www" :ttl 1200 :type "A" :data "192.0.2.3"}]}}
        @calls
        => [{:method            :get
             :path              "/v1/domains/example.com/records"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        "shopper-id"
             :timeout-positive? true}
            {:method            :patch
             :path              "/v1/domains/example.com/records"
             :query             {}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        "shopper-id"
             :timeout-positive? true
             :body              [{:data "192.0.2.2"
                                  :name "www"
                                  :ttl  600
                                  :type "A"}
                                 {:data "192.0.2.3"
                                  :name "www"
                                  :ttl  900
                                  :type "A"}]
             :content-type      "application/json"}
            {:method            :get
             :path              "/v1/domains/example.com/records"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        "shopper-id"
             :timeout-positive? true}])))

  (behavior "converts structured Records in one successful batch"
    (let [calls  (atom [])
          stored [(api-record "@" 600 "MX" "mail.example.net" {:priority 10})
                  (api-record "voip" 600 "SRV" "service.example.net"
                              {:port     5060
                               :priority 20
                               :protocol "_tcp"
                               :service  "_sip"
                               :weight   5})]
          client (scripted-client [(response [])
                                   (response 200 {})
                                   (response stored)]
                                  calls)
          result (p53/append-records!
                  (provider client)
                  "example.com."
                  [{:name "@"                    :ttl 300 :type "MX"
                    :data "10 mail.example.net."}
                   {:name "_SIP._TCP.VoIP.Example.COM."    :ttl 300 :type "srv"
                    :data "20 5 5060 service.example.net."}]
                  (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "@"                    :ttl 600 :type "MX"
                        :data "10 mail.example.net."}
                       {:name "_sip._tcp.voip"                 :ttl 600 :type "SRV"
                        :data "20 5 5060 service.example.net."}]}}
        (second @calls)
        => {:method            :patch
            :path              "/v1/domains/example.com/records"
            :query             {}
            :accept            "application/json"
            :authorization     "sso-key api-key:api-secret"
            :shopper-id        nil
            :timeout-positive? true
            :body              [{:data     "mail.example.net"
                                 :name     "@"
                                 :priority 10
                                 :ttl      600
                                 :type     "MX"}
                                {:data     "service.example.net"
                                 :name     "voip"
                                 :port     5060
                                 :priority 20
                                 :protocol "_tcp"
                                 :service  "_sip"
                                 :ttl      600
                                 :type     "SRV"
                                 :weight   5}]
            :content-type      "application/json"})))

  (behavior "short-circuits empty and already-present appends"
    (let [calls     (atom [])
          record    {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          client    (scripted-client [(response [(api-record "www" 600 "A"
                                                             "192.0.2.1")])]
                                     calls)
          sut       (provider client)
          operation (opts)]
      (assertions
        [(p53/append-records! sut "example.com." [] operation)
         (p53/append-records! sut "example.com." [record] operation)]
        => [{:ol.protocol53/result {:records []}}
            {:ol.protocol53/result {:records []}}]
        (mapv :method @calls) => [:get])))

  (behavior "validates every structured append Record before reading"
    (let [calls   (atom [])
          outcome (fn [records]
                    (p53/append-records!
                     (provider (scripted-client [] calls))
                     "example.com."
                     records
                     (opts)))
          invalid {:ol.protocol53/error
                   {:type       :invalid-record
                    :message    "Invalid GoDaddy record data"
                    :operation  :append-records
                    :provider   :godaddy
                    :zone       "example.com."
                    :zone-state :unchanged
                    :retryable? false}}]
      (assertions
        [(outcome [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
                   {:name "mail" :ttl 600 :type "MX" :data "malformed"}])
         (outcome [{:name "voip" :ttl 600 :type "SRV"
                    :data "20 5 5060 service.example.net."}])
         (outcome [{:name " " :ttl 600 :type "A" :data "192.0.2.1"}])
         (outcome [{:name "www" :ttl 600 :type " " :data "192.0.2.1"}])]
        => [invalid invalid invalid invalid]
        @calls => [])))

  (behavior "classifies every Append uncertainty boundary"
    (let [record {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          run    (fn [steps]
                   (let [calls  (atom [])
                         result (p53/append-records!
                                 (provider (scripted-client steps calls))
                                 "example.com."
                                 [record]
                                 (opts))]
                     {:outcome result :methods (mapv :method @calls)}))
          runs   [(run [(response 503 "unavailable")])
                  (run [(response [])
                        {:throw (java.io.IOException. "offline")}])
                  (run [(response []) (response 503 "unavailable")])
                  (run [(response []) (response 200 "not-json")])
                  (run [(response []) (response 204 "")
                        (response 503 "unavailable")])]]
      (assertions
        (mapv :outcome runs)
        => [{:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 503"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 503"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-response
              :message    "GoDaddy returned malformed JSON"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 503"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? true}}]
        (mapv :methods runs)
        => [[:get] [:get :patch] [:get :patch]
            [:get :patch] [:get :patch :get]])))

  (behavior "distinguishes deadlines before and after Append dispatch"
    (let [record        {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          before-calls  (atom [])
          before-checks (atom 0)
          before        (with-redefs [deadline/expired? (constantly false)
                                      deadline/remaining
                                      (fn [_]
                                        (if (= 1 (swap! before-checks inc))
                                          (Duration/ofSeconds 5)
                                          (Duration/ofNanos 999999)))]
                          (p53/append-records!
                           (provider (scripted-client [(response [])] before-calls))
                           "example.com."
                           [record]
                           (opts)))
          after-calls   (atom [])
          after-checks  (atom 0)
          after         (with-redefs [deadline/expired?
                                      (fn [_]
                                        (= 3 (swap! after-checks inc)))]
                          (p53/append-records!
                           (provider
                            (scripted-client [(response []) (response 204 "")]
                                             after-calls))
                           "example.com."
                           [record]
                           (opts)))]
      (assertions
        {:before         before
         :before-methods (mapv :method @before-calls)
         :after          after
         :after-methods  (mapv :method @after-calls)}
        => {:before
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            :before-methods [:get]
            :after
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :append-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? false}}
            :after-methods  [:get :patch]})))

  (behavior "sets complete RRsets in first-appearance order and returns Stored Records"
    (let [calls        (atom [])
          stored-a     [(api-record "www" 600 "A" "192.0.2.1")
                        (api-record "www" 1200 "A" "192.0.2.2")]
          existing-srv [(api-record "voip" 1800 "SRV" "ldap.example.net"
                                    {:port     389
                                     :priority 0
                                     :protocol "_tcp"
                                     :service  "_ldap"
                                     :weight   0})]
          stored-srv   [(api-record "voip" 600 "srv" "sip.example.net"
                                    {:port     5060
                                     :priority 20
                                     :protocol "_TCP"
                                     :service  "_SIP"
                                     :weight   5})
                        (api-record "voip" 900 "SRV" "xmpp.example.net"
                                    {:port     5222
                                     :priority 10
                                     :protocol "_tcp"
                                     :service  "_xmpp"
                                     :weight   1})
                        (first existing-srv)]
          stored-mx    [(api-record "@" 1200 "MX" "mail.example.net"
                                    {:priority 10})]
          client       (scripted-client [(response existing-srv)
                                         (response 204 "")
                                         (response 200 {})
                                         (response 204 "")
                                         (response stored-a)
                                         (response stored-srv)
                                         (response stored-mx)]
                                        calls)
          result       (p53/set-records!
                        (provider client)
                        "Example.COM."
                        [{:name "WWW" :ttl 300 :type "a" :data "192.0.2.1"}
                         {:name "_SIP._TCP.VoIP"             :ttl 300 :type "srv"
                          :data "20 5 5060 sip.example.net."}
                         {:name "www.example.com." :ttl 900 :type "A"
                          :data "192.0.2.2"}
                         {:name "_XMPP._TCP.voip"             :ttl 900 :type "SRV"
                          :data "10 1 5222 xmpp.example.net."}
                         {:name "@"                    :ttl 1200 :type "MX"
                          :data "10 mail.example.net."}]
                        (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
                       {:name "www" :ttl 1200 :type "A" :data "192.0.2.2"}
                       {:name "_sip._tcp.voip"             :ttl 600 :type "SRV"
                        :data "20 5 5060 sip.example.net."}
                       {:name "_xmpp._tcp.voip"             :ttl 900 :type "SRV"
                        :data "10 1 5222 xmpp.example.net."}
                       {:name "@"                    :ttl 1200 :type "MX"
                        :data "10 mail.example.net."}]}}
        @calls
        => [{:method            :get
             :path              "/v1/domains/example.com/records/SRV/voip"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true}
            {:method            :put
             :path              "/v1/domains/example.com/records/A/www"
             :query             {}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true
             :body              [{:data "192.0.2.1" :ttl 600}
                                 {:data "192.0.2.2" :ttl 900}]
             :content-type      "application/json"}
            {:method            :put
             :path              "/v1/domains/example.com/records/SRV/voip"
             :query             {}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true
             :body              [{:data     "sip.example.net"
                                  :port     5060
                                  :priority 20
                                  :protocol "_tcp"
                                  :service  "_sip"
                                  :ttl      600
                                  :weight   5}
                                 {:data     "xmpp.example.net"
                                  :port     5222
                                  :priority 10
                                  :protocol "_tcp"
                                  :service  "_xmpp"
                                  :ttl      900
                                  :weight   1}
                                 {:data     "ldap.example.net"
                                  :port     389
                                  :priority 0
                                  :protocol "_tcp"
                                  :service  "_ldap"
                                  :ttl      1800
                                  :weight   0}]
             :content-type      "application/json"}
            {:method            :put
             :path              "/v1/domains/example.com/records/MX/@"
             :query             {}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true
             :body              [{:data     "mail.example.net"
                                  :priority 10
                                  :ttl      1200}]
             :content-type      "application/json"}
            {:method            :get
             :path              "/v1/domains/example.com/records/A/www"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true}
            {:method            :get
             :path              "/v1/domains/example.com/records/SRV/voip"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true}
            {:method            :get
             :path              "/v1/domains/example.com/records/MX/@"
             :query             {"offset" "0" "limit" "500"}
             :accept            "application/json"
             :authorization     "sso-key api-key:api-secret"
             :shopper-id        nil
             :timeout-positive? true}])))

  (behavior "keeps the Zone unchanged when SRV preservation reads fail"
    (let [calls  (atom [])
          result (p53/set-records!
                  (provider
                   (scripted-client [(response 503 "unavailable")] calls))
                  "example.com."
                  [{:name "_sip._tcp.voip"             :ttl 600 :type "SRV"
                    :data "20 5 5060 sip.example.net."}]
                  (opts))]
      (assertions
        {:outcome result :methods (mapv :method @calls)}
        => {:outcome
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "GoDaddy request failed with HTTP 503"
              :operation  :set-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? true}}
            :methods [:get]})))

  (behavior "short-circuits empty Sets and prevalidates every desired group"
    (let [calls   (atom [])
          sut     (provider (scripted-client [] calls))
          invalid {:ol.protocol53/error
                   {:type       :invalid-record
                    :message    "Invalid GoDaddy record data"
                    :operation  :set-records
                    :provider   :godaddy
                    :zone       "example.com."
                    :zone-state :unchanged
                    :retryable? false}}]
      (assertions
        [(p53/set-records! sut "example.com." [] (opts))
         (p53/set-records!
          sut
          "example.com."
          [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
           {:name "mail" :ttl 600 :type "MX" :data "malformed"}]
          (opts))
         (p53/set-records!
          sut
          "example.com."
          [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
           {:name "voip" :ttl 600 :type "SRV"
            :data "20 5 5060 service.example.net."}]
          (opts))]
        => [{:ol.protocol53/result {:records []}} invalid invalid]
        @calls => [])))

  (behavior "stops Set at the first uncertain failure"
    (let [records [{:name "one" :ttl 600 :type "A" :data "192.0.2.1"}
                   {:name "two" :ttl 600 :type "TXT" :data "two"}
                   {:name "@"                    :ttl 600 :type "MX"
                    :data "10 mail.example.net."}]
          run     (fn [steps]
                    (let [calls  (atom [])
                          result (p53/set-records!
                                  (provider (scripted-client steps calls))
                                  "example.com."
                                  records
                                  (opts))]
                      {:outcome result :methods (mapv :method @calls)}))
          runs    [(run [{:throw (java.io.IOException. "offline")}])
                   (run [(response 503 "unavailable")])
                   (run [(response 200 "not-json")])
                   (run [(response 204 "")
                         (response 503 "unavailable")])
                   (run [(response 204 "")
                         (response 204 "")
                         (response 204 "")
                         (response 503 "unavailable")])]]
      (assertions
        runs
        => [{:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed"
               :operation  :set-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:put]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :set-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:put]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-response
               :message    "GoDaddy returned malformed JSON"
               :operation  :set-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? false}}
             :methods [:put]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :set-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:put :put]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :set-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:put :put :put :get]}])))

  (behavior "rejects missing, extra, or malformed Stored RRset data"
    (let [record  {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          run     (fn [stored]
                    (let [calls  (atom [])
                          result (p53/set-records!
                                  (provider
                                   (scripted-client [(response 204 "")
                                                     (response stored)]
                                                    calls))
                                  "example.com."
                                  [record]
                                  (opts))]
                      {:outcome result :methods (mapv :method @calls)}))
          runs    [(run [])
                   (run [(api-record "www" 600 "A" "192.0.2.1")
                         (api-record "www" 600 "A" "192.0.2.2")])
                   (run [{:name "www" :type "A" :data "192.0.2.1"}])]
          invalid {:ol.protocol53/error
                   {:type       :provider-response
                    :message    "GoDaddy returned an invalid response"
                    :operation  :set-records
                    :provider   :godaddy
                    :zone       "example.com."
                    :zone-state :unknown
                    :retryable? false}}]
      (assertions
        runs
        => [{:outcome invalid :methods [:put :get]}
            {:outcome invalid :methods [:put :get]}
            {:outcome invalid :methods [:put :get]}])))

  (behavior "distinguishes Set deadlines before and after an earlier write"
    (let [records      [{:name "one" :ttl 600 :type "A" :data "192.0.2.1"}
                        {:name "two" :ttl 600 :type "TXT" :data "two"}]
          before-calls (atom [])
          before       (with-redefs [deadline/remaining
                                     (constantly (Duration/ofNanos 999999))]
                         (p53/set-records!
                          (provider (scripted-client [] before-calls))
                          "example.com."
                          records
                          (opts)))
          later-calls  (atom [])
          checks       (atom 0)
          later        (with-redefs [deadline/remaining
                                     (fn [_]
                                       (if (<= (swap! checks inc) 3)
                                         (Duration/ofSeconds 5)
                                         (Duration/ofNanos 999999)))]
                         (p53/set-records!
                          (provider
                           (scripted-client [(response 204 "")] later-calls))
                          "example.com."
                          records
                          (opts)))]
      (assertions
        {:before         before
         :before-methods (mapv :method @before-calls)
         :later          later
         :later-methods  (mapv :method @later-calls)}
        => {:before
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :set-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            :before-methods []
            :later
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :set-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? false}}
            :later-methods  [:put]})))

  (behavior "deletes selected Stored Records and preserves every survivor"
    (let [calls    (atom [])
          existing [(api-record "keep" 600 "TXT" "unrelated")
                    (api-record "host" 600 "A" "192.0.2.1")
                    (api-record "host" 600 "A" "192.0.2.2")
                    (api-record "host" 600 "TXT" "")
                    (api-record "host" 600 "TXT" "keep")
                    (api-record "ttlwild" 900 "A" "192.0.2.3")
                    (api-record "datawild" 900 "TXT" "one")
                    (api-record "datawild" 900 "TXT" "two")
                    (api-record "all" 1200 "A" "192.0.2.4")
                    (api-record "all" 1200 "TXT" "gone")
                    (api-record "voip" 600 "SRV" "sip.example.net"
                                {:port     5060
                                 :priority 10
                                 :protocol "_tcp"
                                 :service  "_sip"
                                 :weight   5})
                    (api-record "voip" 900 "SRV" "xmpp.example.net"
                                {:port     5222
                                 :priority 20
                                 :protocol "_tcp"
                                 :service  "_xmpp"
                                 :weight   1})]
          client   (scripted-client
                    (into [(response existing)]
                          (repeat 7 (response 204 "")))
                    calls)
          result   (p53/delete-records!
                    (provider client)
                    "Example.COM."
                    [{:name "HOST" :type "txt" :data ""}
                     {:name "host.example.com." :ttl 300 :type "A"
                      :data "192.0.2.1"}
                     {:name "ttlwild" :ttl 600}
                     {:name "ttlwild" :type "A" :data "192.0.2.3"}
                     {:name "datawild" :ttl 900 :type "TXT"}
                     {:name "all"}
                     {:name "_SIP._TCP.VoIP"}
                     {:name "host" :ttl 300 :type "A" :data "192.0.2.1"}
                     {:name "missing"}]
                    (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "host" :ttl 600 :type "TXT" :data ""}
                       {:name "host" :ttl 600 :type "A" :data "192.0.2.1"}
                       {:name "ttlwild" :ttl 900 :type "A" :data "192.0.2.3"}
                       {:name "datawild" :ttl 900 :type "TXT" :data "one"}
                       {:name "datawild" :ttl 900 :type "TXT" :data "two"}
                       {:name "all" :ttl 1200 :type "A" :data "192.0.2.4"}
                       {:name "all" :ttl 1200 :type "TXT" :data "gone"}
                       {:name "_sip._tcp.voip"             :ttl 600 :type "SRV"
                        :data "10 5 5060 sip.example.net."}]}}
        (mapv #(select-keys % [:method :path :body]) @calls)
        => [{:method :get
             :path   "/v1/domains/example.com/records"}
            {:method :put
             :path   "/v1/domains/example.com/records/TXT/host"
             :body   [{:data "keep" :ttl 600}]}
            {:method :put
             :path   "/v1/domains/example.com/records/A/host"
             :body   [{:data "192.0.2.2" :ttl 600}]}
            {:method :delete
             :path   "/v1/domains/example.com/records/A/ttlwild"}
            {:method :delete
             :path   "/v1/domains/example.com/records/TXT/datawild"}
            {:method :delete
             :path   "/v1/domains/example.com/records/A/all"}
            {:method :delete
             :path   "/v1/domains/example.com/records/TXT/all"}
            {:method :put
             :path   "/v1/domains/example.com/records/SRV/voip"
             :body   [{:data     "xmpp.example.net"
                       :port     5222
                       :priority 20
                       :protocol "_tcp"
                       :service  "_xmpp"
                       :ttl      900
                       :weight   1}]}])))

  (behavior "short-circuits empty and unmatched Deletes"
    (let [calls  (atom [])
          sut    (provider
                  (scripted-client
                   [(response [(api-record "keep" 600 "A" "192.0.2.1")])]
                   calls))
          result [(p53/delete-records! sut "example.com." [] (opts))
                  (p53/delete-records!
                   sut
                   "example.com."
                   [{:name "missing"}]
                   (opts))]]
      (assertions
        {:outcomes result :methods (mapv :method @calls)}
        => {:outcomes [{:ol.protocol53/result {:records []}}
                       {:ol.protocol53/result {:records []}}]
            :methods  [:get]})))

  (behavior "validates the complete Delete plan before writing"
    (let [local-calls  (atom [])
          local        (fn [selectors]
                         (p53/delete-records!
                          (provider (scripted-client [] local-calls))
                          "example.com."
                          selectors
                          (opts)))
          remote-calls (atom [])
          remote       (p53/delete-records!
                        (provider
                         (scripted-client
                          [(response [{:name "www" :type "A" :data "192.0.2.1"}])]
                          remote-calls))
                        "example.com."
                        [{:name "www"}]
                        (opts))
          invalid      {:ol.protocol53/error
                        {:type       :invalid-record
                         :message    "Invalid GoDaddy record data"
                         :operation  :delete-records
                         :provider   :godaddy
                         :zone       "example.com."
                         :zone-state :unchanged
                         :retryable? false}}]
      (assertions
        {:local-outcomes [(local [{:name "www"}
                                  {:name "mail" :type "MX" :data "malformed"}])
                          (local [{:name " "}])
                          (local [{:name "www" :type " "}])]
         :local-methods  (mapv :method @local-calls)
         :remote-outcome remote
         :remote-methods (mapv :method @remote-calls)}
        => {:local-outcomes [invalid invalid invalid]
            :local-methods  []
            :remote-outcome
            {:ol.protocol53/error
             {:type       :provider-response
              :message    "GoDaddy returned an invalid response"
              :operation  :delete-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            :remote-methods [:get]})))

  (behavior "stops Delete at the first uncertain failure"
    (let [existing  [(api-record "one" 600 "A" "192.0.2.1")
                     (api-record "two" 600 "TXT" "two")
                     (api-record "@" 600 "MX" "mail.example.net"
                                 {:priority 10})]
          selectors [{:name "one"} {:name "two"} {:name "@"}]
          run       (fn [steps]
                      (let [calls  (atom [])
                            result (p53/delete-records!
                                    (provider (scripted-client steps calls))
                                    "example.com."
                                    selectors
                                    (opts))]
                        {:outcome result :methods (mapv :method @calls)}))
          runs      [(run [(response 503 "unavailable")])
                     (run [(response existing)
                           {:throw (java.io.IOException. "offline")}])
                     (run [(response existing) (response 503 "unavailable")])
                     (run [(response existing) (response 200 "not-json")])
                     (run [(response existing) (response 204 "")
                           (response 503 "unavailable")])]]
      (assertions
        runs
        => [{:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :delete-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unchanged
               :retryable? true}}
             :methods [:get]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed"
               :operation  :delete-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:get :delete]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :delete-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:get :delete]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-response
               :message    "GoDaddy returned malformed JSON"
               :operation  :delete-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? false}}
             :methods [:get :delete]}
            {:outcome
             {:ol.protocol53/error
              {:type       :provider-request
               :message    "GoDaddy request failed with HTTP 503"
               :operation  :delete-records
               :provider   :godaddy
               :zone       "example.com."
               :zone-state :unknown
               :retryable? true}}
             :methods [:get :delete :delete]}])))

  (behavior "distinguishes Delete deadlines before and after an earlier write"
    (let [existing      [(api-record "one" 600 "A" "192.0.2.1")
                         (api-record "two" 600 "TXT" "two")]
          selectors     [{:name "one"} {:name "two"}]
          before-calls  (atom [])
          before-checks (atom 0)
          before        (with-redefs [deadline/expired? (constantly false)
                                      deadline/remaining
                                      (fn [_]
                                        (if (= 1 (swap! before-checks inc))
                                          (Duration/ofSeconds 5)
                                          (Duration/ofNanos 999999)))]
                          (p53/delete-records!
                           (provider
                            (scripted-client [(response existing)] before-calls))
                           "example.com."
                           selectors
                           (opts)))
          later-calls   (atom [])
          later-checks  (atom 0)
          later         (with-redefs [deadline/expired? (constantly false)
                                      deadline/remaining
                                      (fn [_]
                                        (if (<= (swap! later-checks inc) 2)
                                          (Duration/ofSeconds 5)
                                          (Duration/ofNanos 999999)))]
                          (p53/delete-records!
                           (provider
                            (scripted-client [(response existing)
                                              (response 204 "")]
                                             later-calls))
                           "example.com."
                           selectors
                           (opts)))]
      (assertions
        {:before         before
         :before-methods (mapv :method @before-calls)
         :later          later
         :later-methods  (mapv :method @later-calls)}
        => {:before
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :delete-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            :before-methods [:get]
            :later
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded during GoDaddy request"
              :operation  :delete-records
              :provider   :godaddy
              :zone       "example.com."
              :zone-state :unknown
              :retryable? false}}
            :later-methods  [:get :delete]})))

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
