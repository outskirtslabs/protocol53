(ns ol.protocol53.porkbun-test
  (:require
   [babashka.json :as json]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53 :as p53]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.porkbun :as porkbun])
  (:import
   [java.net URI]
   [java.time Duration Instant]))

(defn- opts
  ([]
   (opts 5))
  ([seconds]
   {:timeout (Duration/ofSeconds seconds)}))

(defn- request-view [request]
  (let [^URI uri (:uri request)]
    {:method          (:method request)
     :path            (.getPath uri)
     :content-type    (get-in request [:headers "Content-Type"])
     :idempotency-key (get-in request [:headers "Idempotency-Key"])
     :timeout         (:timeout request)
     :body            (some-> (:body request) json/read-str)}))

(defn- response
  ([body]
   (response 200 {} body))
  ([status headers body]
   {:status status :headers headers :body body}))

(defn- scripted-client [steps calls]
  (let [remaining (atom steps)]
    (fn [request]
      (swap! calls conj (request-view request))
      (let [step (first @remaining)]
        (swap! remaining #(vec (rest %)))
        (when-not step
          (throw (IllegalStateException. "Unexpected Porkbun request")))
        (when-let [delay-ms (:delay-ms step)]
          (Thread/sleep (long delay-ms)))
        (if-let [failure (:throw step)]
          (throw failure)
          {:status  (:status step)
           :headers (:headers step)
           :body    (if (string? (:body step))
                      (:body step)
                      (json/write-str (:body step)))})))))

(defn- domain
  ([name]
   (domain name 1))
  ([name api-access]
   {:apiAccess    api-access
    :autoRenew    1
    :createDate   "2025-01-01 00:00:00"
    :domain       name
    :expireDate   "2027-01-01 00:00:00"
    :notLocal     0
    :securityLock 1
    :status       "ACTIVE"
    :tld          (last (str/split name #"\."))
    :whoisPrivacy 1}))

(defn- api-record
  ([id name ttl type content]
   (api-record id name ttl type content nil))
  ([id name ttl type content priority]
   {:content content
    :id      id
    :name    name
    :notes   nil
    :prio    priority
    :ttl     (str ttl)
    :type    type}))

(defn- dns-response [records]
  {:cloudflare "disabled"
   :records    records
   :status     "SUCCESS"})

(def credentials
  {:apikey       "api-key"
   :secretapikey "secret-key"})

(defn- provider [client]
  (porkbun/provider {:api-key     "api-key"
                     :secret-key  "secret-key"
                     :http-client client}))

(specification "The Porkbun provider"
  (behavior "lists every operable domain page"
    (let [calls          (atom [])
          first-page     (mapv #(domain (str "zone-" % ".test"))
                               (range 1000))
          second-page    [(domain "restricted.example")
                          (domain "legacy.example" 0)
                          (domain "disabled.example" 0)]
          expected-zones (mapv (fn [{:keys [domain]}]
                                 {:name (str domain ".")})
                               (conj first-page (second second-page)))
          success        (response (dns-response []))
          client         (scripted-client
                          (concat
                           [(response {:count   1000
                                       :domains first-page
                                       :status  "SUCCESS"})]
                           (repeat 1000 success)
                           [(response {:count   3
                                       :domains second-page
                                       :status  "SUCCESS"})
                            (response 403 {}
                                      {:code   "DOMAIN_NOT_ALLOWED"
                                       :status "ERROR"})
                            success
                            (response 400 {}
                                      {:code   "INVALID_DOMAIN"
                                       :status "ERROR"})])
                          calls)
          result         (p53/list-zones! (provider client) (opts))
          list-calls     (filterv #(str/ends-with? (:path %) "/domain/listAll")
                                  @calls)
          probe-calls    (filterv #(str/includes? (:path %) "/dns/retrieve/")
                                  @calls)]
      (assertions
        result => {:ol.protocol53/result {:zones expected-zones}}
        "uses body authentication and advances the offset"
        (mapv #(select-keys % [:method :path :content-type :body]) list-calls)
        => [{:method       :post
             :path         "/api/json/v3/domain/listAll"
             :content-type "application/json"
             :body         (assoc credentials :start 0)}
            {:method       :post
             :path         "/api/json/v3/domain/listAll"
             :content-type "application/json"
             :body         (assoc credentials :start 1000)}]
        "probes every candidate regardless of API metadata"
        (count probe-calls) => 1003
        (mapv :path (take-last 3 probe-calls))
        => ["/api/json/v3/dns/retrieve/restricted.example"
            "/api/json/v3/dns/retrieve/legacy.example"
            "/api/json/v3/dns/retrieve/disabled.example"]
        "bounds each request by the remaining deadline"
        (every? #(<= 1 (:timeout %) 5000) @calls) => true)))

  (behavior "propagates unexpected zone probe failures"
    (let [candidate   {:count  1         :domains [(domain "candidate.example")]
                       :status "SUCCESS"}
          invoke      (fn [probe calls]
                        (p53/list-zones!
                         (provider (scripted-client [(response candidate) probe]
                                                    calls))
                         (opts)))
          bad-calls   (atom [])
          retry-calls (atom [])
          shape-calls (atom [])
          bad         (invoke (response 400 {}
                                        {:code "INVALID_TYPE" :status "ERROR"})
                              bad-calls)
          retry       (invoke (response 503 {} "unavailable") retry-calls)
          malformed   (invoke (response {:status "SUCCESS"}) shape-calls)]
      (assertions
        [bad retry malformed]
        => [{:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed with HTTP 400"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed with HTTP 503"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-response
              :message    "Porkbun returned an invalid response"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? false}}]
        [(count @bad-calls) (count @retry-calls) (count @shape-calls)]
        => [2 2 2])))

  (behavior "gets and normalizes portable records"
    (let [calls   (atom [])
          records [(api-record "1" "example.com" 600 "A" "192.0.2.1")
                   (api-record "2" "www.example.com" 600 "CNAME"
                               "target.example.net")
                   (api-record "3" "example.com" 900 "MX"
                               "mail.example.net" "10")
                   (api-record "4" "_sip._tcp.example.com" 600 "SRV"
                               "5 443 service.example.net" "20")
                   (api-record "5" "txt.example.com" 600 "TXT"
                               "hello \"world\"")
                   (api-record "6" "example.com" 600 "CAA"
                               "0 issue \"letsencrypt.org\"")
                   (api-record "7" "https.example.com" 600 "HTTPS"
                               "1 svc.example.net. alpn=\"h2,h3\"")
                   (api-record "8" "svcb.example.com" 600 "SVCB"
                               "1 . alpn=\"dot\"")]
          client  (scripted-client [(response (dns-response records))] calls)
          result  (p53/get-records! (provider client) "Example.COM." (opts))]

      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "@" :ttl 600 :type "A" :data "192.0.2.1"}
                       {:name "www"                 :ttl 600 :type "CNAME"
                        :data "target.example.net."}
                       {:name "@"                    :ttl 900 :type "MX"
                        :data "10 mail.example.net."}
                       {:name "_sip._tcp" :ttl 600 :type "SRV"
                        :data "20 5 443 service.example.net."}
                       {:name "txt"             :ttl 600 :type "TXT"
                        :data "hello \"world\""}
                       {:name "@" :ttl 600 :type "CAA"
                        :data "0 issue \"letsencrypt.org\""}
                       {:name "https" :ttl 600 :type "HTTPS"
                        :data "1 svc.example.net. alpn=h2,h3"}
                       {:name "svcb"         :ttl 600 :type "SVCB"
                        :data "1 . alpn=dot"}]}}
        (mapv #(select-keys % [:method :path :body]) @calls)
        => [{:method :post
             :path   "/api/json/v3/dns/retrieve/example.com"
             :body   credentials}])))

  (behavior "appends records with Porkbun names, priorities, and minimum TTLs"
    (let [calls   (atom [])
          records [{:name "WWW.Example.com." :ttl 300 :type "a"
                    :data "192.0.2.10"}
                   {:name "@"                    :ttl 900 :type "MX"
                    :data "10 mail.example.net."}
                   {:name "_sip._tcp" :ttl 900 :type "SRV"
                    :data "20 5 443 service.example.net."}
                   {:name "alias"               :ttl 900 :type "CNAME"
                    :data "target.example.net."}]
          client  (scripted-client
                   [(response {:status "SUCCESS"})
                    (response {:id 12 :status "SUCCESS"})
                    (response {:id "13" :status "SUCCESS"})
                    (response {:id "14" :status "SUCCESS"})
                    (response
                     (dns-response
                      [(api-record "12" "www.example.com" 1200 "A"
                                   "192.0.2.10")
                       (api-record "13" "example.com" 900 "MX"
                                   "mail.example.net" "10")
                       (api-record "14" "_sip._tcp.example.com" 900 "SRV"
                                   "5 443 service.example.net" "20")
                       (api-record "15" "alias.example.com" 900 "CNAME"
                                   "target.example.net")]))]
                   calls)
          result  (p53/append-records!
                   (provider client) "example.com." records (opts))]

      (assertions
        "returns values as Porkbun stores them"
        result
        => {:ol.protocol53/result
            {:records [{:name "www"        :ttl 1200 :type "A"
                        :data "192.0.2.10"}
                       {:name "@"                    :ttl 900 :type "MX"
                        :data "10 mail.example.net."}
                       {:name "_sip._tcp" :ttl 900 :type "SRV"
                        :data "20 5 443 service.example.net."}
                       {:name "alias"               :ttl 900 :type "CNAME"
                        :data "target.example.net."}]}}
        "sends one create request per record"
        (mapv #(select-keys % [:method :path :body]) @calls)
        => [{:method :post
             :path   "/api/json/v3/dns/create/example.com"
             :body   (merge credentials
                            {:content "192.0.2.10"
                             :name    "www"
                             :ttl     600
                             :type    "A"})}
            {:method :post
             :path   "/api/json/v3/dns/create/example.com"
             :body   (merge credentials
                            {:content "mail.example.net"
                             :name    ""
                             :prio    10
                             :ttl     900
                             :type    "MX"})}
            {:method :post
             :path   "/api/json/v3/dns/create/example.com"
             :body   (merge credentials
                            {:content "5 443 service.example.net"
                             :name    "_sip._tcp"
                             :prio    20
                             :ttl     900
                             :type    "SRV"})}
            {:method :post
             :path   "/api/json/v3/dns/create/example.com"
             :body   (merge credentials
                            {:content "target.example.net"
                             :name    "alias"
                             :ttl     900
                             :type    "CNAME"})}
            {:method :post
             :path   "/api/json/v3/dns/retrieve/example.com"
             :body   credentials}])))

  (behavior "sets complete selected RRsets"
    (let [calls    (atom [])
          existing [(api-record "21" "www.example.com" 600 "A" "192.0.2.1")
                    (api-record "22" "www.example.com" 600 "A" "192.0.2.2")
                    (api-record "23" "keep.example.com" 600 "TXT" "keep")]
          desired  [{:name "WWW" :ttl 300 :type "a" :data "192.0.2.20"}
                    {:name "www" :ttl 300 :type "A" :data "192.0.2.21"}]
          stored   [(api-record "21" "www.example.com" 600 "A" "192.0.2.20")
                    (api-record "22" "www.example.com" 600 "A" "192.0.2.21")
                    (api-record "23" "keep.example.com" 600 "TXT" "keep")]
          client   (scripted-client
                    [(response (dns-response existing))
                     (response {:status "SUCCESS" :wouldSucceed true})
                     (response {:status "SUCCESS" :wouldSucceed true})
                     (response {:status "SUCCESS"})
                     (response {:status "SUCCESS"})
                     (response (dns-response stored))]
                    calls)
          result   (p53/set-records!
                    (provider client) "example.com." desired (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www" :ttl 600 :type "A" :data "192.0.2.20"}
                       {:name "www" :ttl 600 :type "A" :data "192.0.2.21"}]}}
        "preflights every value and edits existing records by ID"
        (mapv #(select-keys % [:path :body]) @calls)
        => [{:path "/api/json/v3/dns/retrieve/example.com"
             :body credentials}
            {:path "/api/json/v3/dns/create/example.com"
             :body (merge credentials
                          {:content "192.0.2.20"
                           :dryRun  true
                           :name    "www"
                           :ttl     600
                           :type    "A"})}
            {:path "/api/json/v3/dns/create/example.com"
             :body (merge credentials
                          {:content "192.0.2.21"
                           :dryRun  true
                           :name    "www"
                           :ttl     600
                           :type    "A"})}
            {:path "/api/json/v3/dns/edit/example.com/21"
             :body (merge credentials
                          {:content "192.0.2.20"
                           :name    "www"
                           :ttl     600
                           :type    "A"})}
            {:path "/api/json/v3/dns/edit/example.com/22"
             :body (merge credentials
                          {:content "192.0.2.21"
                           :name    "www"
                           :ttl     600
                           :type    "A"})}
            {:path "/api/json/v3/dns/retrieve/example.com"
             :body credentials}])))

  (behavior "preserves matching values while expanding an RRset"
    (let [calls    (atom [])
          existing [(api-record "24" "www.example.com" 600 "A" "192.0.2.1")
                    (api-record "25" "www.example.com" 600 "A" "192.0.2.2")]
          desired  [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
                    {:name "www" :ttl 600 :type "A" :data "192.0.2.3"}
                    {:name "www" :ttl 600 :type "A" :data "192.0.2.4"}]
          stored   [(api-record "24" "www.example.com" 600 "A" "192.0.2.1")
                    (api-record "25" "www.example.com" 600 "A" "192.0.2.3")
                    (api-record "26" "www.example.com" 600 "A" "192.0.2.4")]
          valid    (response {:status "SUCCESS" :wouldSucceed true})
          client   (scripted-client
                    [(response (dns-response existing))
                     valid
                     valid
                     valid
                     (response {:status "SUCCESS"})
                     (response {:status "SUCCESS"})
                     (response (dns-response stored))]
                    calls)
          result   (p53/set-records!
                    (provider client) "example.com." desired (opts))]
      (assertions
        result => {:ol.protocol53/result
                   {:records desired}}
        (mapv :path (drop 4 @calls))
        => ["/api/json/v3/dns/edit/example.com/25"
            "/api/json/v3/dns/create/example.com"
            "/api/json/v3/dns/retrieve/example.com"]
        (some #(str/includes? (:path %) "/dns/delete/") @calls) => nil)))

  (behavior "validates every desired record before changing an RRset"
    (let [calls    (atom [])
          existing [(api-record "26" "later.example.com" 600 "TXT" "old")]
          desired  [{:name "new" :ttl 600 :type "A" :data "192.0.2.20"}
                    {:name "later" :ttl 600 :type "TXT" :data "new"}]
          client   (scripted-client
                    [(response (dns-response existing))
                     (response {:dryRun       true
                                :status       "SUCCESS"
                                :wouldSucceed true})
                     (response 400 {}
                               {:code   "INVALID_RECORD"
                                :status "ERROR"})]
                    calls)
          result   (p53/set-records!
                    (provider client) "example.com." desired (opts))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Porkbun request failed with HTTP 400"
             :operation  :set-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        (mapv #(select-keys % [:path :body]) @calls)
        => [{:path "/api/json/v3/dns/retrieve/example.com"
             :body credentials}
            {:path "/api/json/v3/dns/create/example.com"
             :body (merge credentials
                          {:content "192.0.2.20"
                           :dryRun  true
                           :name    "new"
                           :ttl     600
                           :type    "A"})}
            {:path "/api/json/v3/dns/create/example.com"
             :body (merge credentials
                          {:content "new"
                           :dryRun  true
                           :name    "later"
                           :ttl     600
                           :type    "TXT"})}])))

  (behavior "rejects an ambiguous dry-run result before writing"
    (let [calls  (atom [])
          record {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          client (scripted-client
                  [(response (dns-response []))
                   (response {:status "SUCCESS" :wouldSucceed false})]
                  calls)
          result (p53/set-records!
                  (provider client) "example.com." [record] (opts))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-response
             :message    "Porkbun returned an invalid response"
             :operation  :set-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        (count @calls) => 2)))

  (behavior "stops applying RRsets after the first actual write failure"
    (let [calls    (atom [])
          existing [(api-record "27" "later.example.com" 600 "TXT" "old")]
          desired  [{:name "new" :ttl 600 :type "A" :data "192.0.2.20"}
                    {:name "later" :ttl 600 :type "TXT" :data "new"}]
          valid    (response {:dryRun       true
                              :status       "SUCCESS"
                              :wouldSucceed true})
          client   (scripted-client
                    [(response (dns-response existing))
                     valid
                     valid
                     (response 400 {}
                               {:code   "INVALID_RECORD"
                                :status "ERROR"})]
                    calls)
          result   (p53/set-records!
                    (provider client) "example.com." desired (opts))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Porkbun request failed with HTTP 400"
             :operation  :set-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        (mapv :path @calls)
        => ["/api/json/v3/dns/retrieve/example.com"
            "/api/json/v3/dns/create/example.com"
            "/api/json/v3/dns/create/example.com"
            "/api/json/v3/dns/create/example.com"]
        (some #(str/includes? (:path %) "/dns/delete/") @calls) => nil
        (some #(str/includes? (:path %) "/dns/edit/") @calls) => nil)))

  (behavior "deletes wildcard and exact selector matches by record ID"
    (let [calls    (atom [])
          existing [(api-record "31" "host.example.com" 600 "A" "192.0.2.1")
                    (api-record "32" "host.example.com" 600 "A" "192.0.2.2")
                    (api-record "33" "host.example.com" 600 "TXT" "")
                    (api-record "34" "host.example.com" 600 "TXT" "keep")
                    (api-record "35" "all.example.com" 900 "A" "192.0.2.3")
                    (api-record "36" "all.example.com" 900 "TXT" "gone")]
          client   (scripted-client
                    (into [(response (dns-response existing))]
                          (repeat 4 (response {:status "SUCCESS"})))
                    calls)
          result   (p53/delete-records!
                    (provider client)
                    "example.com."
                    [{:name "HOST" :ttl 600 :type "a" :data "192.0.2.1"}
                     {:name "host" :type "TXT" :data ""}
                     {:name "all"}
                     {:name "missing"}]
                    (opts))]

      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "host"      :ttl 600 :type "A"
                        :data "192.0.2.1"}
                       {:name "host" :ttl 600 :type "TXT" :data ""}
                       {:name "all"       :ttl 900 :type "A"
                        :data "192.0.2.3"}
                       {:name "all" :ttl 900 :type "TXT" :data "gone"}]}}
        "removes only matching IDs"
        (mapv :path (rest @calls))
        => ["/api/json/v3/dns/delete/example.com/31"
            "/api/json/v3/dns/delete/example.com/33"
            "/api/json/v3/dns/delete/example.com/35"
            "/api/json/v3/dns/delete/example.com/36"])))

  (behavior "short-circuits empty mutations"
    (let [calls  (atom [])
          client (scripted-client [] calls)
          sut    (provider client)
          op     (opts)]

      (assertions
        [(p53/append-records! sut "example.com." [] op)
         (p53/set-records! sut "example.com." [] op)
         (p53/delete-records! sut "example.com." [] op)]
        => (vec (repeat 3 {:ol.protocol53/result {:records []}}))
        @calls => [])))

  (behavior "redacts credentials from every standard printed representation"
    (let [sut     (porkbun/provider {:api-key    "visible-api-secret"
                                     :secret-key "visible-secret-secret"})
          outputs [(pr-str sut)
                   (binding [*print-dup* true] (pr-str sut))
                   (with-out-str (pprint/pprint sut))]]
      (assertions
        (every? #(str/includes? % "<redacted>") outputs) => true
        (some #(or (str/includes? % "visible-api-secret")
                   (str/includes? % "visible-secret-secret"))
              outputs)
        => nil)))

  (behavior "rejects malformed local and remote record data"
    (let [local-calls  (atom [])
          remote-calls (atom [])
          local        (p53/append-records!
                        (provider (scripted-client [] local-calls))
                        "example.com."
                        [{:name "mail" :ttl 600 :type "MX" :data "bad"}]
                        (opts))
          remote       (p53/get-records!
                        (provider
                         (scripted-client
                          [(response
                            (dns-response
                             [(assoc (api-record "1" "example.com" 600
                                                 "A" "192.0.2.1")
                                     :ttl "soon")]))]
                          remote-calls))
                        "example.com."
                        (opts))]

      (assertions
        local
        => {:ol.protocol53/error
            {:type       :invalid-record
             :message    "Invalid Porkbun record data"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        remote
        => {:ol.protocol53/error
            {:type       :provider-response
             :message    "Porkbun returned an invalid response"
             :operation  :get-records
             :provider   :porkbun
             :zone       "example.com."
             :retryable? false}}
        @local-calls => []
        (count @remote-calls) => 1)))

  (behavior "classifies failures without leaking credentials or response text"
    (let [api-key      "visible-api-secret"
          secret-key   "visible-secret-secret"
          config       (fn [steps]
                         (porkbun/provider
                          {:api-key     api-key
                           :secret-key  secret-key
                           :http-client (scripted-client steps (atom []))}))
          transport    (p53/list-zones!
                        (config [{:throw (java.io.IOException. "offline")}])
                        (opts))
          limited      (p53/list-zones!
                        (config [(response 429 {} "slow down")])
                        (opts))
          malformed    (p53/list-zones!
                        (config [(response "not-json")])
                        (opts))
          unsuccessful (p53/list-zones!
                        (config [(response {:code    "INVALID_API_KEYS_001"
                                            :message (str api-key secret-key)
                                            :status  "ERROR"})])
                        (opts))
          outcomes     [transport limited malformed unsuccessful]]

      (assertions
        outcomes
        => [{:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed with HTTP 429"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-response
              :message    "Porkbun returned malformed JSON"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun returned an unsuccessful response"
              :operation  :list-zones
              :provider   :porkbun
              :retryable? false}}]
        "keeps credentials and provider messages out of outcomes"
        (some #(or (str/includes? (pr-str %) api-key)
                   (str/includes? (pr-str %) secret-key))
              outcomes)
        => nil)))

  (behavior "reports whether a failed mutation might have changed the zone"
    (let [record       {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          before-calls (atom [])
          after-calls  (atom [])
          before       (p53/set-records!
                        (provider
                         (scripted-client
                          [{:throw (java.io.IOException. "offline")}]
                          before-calls))
                        "example.com."
                        [record]
                        (opts))
          after        (p53/append-records!
                        (provider
                         (scripted-client
                          (repeat 3
                                  (response 503
                                            {"Retry-After" "0"}
                                            "unavailable"))
                          after-calls))
                        "example.com."
                        [record]
                        (opts))]

      (assertions
        before
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Porkbun request failed"
             :operation  :set-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? true}}
        after
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Porkbun request failed with HTTP 503"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? true}})))

  (behavior "retries transient mutation failures with one idempotency key"
    (let [calls  (atom [])
          client (scripted-client
                  [(response 503 {:Retry-After "0"} "unavailable")
                   {:throw (java.io.IOException. "connection reset")}
                   (response {:status "SUCCESS"})
                   (response
                    (dns-response
                     [(api-record "40" "www.example.com" 600 "A"
                                  "192.0.2.1")]))]
                  calls)
          result (p53/append-records!
                  (provider client)
                  "example.com."
                  [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                  (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www"       :ttl 600 :type "A"
                        :data "192.0.2.1"}]}}
        "replays the same request no more than needed"
        (mapv #(select-keys % [:path :body]) (take 3 @calls))
        => (vec
            (repeat 3
                    {:path "/api/json/v3/dns/create/example.com"
                     :body (merge credentials
                                  {:content "192.0.2.1"
                                   :name    "www"
                                   :ttl     600
                                   :type    "A"})}))
        "uses one nonempty key for every attempt"
        (let [keys (mapv :idempotency-key (take 3 @calls))]
          [(count (distinct keys)) (boolean (seq (first keys)))])
        => [1 true])))

  (behavior "retries an in-flight idempotent replay with the same key"
    (let [calls  (atom [])
          stored (api-record "41" "www.example.com" 600 "A" "192.0.2.1")
          client (scripted-client
                  [(response 409 {"Retry-After" "0"}
                             {:code   "IDEMPOTENCY_KEY_IN_USE"
                              :status "ERROR"})
                   (response {:id "41" :status "SUCCESS"})
                   (response (dns-response [stored]))]
                  calls)
          result (with-redefs [deadline/remaining
                               (constantly (Duration/ofMillis 500))]
                   (p53/append-records!
                    (provider client)
                    "example.com."
                    [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www"       :ttl 600 :type "A"
                        :data "192.0.2.1"}]}}
        (count @calls) => 3
        (mapv :idempotency-key (take 2 @calls))
        => (vec (repeat 2 (:idempotency-key (first @calls)))))))

  (behavior "classifies exhausted and mismatched idempotency replays"
    (let [record         {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          in-use-calls   (atom [])
          mismatch-calls (atom [])
          in-use         (response 409 {"Retry-After" "0"}
                                   {:code   "IDEMPOTENCY_KEY_IN_USE"
                                    :status "ERROR"})
          invoke         (fn [client]
                           (p53/append-records!
                            (provider client) "example.com." [record] (opts)))
          exhausted      (invoke
                          (scripted-client (repeat 3 in-use) in-use-calls))
          mismatched     (invoke
                          (scripted-client
                           [(response 409 {}
                                      {:code   "IDEMPOTENCY_KEY_MISMATCH"
                                       :status "ERROR"})]
                           mismatch-calls))]
      (assertions
        [exhausted mismatched]
        => [{:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed with HTTP 409"
              :operation  :append-records
              :provider   :porkbun
              :zone       "example.com."
              :zone-state :unknown
              :retryable? true}}
            {:ol.protocol53/error
             {:type       :provider-request
              :message    "Porkbun request failed with HTTP 409"
              :operation  :append-records
              :provider   :porkbun
              :zone       "example.com."
              :zone-state :unknown
              :retryable? false}}]
        (count (distinct (map :idempotency-key @in-use-calls))) => 1
        (count @mismatch-calls) => 1)))

  (behavior "uses Porkbun rate-limit body reset signals"
    (let [calls  (atom [])
          stored (api-record "42" "www.example.com" 600 "A" "192.0.2.1")
          client (scripted-client
                  [(response {:code         "RATE_LIMIT_EXCEEDED"
                              :status       "ERROR"
                              :ttlRemaining 0})
                   (response {:id "42" :status "SUCCESS"})
                   (response (dns-response [stored]))]
                  calls)
          result (with-redefs [deadline/remaining
                               (constantly (Duration/ofMillis 500))]
                   (p53/append-records!
                    (provider client)
                    "example.com."
                    [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result => {:ol.protocol53/result
                   {:records [{:name "www"       :ttl 600 :type "A"
                               :data "192.0.2.1"}]}}
        (count @calls) => 3)))

  (behavior "interprets X-RateLimit-Reset as an epoch"
    (let [now           (.getEpochSecond (Instant/now))
          stored        (api-record "43" "www.example.com" 600 "A" "192.0.2.1")
          record        {:name "www" :ttl 600 :type "A" :data "192.0.2.1"}
          steps         (fn [reset]
                          [(response 429
                                     {"X-RateLimit-Reset" (str reset)}
                                     {:code "RATE_LIMIT_EXCEEDED" :status "ERROR"})
                           (response {:id "43" :status "SUCCESS"})
                           (response (dns-response [stored]))])
          invoke        (fn [reset remaining calls]
                          (with-redefs [deadline/remaining (constantly remaining)]
                            (p53/append-records!
                             (provider (scripted-client (steps reset) calls))
                             "example.com." [record] (opts))))
          within-calls  (atom [])
          past-calls    (atom [])
          outside-calls (atom [])
          within        (invoke (inc now) (Duration/ofSeconds 2) within-calls)
          past          (invoke (- now 30) (Duration/ofMillis 500) past-calls)
          outside       (invoke (+ now 30) (Duration/ofSeconds 5) outside-calls)]
      (assertions
        [within past]
        => (vec (repeat 2 {:ol.protocol53/result {:records [record]}}))
        outside
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded while waiting to retry Porkbun request"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        [(count @within-calls) (count @past-calls) (count @outside-calls)]
        => [3 3 1])))

  (behavior "does not outwait an advertised rate-limit reset"
    (let [calls  (atom [])
          client (scripted-client
                  [(response {:code         "RATE_LIMIT_EXCEEDED"
                              :status       "ERROR"
                              :ttlRemaining 2})]
                  calls)
          result (with-redefs [deadline/remaining
                               (constantly (Duration/ofMillis 1500))]
                   (p53/append-records!
                    (provider client)
                    "example.com."
                    [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded while waiting to retry Porkbun request"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        (count @calls) => 1)))

  (behavior "does not begin a retry wait that exceeds the deadline"
    (let [calls  (atom [])
          client (scripted-client [(response 503 {} "unavailable")] calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (constantly (Duration/ofMillis 500))]
                   (p53/append-records!
                    (provider client)
                    "example.com."
                    [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded while waiting to retry Porkbun request"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        (count @calls) => 1)))

  (behavior "does not issue a request for a sub-millisecond remaining budget"
    (let [calls  (atom [])
          client (scripted-client [(response {:count   0
                                              :domains []
                                              :status  "SUCCESS"})]
                                  calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (constantly (Duration/ofNanos 999999))]
                   (p53/list-zones! (provider client) (opts)))]

      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Porkbun request"
             :operation  :list-zones
             :provider   :porkbun
             :retryable? false}}
        @calls => [])))

  (behavior "keeps mutation state unchanged before HTTP dispatch"
    (let [calls  (atom [])
          client (scripted-client [(response {:status "SUCCESS"})] calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (constantly (Duration/ofNanos 999999))]
                   (p53/append-records!
                    (provider client)
                    "example.com."
                    [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Porkbun request"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        @calls => [])))

  (behavior "rechecks the deadline after a write"
    (let [calls  (atom [])
          client (scripted-client
                  [(assoc (response {:id "41" :status "SUCCESS"})
                          :delay-ms 1100)]
                  calls)
          result (p53/append-records!
                  (provider client)
                  "example.com."
                  [{:name "www" :ttl 600 :type "A" :data "192.0.2.1"}]
                  (opts 1))]

      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Porkbun request"
             :operation  :append-records
             :provider   :porkbun
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        (count @calls) => 1))))
