(ns ol.protocol53.cloudflare-test
  (:require
   [babashka.json :as json]
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53 :as p53]
   [ol.protocol53.cloudflare :as cloudflare]
   [ol.protocol53.deadline :as deadline])
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
    {:method        (:method request)
     :path          (.getPath uri)
     :query         (query-map uri)
     :authorization (get-in request [:headers "Authorization"])
     :content-type  (get-in request [:headers "Content-Type"])
     :timeout       (:timeout request)
     :body          (some-> (:body request) json/read-str)}))

(defn- success
  ([result]
   (success result nil))
  ([result result-info]
   {:status 200
    :body   (cond-> {:success true
                     :errors  []
                     :result  result}
              result-info (assoc :result_info result-info))}))

(defn- scripted-client [steps calls]
  (let [remaining (atom steps)]
    (fn [request]
      (swap! calls conj (request-view request))
      (let [step (first @remaining)]
        (swap! remaining #(vec (rest %)))
        (when-not step
          (throw (IllegalStateException. "Unexpected Cloudflare request")))
        (when-let [delay-ms (:delay-ms step)]
          (Thread/sleep (long delay-ms)))
        (if-let [failure (:throw step)]
          (throw failure)
          {:status  (:status step)
           :headers {}
           :body    (if (string? (:body step))
                      (:body step)
                      (json/write-str (:body step)))})))))

(def zone-response
  (success [{:id "zone-1" :name "example.com"}]))

(specification "The Cloudflare provider"
  (behavior "lists every zone page with the read token"
    (let [calls  (atom [])
          client (scripted-client
                  [(success [{:id "zone-1" :name "example.com"}]
                            {:page 1 :per_page 1 :total_count 2})
                   (success [{:id "zone-2" :name "example.net."}]
                            {:page 2 :per_page 1 :total_count 2})]
                  calls)
          result (p53/list-zones!
                  (cloudflare/provider {:api-token   "dns-token"
                                        :zone-token  "zone-token"
                                        :http-client client})
                  (opts))]

      (assertions
        "returns normalized absolute zone names"
        result => {:ol.protocol53/result
                   {:zones [{:name "example.com."}
                            {:name "example.net."}]}}
        "requests all pages through the read credential"
        (mapv #(select-keys % [:method :path :query :authorization]) @calls)
        => [{:method        :get
             :path          "/client/v4/zones"
             :query         {"page" "1" "per_page" "100"}
             :authorization "Bearer zone-token"}
            {:method        :get
             :path          "/client/v4/zones"
             :query         {"page" "2" "per_page" "100"}
             :authorization "Bearer zone-token"}]
        "bounds each request by the remaining deadline"
        (every? #(<= 1 (:timeout %) 5000) @calls) => true)))

  (behavior "gets and normalizes portable records from every page"
    (let [calls   (atom [])
          records [{:id      "a-1"
                    :name    "example.com"
                    :type    "A"
                    :ttl     300
                    :content "192.0.2.1"}
                   {:id      "cname-1"
                    :name    "www.example.com"
                    :type    "CNAME"
                    :ttl     120
                    :content "target.example.net"}
                   {:id       "mx-1"
                    :name     "example.com"
                    :type     "MX"
                    :ttl      600
                    :priority 10
                    :content  "mail.example.com"}]
          more    [{:id      "txt-1"
                    :name    "txt.example.com"
                    :type    "TXT"
                    :ttl     60
                    :content "\"hello\" \" world\""}]
          client  (scripted-client
                   [zone-response
                    (success records
                             {:page 1 :per_page 3 :total_count 4})
                    (success more
                             {:page 2 :per_page 3 :total_count 4})]
                   calls)
          result  (p53/get-records!
                   (cloudflare/provider {:api-token   "dns-token"
                                         :zone-token  "zone-token"
                                         :http-client client})
                   "example.com."
                   (opts))]

      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "@"
                        :ttl  300
                        :type "A"
                        :data "192.0.2.1"}
                       {:name "www"
                        :ttl  120
                        :type "CNAME"
                        :data "target.example.net."}
                       {:name "@"
                        :ttl  600
                        :type "MX"
                        :data "10 mail.example.com."}
                       {:name "txt"
                        :ttl  60
                        :type "TXT"
                        :data "hello world"}]}}
        "uses the read token only for zone discovery"
        (mapv :authorization @calls)
        => ["Bearer zone-token" "Bearer dns-token" "Bearer dns-token"]
        "normalizes the zone lookup and paginates records"
        (mapv #(select-keys % [:path :query]) @calls)
        => [{:path  "/client/v4/zones"
             :query {"name" "example.com"}}
            {:path  "/client/v4/zones/zone-1/dns_records"
             :query {"page" "1" "per_page" "100"}}
            {:path  "/client/v4/zones/zone-1/dns_records"
             :query {"page" "2" "per_page" "100"}}])))

  (behavior "appends records without changing their portable values"
    (let [calls       (atom [])
          long-text   (apply str (repeat 256 "a"))
          encoded-txt (str "\"" (apply str (repeat 255 "a")) "\" \"a\"")
          records     [{:name "www"
                        :ttl  300
                        :type "A"
                        :data "192.0.2.10"}
                       {:name "txt"
                        :ttl  60
                        :type "TXT"
                        :data long-text}
                       {:name "@"
                        :ttl  600
                        :type "MX"
                        :data "10 mail.example.com."}
                       {:name "tunnel"
                        :ttl  1
                        :type "CNAME"
                        :data "abc.cfargotunnel.com."}]
          client      (scripted-client
                       [zone-response
                        (success {:id      "a-1"
                                  :name    "www.example.com"
                                  :ttl     300
                                  :type    "A"
                                  :content "192.0.2.10"})
                        (success {:id      "txt-1"
                                  :name    "txt.example.com"
                                  :ttl     60
                                  :type    "TXT"
                                  :content encoded-txt})
                        (success {:id       "mx-1"
                                  :name     "example.com"
                                  :ttl      600
                                  :type     "MX"
                                  :priority 10
                                  :content  "mail.example.com"})
                        (success {:id      "cname-1"
                                  :name    "tunnel.example.com"
                                  :ttl     1
                                  :type    "CNAME"
                                  :content "abc.cfargotunnel.com"})]
                       calls)
          result      (p53/append-records!
                       (cloudflare/provider {:api-token   "dns-token"
                                             :http-client client})
                       "example.com."
                       records
                       (opts))]

      (assertions
        "returns the records actually created"
        result => {:ol.protocol53/result {:records records}}
        "sends Cloudflare's normalized payloads"
        (mapv :body (rest @calls))
        => [{:name    "www.example.com"
             :ttl     300
             :type    "A"
             :content "192.0.2.10"}
            {:name    "txt.example.com"
             :ttl     60
             :type    "TXT"
             :content encoded-txt}
            {:name     "example.com"
             :ttl      600
             :type     "MX"
             :content  "mail.example.com"
             :priority 10}
            {:name    "tunnel.example.com"
             :ttl     1
             :type    "CNAME"
             :content "abc.cfargotunnel.com"
             :proxied true}]
        "uses JSON POST requests"
        (mapv #(select-keys % [:method :content-type]) (rest @calls))
        => (vec (repeat 4 {:method       :post
                           :content-type "application/json"})))))

  (behavior "round-trips structured CAA, SRV, HTTPS, and SVCB records"
    (let [calls     (atom [])
          records   [{:name "@"
                      :ttl  300
                      :type "CAA"
                      :data "0 issue \"letsencrypt.org\""}
                     {:name "_sip._tcp"
                      :ttl  60
                      :type "SRV"
                      :data "10 5 5060 sip.example.com."}
                     {:name "@"
                      :ttl  120
                      :type "HTTPS"
                      :data "1 svc.example.net. alpn=h2,h3"}
                     {:name "_dns"
                      :ttl  120
                      :type "SVCB"
                      :data "2 svc.example.net. port=853"}]
          responses [{:id   "caa-1"
                      :name "example.com"
                      :ttl  300
                      :type "CAA"
                      :data {:flags 0
                             :tag   "issue"
                             :value "letsencrypt.org"}}
                     {:id   "srv-1"
                      :name "_sip._tcp.example.com"
                      :ttl  60
                      :type "SRV"
                      :data {:priority 10
                             :weight   5
                             :port     5060
                             :target   "sip.example.com"}}
                     {:id      "https-1"
                      :name    "example.com"
                      :ttl     120
                      :type    "HTTPS"
                      :content "1 svc.example.net. alpn=h2,h3"}
                     {:id      "svcb-1"
                      :name    "_dns.example.com"
                      :ttl     120
                      :type    "SVCB"
                      :content "2 svc.example.net. port=853"}]
          client    (scripted-client
                     (concat [zone-response]
                             (map success responses)
                             [zone-response (success responses)])
                     calls)
          sut       (cloudflare/provider {:api-token   "dns-token"
                                          :http-client client})
          appended  (p53/append-records! sut "example.com." records (opts))
          retrieved (p53/get-records! sut "example.com." (opts))]
      (assertions
        "returns portable values from structured create responses"
        appended => {:ol.protocol53/result {:records records}}
        "returns the same portable values when listing records"
        retrieved => {:ol.protocol53/result {:records records}}
        "sends Cloudflare's structured write payloads"
        (->> @calls
             (filter #(= :post (:method %)))
             (mapv :body))
        => [{:name    "example.com"
             :ttl     300
             :type    "CAA"
             :content "0 issue \"letsencrypt.org\""
             :data    {:flags 0
                       :tag   "issue"
                       :value "letsencrypt.org"}}
            {:name "_sip._tcp.example.com"
             :ttl  60
             :type "SRV"
             :data {:service  "_sip"
                    :proto    "_tcp"
                    :name     "example.com"
                    :priority 10
                    :weight   5
                    :port     5060
                    :target   "sip.example.com."}}
            {:name "example.com"
             :ttl  120
             :type "HTTPS"
             :data {:priority 1
                    :target   "svc.example.net."
                    :value    "alpn=h2,h3"}}
            {:name "_dns.example.com"
             :ttl  120
             :type "SVCB"
             :data {:priority 2
                    :target   "svc.example.net."
                    :value    "port=853"}}])))

  (behavior "sets complete selected RRsets instead of upserting one record"
    (let [calls   (atom [])
          desired [{:name "www"
                    :ttl  300
                    :type "A"
                    :data "192.0.2.20"}
                   {:name "www"
                    :ttl  300
                    :type "A"
                    :data "192.0.2.21"}]
          client  (scripted-client
                   [zone-response
                    (success [{:id      "old-1"
                               :name    "www.example.com"
                               :ttl     60
                               :type    "A"
                               :content "192.0.2.1"}
                              {:id      "old-2"
                               :name    "www.example.com"
                               :ttl     60
                               :type    "A"
                               :content "192.0.2.2"}])
                    (success {:id "old-1"})
                    (success {:id "old-2"})
                    (success {:id      "new-1"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.20"})
                    (success {:id      "new-2"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.21"})]
                   calls)
          _       (p53/set-records!
                   (cloudflare/provider {:api-token   "dns-token"
                                         :http-client client})
                   "example.com."
                   desired
                   (opts))]

      (assertions
        "finds the selected RRset, removes it, and creates the replacement"
        (mapv #(select-keys % [:method :path :query :body]) (rest @calls))
        => [{:method :get
             :path   "/client/v4/zones/zone-1/dns_records"
             :query  {"name"     "www.example.com"
                      "type"     "A"
                      "page"     "1"
                      "per_page" "100"}
             :body   nil}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/old-1"
             :query  {}
             :body   nil}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/old-2"
             :query  {}
             :body   nil}
            {:method :post
             :path   "/client/v4/zones/zone-1/dns_records"
             :query  {}
             :body   {:name    "www.example.com"
                      :ttl     300
                      :type    "A"
                      :content "192.0.2.20"}}
            {:method :post
             :path   "/client/v4/zones/zone-1/dns_records"
             :query  {}
             :body   {:name    "www.example.com"
                      :ttl     300
                      :type    "A"
                      :content "192.0.2.21"}}])))

  (behavior "deletes wildcard matches and returns the records that existed"
    (let [calls      (atom [])
          candidates [{:id      "a-1"
                       :name    "host.example.com"
                       :ttl     300
                       :type    "A"
                       :content "192.0.2.1"}
                      {:id      "txt-1"
                       :name    "host.example.com"
                       :ttl     60
                       :type    "TXT"
                       :content "\"hello\""}]
          client     (scripted-client
                      [zone-response
                       (success candidates)
                       (success {:id "a-1"})
                       (success {:id "txt-1"})]
                      calls)
          result     (p53/delete-records!
                      (cloudflare/provider {:api-token   "dns-token"
                                            :http-client client})
                      "example.com."
                      [{:name "host"}]
                      (opts))]

      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "host"
                        :ttl  300
                        :type "A"
                        :data "192.0.2.1"}
                       {:name "host"
                        :ttl  60
                        :type "TXT"
                        :data "hello"}]}}
        "queries by required name and deletes every returned match"
        (mapv #(select-keys % [:method :path :query]) (rest @calls))
        => [{:method :get
             :path   "/client/v4/zones/zone-1/dns_records"
             :query  {"name"     "host.example.com"
                      "page"     "1"
                      "per_page" "100"}}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/a-1"
             :query  {}}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/txt-1"
             :query  {}}])))

  (behavior "matches every specified delete field and treats only omissions as wildcards"
    (let [calls  (atom [])
          client (scripted-client
                  [zone-response
                   (success [{:id      "match"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.1"}
                             {:id      "wrong-data"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.2"}
                             {:id      "wrong-ttl"
                              :name    "www.example.com"
                              :ttl     60
                              :type    "A"
                              :content "192.0.2.1"}])
                   (success [{:id      "empty"
                              :name    "txt.example.com"
                              :ttl     60
                              :type    "TXT"
                              :content "\"\""}
                             {:id      "nonempty"
                              :name    "txt.example.com"
                              :ttl     60
                              :type    "TXT"
                              :content "\"value\""}])
                   (success {:id "match"})
                   (success {:id "empty"})]
                  calls)
          result (p53/delete-records!
                  (cloudflare/provider {:api-token   "dns-token"
                                        :http-client client})
                  "example.com."
                  [{:name "www"
                    :ttl  300
                    :type "A"
                    :data "192.0.2.1"}
                   {:name "txt"
                    :type "TXT"
                    :data ""}]
                  (opts))]

      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "www"
                        :ttl  300
                        :type "A"
                        :data "192.0.2.1"}
                       {:name "txt"
                        :ttl  60
                        :type "TXT"
                        :data ""}]}}
        "deletes only the two exact matches"
        (->> @calls
             (filter #(= :delete (:method %)))
             (mapv :path))
        => ["/client/v4/zones/zone-1/dns_records/match"
            "/client/v4/zones/zone-1/dns_records/empty"])))

  (behavior "discovers every selected RRset before the first set write"
    (let [calls   (atom [])
          desired [{:name "www" :ttl 300 :type "A" :data "192.0.2.1"}
                   {:name "later" :ttl 60 :type "TXT" :data "value"}]
          client  (scripted-client
                   [zone-response
                    (success [])
                    {:status 503 :body "unavailable"}]
                   calls)
          result  (p53/set-records!
                   (cloudflare/provider {:api-token   "dns-token"
                                         :http-client client})
                   "example.com."
                   desired
                   (opts))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed with HTTP 503"
             :operation  :set-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? true}}
        "sends only discovery reads"
        (mapv :method @calls) => [:get :get :get]
        (mapv #(get-in % [:query "name"]) (rest @calls))
        => ["www.example.com" "later.example.com"])))

  (behavior "executes complete set plans in RRset order"
    (let [calls   (atom [])
          desired [{:name "www" :ttl 300 :type "A" :data "192.0.2.20"}
                   {:name "www" :ttl 300 :type "A" :data "192.0.2.21"}
                   {:name "later" :ttl 60 :type "TXT" :data "value"}]
          client  (scripted-client
                   [zone-response
                    (success [{:id      "old-a-1"
                               :name    "www.example.com"
                               :ttl     60
                               :type    "A"
                               :content "192.0.2.1"}
                              {:id      "old-a-2"
                               :name    "www.example.com"
                               :ttl     60
                               :type    "A"
                               :content "192.0.2.2"}])
                    (success [{:id      "old-txt"
                               :name    "later.example.com"
                               :ttl     60
                               :type    "TXT"
                               :content "\"old\""}])
                    (success {:id "old-a-1"})
                    (success {:id "old-a-2"})
                    (success {:id      "new-a-1"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.20"})
                    (success {:id      "new-a-2"
                              :name    "www.example.com"
                              :ttl     300
                              :type    "A"
                              :content "192.0.2.21"})
                    (success {:id "old-txt"})
                    (success {:id      "new-txt"
                              :name    "later.example.com"
                              :ttl     60
                              :type    "TXT"
                              :content "\"value\""})]
                   calls)
          result  (p53/set-records!
                   (cloudflare/provider {:api-token   "dns-token"
                                         :http-client client})
                   "example.com."
                   desired
                   (opts))]

      (assertions
        result => {:ol.protocol53/result {:records desired}}
        "finishes all discovery before each RRset's deletes and creates"
        (mapv #(select-keys % [:method :path]) (drop 3 @calls))
        => [{:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/old-a-1"}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/old-a-2"}
            {:method :post
             :path   "/client/v4/zones/zone-1/dns_records"}
            {:method :post
             :path   "/client/v4/zones/zone-1/dns_records"}
            {:method :delete
             :path   "/client/v4/zones/zone-1/dns_records/old-txt"}
            {:method :post
             :path   "/client/v4/zones/zone-1/dns_records"}]
        (mapv #(get-in % [:query "name"]) (take 2 (rest @calls)))
        => ["www.example.com" "later.example.com"]
        (mapv #(select-keys (:body %) [:name :type :content])
              (filter #(= :post (:method %)) @calls))
        => [{:name "www.example.com" :type "A" :content "192.0.2.20"}
            {:name "www.example.com" :type "A" :content "192.0.2.21"}
            {:name "later.example.com" :type "TXT" :content "\"value\""}])))

  (behavior "validates every discovered record ID before the first set write"
    (let [calls   (atom [])
          desired [{:name "www" :ttl 300 :type "A" :data "192.0.2.1"}
                   {:name "later" :ttl 60 :type "TXT" :data "value"}]
          client  (scripted-client
                   [zone-response
                    (success [{:id      "old-a"
                               :name    "www.example.com"
                               :ttl     60
                               :type    "A"
                               :content "192.0.2.9"}])
                    (success [{:name    "later.example.com"
                               :ttl     60
                               :type    "TXT"
                               :content "\"old\""}])]
                   calls)
          result  (p53/set-records!
                   (cloudflare/provider {:api-token   "dns-token"
                                         :http-client client})
                   "example.com."
                   desired
                   (opts))]

      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-response
             :message    "Cloudflare returned an invalid response"
             :operation  :set-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        (mapv :method @calls) => [:get :get :get])))

  (behavior "does not write when a nonempty delete plan finds no matches"
    (let [calls  (atom [])
          client (scripted-client [zone-response (success [])] calls)
          result (p53/delete-records!
                  (cloudflare/provider {:api-token   "dns-token"
                                        :http-client client})
                  "example.com."
                  [{:name "missing"}]
                  (opts))]

      (assertions
        result => {:ol.protocol53/result {:records []}}
        (mapv :method @calls) => [:get :get])))

  (behavior "keeps a later pre-dispatch deadline failure uncertain"
    (let [calls  (atom [])
          checks (atom 0)
          client (scripted-client
                  [zone-response
                   (success {:id      "new-1"
                             :name    "one.example.com"
                             :ttl     300
                             :type    "A"
                             :content "192.0.2.1"})]
                  calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (fn [_]
                                 (if (< (swap! checks inc) 3)
                                   (Duration/ofSeconds 5)
                                   (Duration/ofNanos 999999)))]
                   (p53/append-records!
                    (cloudflare/provider {:api-token   "dns-token"
                                          :http-client client})
                    "example.com."
                    [{:name "one" :ttl 300 :type "A" :data "192.0.2.1"}
                     {:name "two" :ttl 300 :type "A" :data "192.0.2.2"}]
                    (opts)))]

      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Cloudflare request"
             :operation  :append-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        (mapv :method @calls) => [:get :post])))

  (behavior "marks a write-boundary transport failure uncertain"
    (let [calls  (atom [])
          client (scripted-client
                  [zone-response
                   {:throw (java.io.IOException. "offline")}]
                  calls)
          result (p53/append-records!
                  (cloudflare/provider {:api-token   "dns-token"
                                        :http-client client})
                  "example.com."
                  [{:name "www" :ttl 300 :type "A" :data "192.0.2.1"}]
                  (opts))]

      (assertions
        result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed"
             :operation  :append-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unknown
             :retryable? true}}
        (mapv :method @calls) => [:get :post])))

  (behavior "deduplicates overlapping delete selectors before writing"
    (let [calls      (atom [])
          a-record   {:id      "a-1"
                      :name    "host.example.com"
                      :ttl     300
                      :type    "A"
                      :content "192.0.2.1"}
          txt-record {:id      "txt-1"
                      :name    "host.example.com"
                      :ttl     60
                      :type    "TXT"
                      :content "\"hello\""}
          client     (scripted-client
                      [zone-response
                       (success [a-record])
                       (success [a-record txt-record])
                       (success [a-record txt-record])
                       (success {:id "a-1"})
                       (success {:id "txt-1"})]
                      calls)
          result     (p53/delete-records!
                      (cloudflare/provider {:api-token   "dns-token"
                                            :http-client client})
                      "example.com."
                      [{:name "host" :type "A"}
                       {:name "host"}]
                      (opts))]
      (assertions
        result
        => {:ol.protocol53/result
            {:records [{:name "host"
                        :ttl  300
                        :type "A"
                        :data "192.0.2.1"}
                       {:name "host"
                        :ttl  60
                        :type "TXT"
                        :data "hello"}]}}
        "discovers both selectors before deleting each record once"
        (mapv :method @calls) => [:get :get :get :delete :delete]
        (mapv :path (filter #(= :delete (:method %)) @calls))
        => ["/client/v4/zones/zone-1/dns_records/a-1"
            "/client/v4/zones/zone-1/dns_records/txt-1"])))

  (behavior "keeps a complete plan unchanged when its first write cannot dispatch"
    (let [calls  (atom [])
          checks (atom 0)
          client (scripted-client [zone-response (success [])] calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (fn [_]
                                 (if (< (swap! checks inc) 3)
                                   (Duration/ofSeconds 5)
                                   (Duration/ofNanos 999999)))]
                   (p53/set-records!
                    (cloudflare/provider {:api-token   "dns-token"
                                          :http-client client})
                    "example.com."
                    [{:name "www"
                      :ttl  300
                      :type "A"
                      :data "192.0.2.1"}]
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Cloudflare request"
             :operation  :set-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? false}}
        (mapv :method @calls) => [:get :get])))

  (behavior "short-circuits empty mutations without discovering the zone"
    (let [calls  (atom [])
          client (scripted-client [] calls)
          sut    (cloudflare/provider {:api-token   "dns-token"
                                       :http-client client})
          op     (opts)]

      (assertions
        [(p53/append-records! sut "example.com." [] op)
         (p53/set-records! sut "example.com." [] op)
         (p53/delete-records! sut "example.com." [] op)]
        => (vec (repeat 3 {:ol.protocol53/result {:records []}}))
        @calls => [])))

  (behavior "does not round a sub-millisecond deadline up to a transport timeout"
    (let [calls  (atom [])
          client (scripted-client [(success [])] calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (constantly (Duration/ofNanos 999999))]
                   (p53/list-zones!
                    (cloudflare/provider {:api-token   "dns-token"
                                          :http-client client})
                    (opts)))]
      (assertions
        result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Cloudflare request"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? false}}
        "does not issue a request with a timeout larger than the budget"
        @calls => [])))

  (behavior "classifies provider failures without leaking credentials"
    (let [token           "super-secret-token"
          rate-calls      (atom [])
          service-calls   (atom [])
          malformed-calls (atom [])
          false-calls     (atom [])
          rate-result     (p53/list-zones!
                           (cloudflare/provider
                            {:api-token token
                             :http-client
                             (scripted-client
                              [{:status 429 :body "rate limited"}]
                              rate-calls)})
                           (opts))
          service-result  (p53/list-zones!
                           (cloudflare/provider
                            {:api-token token
                             :http-client
                             (scripted-client
                              [{:status 503 :body "<html>unavailable</html>"}]
                              service-calls)})
                           (opts))
          malformed       (p53/list-zones!
                           (cloudflare/provider
                            {:api-token token
                             :http-client
                             (scripted-client
                              [{:status 200 :body "not-json"}]
                              malformed-calls)})
                           (opts))
          unsuccessful    (p53/list-zones!
                           (cloudflare/provider
                            {:api-token token
                             :http-client
                             (scripted-client
                              [{:status 200
                                :body   {:success false
                                         :errors  []
                                         :result  nil}}]
                              false-calls)})
                           (opts))]

      (assertions
        rate-result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed with HTTP 429"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? true}}
        service-result
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed with HTTP 503"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? true}}
        malformed
        => {:ol.protocol53/error
            {:type       :provider-response
             :message    "Cloudflare returned malformed JSON"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? false}}
        unsuccessful
        => {:ol.protocol53/error
            {:type       :provider-response
             :message    "Cloudflare returned an unsuccessful response"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? false}}
        "does not copy the credential or provider error text into outcomes"
        (boolean
         (some #(str/includes? (pr-str %) token)
               [rate-result service-result malformed unsuccessful])) => false)))

  (behavior "reports whether a failed mutation might have changed the zone"
    (let [token        "dns-token"
          before-calls (atom [])
          after-calls  (atom [])
          before       (p53/append-records!
                        (cloudflare/provider
                         {:api-token token
                          :http-client
                          (scripted-client
                           [{:throw (java.io.IOException. "offline")}]
                           before-calls)})
                        "example.com."
                        [{:name "www"
                          :ttl  300
                          :type "A"
                          :data "192.0.2.1"}]
                        (opts))
          after        (p53/append-records!
                        (cloudflare/provider
                         {:api-token token
                          :http-client
                          (scripted-client
                           [zone-response
                            {:status 403
                             :body   {:success false
                                      :errors  [{:code    9109
                                                 :message "unauthorized"}]
                                      :result  nil}}]
                           after-calls)})
                        "example.com."
                        [{:name "www"
                          :ttl  300
                          :type "A"
                          :data "192.0.2.1"}]
                        (opts))]

      (assertions
        before
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed"
             :operation  :append-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unchanged
             :retryable? true}}
        after
        => {:ol.protocol53/error
            {:type       :provider-request
             :message    "Cloudflare request failed with HTTP 403"
             :operation  :append-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}})))

  (behavior "rechecks the deadline after reads and writes"
    (let [read-calls   (atom [])
          write-calls  (atom [])
          read-result  (p53/list-zones!
                        (cloudflare/provider
                         {:api-token "dns-token"
                          :http-client
                          (scripted-client
                           [(assoc (success []) :delay-ms 1100)]
                           read-calls)})
                        (opts 1))
          write-result (p53/append-records!
                        (cloudflare/provider
                         {:api-token "dns-token"
                          :http-client
                          (scripted-client
                           [zone-response
                            (assoc
                             (success {:id      "a-1"
                                       :name    "www.example.com"
                                       :ttl     300
                                       :type    "A"
                                       :content "192.0.2.1"})
                             :delay-ms 1100)]
                           write-calls)})
                        "example.com."
                        [{:name "www"
                          :ttl  300
                          :type "A"
                          :data "192.0.2.1"}]
                        (opts 1))]

      (assertions
        read-result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Cloudflare request"
             :operation  :list-zones
             :provider   :cloudflare
             :retryable? false}}
        write-result
        => {:ol.protocol53/error
            {:type       :deadline-exceeded
             :message    "Deadline exceeded during Cloudflare request"
             :operation  :append-records
             :provider   :cloudflare
             :zone       "example.com."
             :zone-state :unknown
             :retryable? false}}
        "does not continue after the expired write"
        (count @write-calls) => 2))))
