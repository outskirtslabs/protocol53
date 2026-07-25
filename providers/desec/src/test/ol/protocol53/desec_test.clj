(ns ol.protocol53.desec-test
  (:require
   [babashka.json :as json]
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53 :as p53]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.desec :as desec])
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
     :accept        (get-in request [:headers "Accept"])
     :content-type  (get-in request [:headers "Content-Type"])
     :timeout       (:timeout request)
     :body          (some-> (:body request) json/read-str)}))

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
          (throw (IllegalStateException. "Unexpected deSEC request")))
        (if-let [failure (:throw step)]
          (throw failure)
          {:status  (:status step)
           :headers (:headers step)
           :body    (if (string? (:body step))
                      (:body step)
                      (json/write-str (:body step)))})))))

(defn- domain [name]
  {:created     "2026-01-01T00:00:00Z"
   :minimum_ttl 3600
   :name        name
   :published   "2026-01-01T00:00:00Z"
   :touched     "2026-01-01T00:00:00Z"})

(defn- rrset [domain-name subname ttl type records]
  {:created "2026-01-01T00:00:00Z"
   :domain  domain-name
   :name    (str (when (seq subname) (str subname ".")) domain-name ".")
   :records records
   :subname subname
   :ttl     ttl
   :type    type
   :touched "2026-01-01T00:00:00Z"})

(specification "The deSEC provider"
  (behavior "lists every domain page with token authentication"
    (let [calls  (atom [])
          client (scripted-client
                  [(response
                    200
                    {"Link" (str "<https://desec.io/api/v1/domains/?cursor=>; rel=\"first\", "
                                 "<https://desec.io/api/v1/domains/?cursor=next-page>; rel=\"next\"")}
                    [(domain "example.com")])
                   (response [(domain "example.net")])]
                  calls)
          result (p53/list-zones!
                  (desec/provider {:token       "dns-token"
                                   :http-client client})
                  (opts))]

      (assertions
        result => {:ol.protocol53/result
                   {:zones [{:name "example.com."}
                            {:name "example.net."}]}}
        (mapv #(select-keys % [:method :path :query :authorization :accept])
              @calls)
        => [{:method        :get
             :path          "/api/v1/domains/"
             :query         {"cursor" ""}
             :authorization "Token dns-token"
             :accept        "application/json; charset=utf-8"}
            {:method        :get
             :path          "/api/v1/domains/"
             :query         {"cursor" "next-page"}
             :authorization "Token dns-token"
             :accept        "application/json; charset=utf-8"}]
        "bounds requests by the remaining deadline"
        (every? #(<= 1 (:timeout %) 5000) @calls) => true)))

  (behavior "gets portable records from paginated RRsets"
    (let [calls  (atom [])
          client (scripted-client
                  [(response
                    200
                    {"link" (str "<https://desec.io/api/v1/domains/example.com/rrsets/?cursor=>; rel=\"first\", "
                                 "<https://desec.io/api/v1/domains/example.com/rrsets/?cursor=more>; rel=\"next\"")}
                    [(rrset "example.com" "" 3600 "A" ["192.0.2.1"])
                     (rrset "example.com" "txt" 7200 "TXT"
                            ["\"hello \" \"caf\\195\\169\""])])
                   (response
                    [(rrset "example.com" "svc" 3600 "HTTPS"
                            ["1 svc.example.net. alpn=\"h2,h3\""])])]
                  calls)
          result (p53/get-records!
                  (desec/provider {:token       "dns-token"
                                   :http-client client})
                  "example.com."
                  (opts))]

      (assertions
        result => {:ol.protocol53/result
                   {:records [{:name "@"
                               :ttl  3600
                               :type "A"
                               :data "192.0.2.1"}
                              {:name "txt"
                               :ttl  7200
                               :type "TXT"
                               :data "hello café"}
                              {:name "svc"
                               :ttl  3600
                               :type "HTTPS"
                               :data "1 svc.example.net. alpn=h2,h3"}]}}
        (mapv #(select-keys % [:path :query]) @calls)
        => [{:path  "/api/v1/domains/example.com/rrsets/"
             :query {"cursor" ""}}
            {:path  "/api/v1/domains/example.com/rrsets/"
             :query {"cursor" "more"}}])))

  (behavior "sets complete RRsets atomically and enforces the 3600-second minimum TTL"
    (let [calls   (atom [])
          txt     "a\"b\\é"
          txt-api "\"a\\\"b\\\\\\195\\169\""
          stored  [(rrset "example.com" "www" 3600 "A"
                          ["192.0.2.10" "192.0.2.11"])
                   (rrset "example.com" "long" 7200 "A" ["192.0.2.20"])
                   (rrset "example.com" "txt" 3600 "TXT" [txt-api])]
          client  (scripted-client [(response stored)] calls)
          result  (p53/set-records!
                   (desec/provider {:token       "dns-token"
                                    :http-client client})
                   "example.com."
                   [{:name "WWW" :ttl 300 :type "a" :data "192.0.2.10"}
                    {:name "www" :ttl 8000 :type "A" :data "192.0.2.11"}
                    {:name "long" :ttl 7200 :type "A" :data "192.0.2.20"}
                    {:name "txt" :ttl 0 :type "TXT" :data txt}]
                   (opts))]

      (assertions
        "returns the values as deSEC stored them"
        result => {:ol.protocol53/result
                   {:records [{:name "www" :ttl 3600 :type "A" :data "192.0.2.10"}
                              {:name "www" :ttl 3600 :type "A" :data "192.0.2.11"}
                              {:name "long" :ttl 7200 :type "A" :data "192.0.2.20"}
                              {:name "txt" :ttl 3600 :type "TXT" :data txt}]}}
        "sends one bulk PUT with one entry per case-insensitive RRset"
        (mapv #(select-keys % [:method :path :query :content-type :body]) @calls)
        => [{:method       :put
             :path         "/api/v1/domains/example.com/rrsets/"
             :query        {}
             :content-type "application/json; charset=utf-8"
             :body         [{:subname "www"
                             :type    "A"
                             :ttl     3600
                             :records ["192.0.2.10" "192.0.2.11"]}
                            {:subname "long"
                             :type    "A"
                             :ttl     7200
                             :records ["192.0.2.20"]}
                            {:subname "txt"
                             :type    "TXT"
                             :ttl     3600
                             :records [txt-api]}]}])))

  (behavior "appends only missing values while preserving an existing RRset TTL"
    (let [calls    (atom [])
          existing [(rrset "example.com" "www" 7200 "A" ["192.0.2.1"])]
          stored   [(rrset "example.com" "www" 7200 "A"
                           ["192.0.2.1" "192.0.2.2"])
                    (rrset "example.com" "txt" 3600 "TXT" ["\"hello\""])]
          client   (scripted-client [(response existing) (response stored)] calls)
          result   (p53/append-records!
                    (desec/provider {:token       "dns-token"
                                     :http-client client})
                    "example.com."
                    [{:name "www" :ttl 4000 :type "A" :data "192.0.2.1"}
                     {:name "WWW" :ttl 4000 :type "a" :data "192.0.2.2"}
                     {:name "txt" :ttl 300 :type "TXT" :data "hello"}]
                    (opts))]

      (assertions
        result => {:ol.protocol53/result
                   {:records [{:name "www" :ttl 7200 :type "A" :data "192.0.2.2"}
                              {:name "txt" :ttl 3600 :type "TXT" :data "hello"}]}}
        (mapv #(select-keys % [:method :path :query :body]) @calls)
        => [{:method :get
             :path   "/api/v1/domains/example.com/rrsets/"
             :query  {"cursor" ""}
             :body   nil}
            {:method :put
             :path   "/api/v1/domains/example.com/rrsets/"
             :query  {}
             :body   [{:subname "www"
                       :type    "A"
                       :ttl     7200
                       :records ["192.0.2.1" "192.0.2.2"]}
                      {:subname "txt"
                       :type    "TXT"
                       :ttl     3600
                       :records ["\"hello\""]}]}])))

  (behavior "does not write when every appended value already exists"
    (let [calls  (atom [])
          client (scripted-client
                  [(response [(rrset "example.com" "www" 3600 "A"
                                     ["192.0.2.1"])])]
                  calls)
          result (p53/append-records!
                  (desec/provider {:token       "dns-token"
                                   :http-client client})
                  "example.com."
                  [{:name "www" :ttl 3600 :type "A" :data "192.0.2.1"}]
                  (opts))]

      (assertions
        result => {:ol.protocol53/result {:records []}}
        (mapv :method @calls) => [:get])))

  (behavior "deletes wildcard and exact selector matches in one bulk write"
    (let [calls    (atom [])
          existing [(rrset "example.com" "host" 3600 "A" ["192.0.2.1"])
                    (rrset "example.com" "host" 3600 "TXT"
                           ["\"\"" "\"keep\""])
                    (rrset "example.com" "all" 7200 "A" ["192.0.2.2"])
                    (rrset "example.com" "all" 7200 "TXT" ["\"gone\""])]
          client   (scripted-client [(response existing)
                                     (response
                                      [(rrset "example.com" "host" 3600 "TXT"
                                              ["\"keep\""])])]
                                    calls)
          result   (p53/delete-records!
                    (desec/provider {:token       "dns-token"
                                     :http-client client})
                    "example.com."
                    [{:name "HOST" :type "a"}
                     {:name "host" :type "TXT" :ttl 3600 :data ""}
                     {:name "all"}]
                    (opts))]

      (assertions
        result => {:ol.protocol53/result
                   {:records [{:name "host" :ttl 3600 :type "A" :data "192.0.2.1"}
                              {:name "host" :ttl 3600 :type "TXT" :data ""}
                              {:name "all" :ttl 7200 :type "A" :data "192.0.2.2"}
                              {:name "all" :ttl 7200 :type "TXT" :data "gone"}]}}
        (mapv #(select-keys % [:method :body]) @calls)
        => [{:method :get :body nil}
            {:method :put
             :body   [{:subname "host"
                       :type    "A"
                       :ttl     3600
                       :records []}
                      {:subname "host"
                       :type    "TXT"
                       :ttl     3600
                       :records ["\"keep\""]}
                      {:subname "all"
                       :type    "A"
                       :ttl     7200
                       :records []}
                      {:subname "all"
                       :type    "TXT"
                       :ttl     7200
                       :records []}]}])))

  (behavior "normalizes an exact low-TTL delete selector to deSEC's minimum"
    (let [calls  (atom [])
          client (scripted-client
                  [(response [(rrset "example.com" "www" 3600 "A"
                                     ["192.0.2.1"])])
                   (response [])]
                  calls)
          result (p53/delete-records!
                  (desec/provider {:token "dns-token" :http-client client})
                  "example.com."
                  [{:name "www"
                    :ttl  300
                    :type "A"
                    :data "192.0.2.1"}]
                  (opts))]

      (assertions
        result => {:ol.protocol53/result
                   {:records [{:name "www"
                               :ttl  3600
                               :type "A"
                               :data "192.0.2.1"}]}}
        (mapv #(select-keys % [:method :body]) @calls)
        => [{:method :get :body nil}
            {:method :put
             :body   [{:subname "www"
                       :type    "A"
                       :ttl     3600
                       :records []}]}])))

  (behavior "short-circuits empty mutations"
    (let [calls  (atom [])
          client (scripted-client [] calls)
          sut    (desec/provider {:token "dns-token" :http-client client})
          op     (opts)]

      (assertions
        [(p53/append-records! sut "example.com." [] op)
         (p53/set-records! sut "example.com." [] op)
         (p53/delete-records! sut "example.com." [] op)]
        => (vec (repeat 3 {:ol.protocol53/result {:records []}}))
        @calls => [])))

  (behavior "retries a rate-limited request using Retry-After"
    (let [calls  (atom [])
          client (scripted-client
                  [(response 429 {"Retry-After" "0"} "slow down")
                   (response [(domain "example.com")])]
                  calls)
          result (p53/list-zones!
                  (desec/provider {:token "dns-token" :http-client client})
                  (opts))]

      (assertions
        result => {:ol.protocol53/result {:zones [{:name "example.com."}]}}
        (count @calls) => 2)))

  (behavior "stops a rate-limit wait that cannot fit before the deadline"
    (let [calls  (atom [])
          client (scripted-client
                  [(response 429 {"Retry-After" "5"} "slow down")]
                  calls)
          result (p53/list-zones!
                  (desec/provider {:token "dns-token" :http-client client})
                  (opts 1))]

      (assertions
        result => {:ol.protocol53/error
                   {:type       :deadline-exceeded
                    :message    "Deadline exceeded while waiting for deSEC rate limit"
                    :operation  :list-zones
                    :provider   :desec
                    :retryable? false}}
        (count @calls) => 1)))

  (behavior "classifies transport and response failures without leaking secrets"
    (let [token     "super-secret-token"
          service   (p53/list-zones!
                     (desec/provider
                      {:token token
                       :http-client
                       (scripted-client [(response 503 {} "unavailable")]
                                        (atom []))})
                     (opts))
          malformed (p53/list-zones!
                     (desec/provider
                      {:token token
                       :http-client
                       (scripted-client [(response 200 {} "not-json")]
                                        (atom []))})
                     (opts))
          bad-retry (p53/list-zones!
                     (desec/provider
                      {:token token
                       :http-client
                       (scripted-client
                        [(response 429 {"Retry-After" "soon"} "slow down")]
                        (atom []))})
                     (opts))]

      (assertions
        service => {:ol.protocol53/error
                    {:type       :provider-request
                     :message    "deSEC request failed with HTTP 503"
                     :operation  :list-zones
                     :provider   :desec
                     :retryable? true}}
        malformed => {:ol.protocol53/error
                      {:type       :provider-response
                       :message    "deSEC returned malformed JSON"
                       :operation  :list-zones
                       :provider   :desec
                       :retryable? false}}
        bad-retry => {:ol.protocol53/error
                      {:type       :provider-response
                       :message    "deSEC returned an invalid Retry-After header"
                       :operation  :list-zones
                       :provider   :desec
                       :retryable? false}}
        (some #(str/includes? (pr-str %) token)
              [service malformed bad-retry]) => nil)))

  (behavior "reports whether a failed mutation might have changed the zone"
    (let [record       {:name "www" :ttl 3600 :type "A" :data "192.0.2.1"}
          before-calls (atom [])
          after-calls  (atom [])
          before       (p53/append-records!
                        (desec/provider
                         {:token "dns-token"
                          :http-client
                          (scripted-client
                           [{:throw (java.io.IOException. "offline")}]
                           before-calls)})
                        "example.com."
                        [record]
                        (opts))
          after        (p53/append-records!
                        (desec/provider
                         {:token "dns-token"
                          :http-client
                          (scripted-client
                           [(response []) (response 403 {} "forbidden")]
                           after-calls)})
                        "example.com."
                        [record]
                        (opts))]

      (assertions
        before => {:ol.protocol53/error
                   {:type       :provider-request
                    :message    "deSEC request failed"
                    :operation  :append-records
                    :provider   :desec
                    :zone       "example.com."
                    :zone-state :unchanged
                    :retryable? true}}
        after => {:ol.protocol53/error
                  {:type       :provider-request
                   :message    "deSEC request failed with HTTP 403"
                   :operation  :append-records
                   :provider   :desec
                   :zone       "example.com."
                   :zone-state :unknown
                   :retryable? false}})))

  (behavior "does not issue a request for a sub-millisecond remaining budget"
    (let [calls  (atom [])
          client (scripted-client [(response [])] calls)
          result (with-redefs [deadline/expired? (constantly false)
                               deadline/remaining
                               (constantly (Duration/ofNanos 999999))]
                   (p53/list-zones!
                    (desec/provider {:token "dns-token" :http-client client})
                    (opts)))]

      (assertions
        result => {:ol.protocol53/error
                   {:type       :deadline-exceeded
                    :message    "Deadline exceeded during deSEC request"
                    :operation  :list-zones
                    :provider   :desec
                    :retryable? false}}
        @calls => []))))
