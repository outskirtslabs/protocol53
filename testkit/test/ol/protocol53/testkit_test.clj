(ns ol.protocol53.testkit-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53.testkit.in-memory-provider :as memory])
  (:import
   [java.time Duration]))

(defn- testkit-var [name]
  (requiring-resolve (symbol "ol.protocol53.testkit" name)))

(defn- run-suite! [config]
  ((testkit-var "run!") config))

(defn- cleanup-reserved! [config]
  ((testkit-var "cleanup!") config))

(defn- same-portable-records? [config expected actual]
  ((testkit-var "same-records?") config expected actual))

(defn- exception-message [f]
  (try
    (f)
    :not-thrown
    (catch Throwable cause
      (ex-message cause))))

(defn- captured-reports [f]
  (let [counters (ref test/*initial-report-counters*)
        reports  (atom [])
        thrown   (atom nil)]
    (binding [test/*report-counters* counters]
      (with-redefs [test/do-report
                    (fn [{:keys [type] :as report}]
                      (swap! reports conj report)
                      (when (contains? #{:pass :fail :error} type)
                        (test/inc-report-counter type))
                      nil)]
        (try
          (f)
          (catch Throwable cause
            (reset! thrown cause)))))
    {:counters @counters
     :reports  @reports
     :thrown   @thrown}))

(defn- focused-failure? [{:keys [reports]} pattern]
  (boolean
   (some #(and (= :fail (:type %))
               (string? (:message %))
               (re-find pattern (:message %)))
         reports)))

(defn- mutation-types [calls]
  (->> calls
       (filter #(contains? #{:append-records :set-records :delete-records}
                           (:operation %)))
       (mapcat :records)
       (keep :type)
       (map str/upper-case)
       set))

(defn- first-operation-index [calls operation]
  (first
   (keep-indexed (fn [index call]
                   (when (= operation (:operation call))
                     index))
                 calls)))

(def valid-config
  {:zone                "example.com."
   :allow-live-changes? true})

(specification "The public testkit preflight"
  (behavior "rejects unsafe or malformed configuration before provider access"
    (let [provider         (memory/provider)
          missing-deleter  (memory/provider-missing-deleter)
          cleanup-provider (memory/provider)
          config           (assoc valid-config :provider provider)
          invalid-configs  [(dissoc config :allow-live-changes?)
                            (assoc config :allow-live-changes? false)
                            (dissoc config :zone)
                            (assoc config :zone "example.com")
                            (assoc config :timeout Duration/ZERO)
                            (assoc config :timeout (Duration/ofSeconds -1))
                            (assoc config :timeout "30 seconds")
                            (assoc config :skip-record-types ["AAAA"])
                            (assoc config :skip-record-types #{"a"})
                            (assoc config :skip-record-types #{"Cname"})
                            (assoc config :skip-record-types #{"TXT"})
                            (assoc config :skip-empty-txt? :yes)
                            (assoc config :ignore-ttl? :yes)]
          messages         (mapv #(exception-message
                                   (fn [] (run-suite! %)))
                                 invalid-configs)
          capability-message
          (exception-message
           #(run-suite! (assoc valid-config :provider missing-deleter)))
          credential-error-data
          (try
            (run-suite!
             (assoc valid-config
                    :provider {:api-token "must-not-appear"}))
            (catch Throwable cause
              (ex-data cause)))
          cleanup-message
          (exception-message
           #(cleanup-reserved!
             {:provider            cleanup-provider
              :zone                "example.com."
              :allow-live-changes? false}))]
      (assertions
        "reports each authoring error directly"
        messages
        => [":allow-live-changes? must be true"
            ":allow-live-changes? must be true"
            ":zone must be an absolute name ending in a trailing dot"
            ":zone must be an absolute name ending in a trailing dot"
            ":timeout must be a positive java.time.Duration"
            ":timeout must be a positive java.time.Duration"
            ":timeout must be a positive java.time.Duration"
            ":skip-record-types must be a set of strings"
            "essential record types cannot be skipped: A"
            "essential record types cannot be skipped: CNAME"
            "essential record types cannot be skipped: TXT"
            ":skip-empty-txt? must be a boolean"
            ":ignore-ttl? must be a boolean"]
        capability-message
        => ":provider must satisfy the four record capabilities"
        "does not retain rejected provider configuration"
        credential-error-data
        => {:key :provider}
        cleanup-message
        => ":allow-live-changes? must be true"
        "does not dispatch any operation"
        (mapv (comp empty? memory/calls)
              [provider missing-deleter cleanup-provider])
        => [true true true]))))

(specification "The complete provider lifecycle"
  (let [provider (memory/provider
                  {:metadata? true
                   :records   [{:name "p53test-stale"
                                :ttl  300
                                :type "TXT"
                                :data "from-an-interrupted-run"}]})]
    (run-suite! (assoc valid-config
                       :provider provider
                       :expect-empty-zone? true))
    (let [calls      (memory/calls provider)
          operations (set (map :operation calls))]
      (assertions
        "runs every capability phase through the provider"
        operations
        => #{:get-records
             :append-records
             :set-records
             :delete-records
             :list-zones}
        "cleans stale records before the first mutation"
        (< (first-operation-index calls :delete-records)
           (first-operation-index calls :append-records))
        => true
        "cleans every generated record after the final mutation"
        (memory/zone-records provider)
        => []))))

(specification "A provider without zone listing"
  (let [provider (memory/provider-without-zone-lister)]
    (run-suite! (assoc valid-config :provider provider))
    (assertions
      "runs all record phases without attempting zone listing"
      (set (map :operation (memory/calls provider)))
      => #{:get-records :append-records :set-records :delete-records}
      "finishes cleanly"
      (memory/zone-records provider)
      => [])))

(specification "Broken provider sentinels"
  (doseq [[fault pattern]
          [[:destructive-append #"append preserves the sentinel"]
           [:incomplete-set #"set installs complete RRsets"]
           [:destructive-set #"set preserves unrelated records"]
           [:dishonest-delete #"delete reports the records it removed"]
           [:no-op-delete #"cleanup removes reserved records"]]]
    (behavior (str "detect " (name fault))
      (let [provider (memory/provider {:fault fault})
            _        (when (= :no-op-delete fault)
                       (memory/seed!
                        provider
                        [{:name "p53test-stale"
                          :ttl  300
                          :type "TXT"
                          :data "must-be-cleaned"}]))
            captured (captured-reports
                      #(run-suite! (assoc valid-config
                                          :provider provider)))]
        (assertions
          "as a focused lifecycle assertion failure"
          [(:thrown captured)
           (get-in captured [:counters :error])
           (pos? (get-in captured [:counters :fail]))
           (focused-failure? captured pattern)]
          => [nil 0 true true])))))

(specification "Reserved-record cleanup"
  (let [unrelated {:name "keep-p53test-unrelated"
                   :ttl  300
                   :type "A"
                   :data "192.0.2.44"}
        provider  (memory/provider
                   {:records [{:name "p53test-stale"
                               :ttl  300
                               :type "A"
                               :data "192.0.2.1"}
                              {:name "_service._tcp.P53TEST-stale-srv"
                               :ttl  300
                               :type "SRV"
                               :data "10 20 443 service.example.com."}
                              unrelated]})]
    (cleanup-reserved! (assoc valid-config :provider provider))
    (assertions
      "removes only names owned by the suite, including service prefixes"
      (memory/zone-records provider)
      => [unrelated])))

(specification "Record-type skipping"
  (let [provider (memory/provider)]
    (run-suite!
     (assoc valid-config
            :provider provider
            :skip-record-types #{"aaaa" "CAA" "https" "mx"
                                 "NS" "srv" "SVCB"}))
    (assertions
      "omits only nonessential fixture types from every mutation phase"
      (mutation-types (memory/calls provider))
      => #{"A" "CNAME" "TXT"}
      "still runs every record mutation"
      (set (map :operation (memory/calls provider)))
      => #{:get-records
           :append-records
           :set-records
           :delete-records
           :list-zones})))

(specification "Empty TXT fixture skipping"
  (let [provider (memory/provider)]
    (run-suite!
     (assoc valid-config
            :provider provider
            :skip-empty-txt? true))
    (let [calls (memory/calls provider)]
      (assertions
        "never sends an empty TXT value to the provider"
        (->> calls
             (mapcat :records)
             (some #(and (= "TXT" (str/upper-case (:type %)))
                         (contains? % :data)
                         (empty? (:data %))))
             boolean)
        => false
        "still exercises omission-based wildcard TXT deletion"
        (boolean
         (some #(and (= :delete-records (:operation %))
                     (= 1 (count (:records %)))
                     (= "TXT" (some-> % :records first :type str/upper-case))
                     (not (contains? (first (:records %)) :data)))
               calls))
        => true
        "finishes cleanly"
        (memory/zone-records provider) => []))))

(specification "Portable record comparison"
  (let [metadata-provider  (memory/provider {:metadata? true})
        duplicate-provider (memory/provider
                            {:fault :duplicate-append-return})
        metadata-result    (captured-reports
                            #(run-suite!
                              (assoc valid-config
                                     :provider metadata-provider)))
        duplicate-result   (captured-reports
                            #(run-suite!
                              (assoc valid-config
                                     :provider duplicate-provider)))]
    (assertions
      "ignores provider extension keys"
      [(:thrown metadata-result)
       (select-keys (:counters metadata-result) [:fail :error])]
      => [nil {:fail 0 :error 0}]
      "retains duplicate frequencies rather than collapsing to sets"
      [(:thrown duplicate-result)
       (focused-failure? duplicate-result
                         #"append returns the requested record multiset")]
      => [nil true]))
  (behavior "compares TTLs unless the provider opts out"
    (let [requested {:name "www" :ttl 300 :type "A" :data "192.0.2.1"}
          clamped   (assoc requested :ttl 3600)]
      (assertions
        "compares TTLs by default"
        (same-portable-records? valid-config [requested] [clamped]) => false
        "ignores provider-normalized TTLs when configured"
        (same-portable-records? (assoc valid-config :ignore-ttl? true)
                                [requested]
                                [clamped])
        => true))))

(specification "Provider error reporting"
  (let [provider (memory/provider {:fault :get-error})
        captured (captured-reports
                  #(run-suite! (assoc valid-config :provider provider)))]
    (assertions
      "reports the safe outcome and stops after failed initial cleanup"
      [(:thrown captured)
       (get-in captured [:counters :error])
       (focused-failure?
        captured
        #"cleanup get-records failed.*Deliberate in-memory provider failure")
       (mapv :operation (memory/calls provider))]
      => [nil 0 true [:get-records]])))

(specification "Strict-zone checking"
  (let [records  [{:name "unrelated"
                   :ttl  300
                   :type "A"
                   :data "192.0.2.9"}
                  {:name "@"
                   :ttl  300
                   :type "NS"
                   :data "ns1.example.com."}]
        provider (memory/provider {:records records})
        captured (captured-reports
                  #(run-suite!
                    (assoc valid-config
                           :provider provider
                           :expect-empty-zone? true)))]
    (assertions
      "rejects active non-NS records without deleting unrelated data"
      [(:thrown captured)
       (focused-failure? captured
                         #"strict zone contains active records")
       (memory/zone-records provider)]
      => [nil true records])))
