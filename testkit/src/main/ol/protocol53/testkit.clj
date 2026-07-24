(ns ol.protocol53.testkit
  "Provider-agnostic lifecycle checks for protocol53 implementations.

  This suite creates, replaces, and deletes DNS records. Run it only against a
  dedicated disposable test zone. Generated owners use the reserved
  `p53test-` prefix so interrupted runs can be cleaned safely."
  (:refer-clojure :exclude [run!])
  (:require
   [clojure.string :as str]
   [clojure.test :as test]
   [ol.protocol53 :as p53]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.time Duration]))

(def ^:private default-timeout (Duration/ofSeconds 30))
(def ^:private essential-record-types #{"A" "CNAME" "TXT"})
(def ^:private active-record-types
  #{"A" "AAAA" "CAA" "CNAME" "HTTPS" "MX" "NS" "SRV" "SVCB" "TXT"})
(def ^:private strict-record-types (disj active-record-types "NS"))

(defn- positive-duration? [duration]
  (and (instance? Duration duration)
       (not (.isZero ^Duration duration))
       (not (.isNegative ^Duration duration))
       (try
         (.toNanos ^Duration duration)
         true
         (catch ArithmeticException _
           false))))

(defn- invalid-config! [message key _value]
  (throw (ex-info message {:key key})))

(defn- normalized-config [config]
  (let [ignore-ttl?     (get config :ignore-ttl? false)
        skip-empty-txt? (get config :skip-empty-txt? false)
        timeout         (get config :timeout default-timeout)
        skipped         (get config :skip-record-types #{})]
    (when-not (true? (:allow-live-changes? config))
      (invalid-config! ":allow-live-changes? must be true"
                       :allow-live-changes? (:allow-live-changes? config)))
    (when-not (and (string? (:zone config))
                   (seq (:zone config))
                   (str/ends-with? (:zone config) "."))
      (invalid-config! ":zone must be an absolute name ending in a trailing dot"
                       :zone (:zone config)))
    (when-not (positive-duration? timeout)
      (invalid-config! ":timeout must be a positive java.time.Duration"
                       :timeout timeout))
    (when-not (and (set? skipped) (every? string? skipped))
      (invalid-config! ":skip-record-types must be a set of strings"
                       :skip-record-types skipped))
    (when-not (boolean? skip-empty-txt?)
      (invalid-config! ":skip-empty-txt? must be a boolean"
                       :skip-empty-txt? skip-empty-txt?))
    (when-not (boolean? ignore-ttl?)
      (invalid-config! ":ignore-ttl? must be a boolean"
                       :ignore-ttl? ignore-ttl?))
    (let [skipped    (set (map str/upper-case skipped))
          essentials (sort (filter skipped essential-record-types))]
      (when-let [record-type (first essentials)]
        (invalid-config!
         (str "essential record types cannot be skipped: " record-type)
         :skip-record-types skipped))
      (when-not (every? #(satisfies? % (:provider config))
                        [protocols/RecordGetter
                         protocols/RecordAppender
                         protocols/RecordSetter
                         protocols/RecordDeleter])
        (invalid-config! ":provider must satisfy the four record capabilities"
                         :provider (:provider config)))
      (assoc config
             :expect-empty-zone? (true? (:expect-empty-zone? config))
             :ignore-ttl?        ignore-ttl?
             :skip-empty-txt?    skip-empty-txt?
             :skip-record-types  skipped
             :timeout            timeout))))

(defn- operation-opts [{:keys [timeout]}]
  {:timeout timeout})

(defn- run-prefix []
  (str "p53test-" (subs (str (random-uuid)) 0 8)))

(defn- record [name ttl type data]
  {:name name :ttl ttl :type type :data data})

(defn- append-corpus [prefix]
  [(record (str prefix "-append-address") 300 "A" "192.0.2.1")
   (record (str prefix "-append-address") 300 "A" "192.0.2.2")
   (record (str prefix "-append-address-v6") 300 "AAAA" "2001:db8::1")
   (record (str prefix "-append-caa") 300 "CAA"
           "0 issue \"letsencrypt.org\"")
   (record (str prefix "-append-cname") 300 "CNAME" "example.com.")
   (record (str prefix "-append-https") 300 "HTTPS"
           "1 svc.example.net. alpn=h2,h3")
   (record (str prefix "-append-mx") 300 "MX" "10 mx.example.com.")
   (record (str prefix "-append-ns") 300 "NS" "ns1.example.com.")
   (record (str "_exampleservice._tcp." prefix "-append-srv")
           300
           "SRV"
           "10 20 443 service.example.com.")
   (record (str prefix "-append-svcb") 300 "SVCB" "1 . alpn=dot")
   (record (str prefix "-append-txt") 300 "TXT" "Hello, world!")])

(defn- set-corpora [prefix]
  (let [address (str prefix "-set-address")]
    {:preserved (record (str prefix "-set-preserve")
                        300
                        "TXT"
                        "should-not-change")
     :initial   [(record address 300 "A" "192.0.2.1")
                 (record (str/upper-case address) 300 "a" "192.0.2.2")
                 (record (str prefix "-set-caa") 300 "CAA"
                         "128 issue \"initial.example.com\"")
                 (record (str prefix "-set-cname") 300 "CNAME"
                         "initial.example.com.")
                 (record (str prefix "-set-mx") 300 "MX"
                         "10 initial-mx.example.com.")
                 (record (str prefix "-set-txt") 300 "TXT" "initial value")]
     :updated   [(record address 600 "A" "192.0.2.3")
                 (record (str prefix "-set-caa") 600 "CAA"
                         "0 issue \"updated.example.com\"")
                 (record (str prefix "-set-cname") 600 "CNAME"
                         "updated.example.com.")
                 (record (str prefix "-set-mx") 600 "MX"
                         "20 updated-mx.example.com.")
                 (record (str prefix "-set-txt") 600 "TXT" "updated value")
                 (record (str "_newservice._tcp." prefix "-set-srv")
                         600
                         "SRV"
                         "5 10 80 updated.example.com.")]}))

(defn- delete-corpus [prefix zone]
  [(record (str prefix "-delete-address") 300 "A" "192.0.2.1")
   (record (str prefix "-delete-address-v6") 300 "AAAA" "2001:db8::1")
   (record (str prefix "-delete-caa") 300 "CAA"
           "0 issue \"ca.example.com\"")
   (record (str prefix "-delete-cname") 300 "CNAME"
           (str prefix "-delete-target." zone))
   (record (str prefix "-delete-https") 300 "HTTPS"
           "1 svc.example.net. alpn=h2,h3")
   (record (str prefix "-delete-mx") 300 "MX"
           (str "10 " prefix "-delete-mx-target." zone))
   (record (str prefix "-delete-ns") 300 "NS"
           (str prefix "-delete-ns-target." zone))
   (record (str "_service._tcp." prefix "-delete-srv")
           300
           "SRV"
           (str "10 20 80 " prefix "-delete-srv-target." zone))
   (record (str prefix "-delete-svcb") 300 "SVCB" "1 . alpn=dot")
   (record (str prefix "-delete-txt") 300 "TXT" "delete value")])

(defn- wildcard-corpus [prefix]
  (let [owner (str prefix "-wildcard-delete")]
    [(record owner 300 "A" "192.0.2.10")
     (record owner 300 "TXT" "")
     (record owner 300 "TXT" "wildcard value")]))

(defn- filtered-corpus [config records]
  (filterv #(not (contains? (:skip-record-types config)
                            (str/upper-case (:type %))))
           records))

(defn- portable-record [record]
  (select-keys record [:name :ttl :type :data]))

(defn- normalized-record [config record]
  (cond-> (portable-record record)
    (:ignore-ttl? config) (dissoc :ttl)
    true (update :name str/lower-case)
    true (update :type str/upper-case)))

(defn- record-frequencies [config records]
  (frequencies (map #(normalized-record config %) records)))

(defn- same-records? [config expected actual]
  (= (record-frequencies config expected)
     (record-frequencies config actual)))

(defn- contains-records? [config actual expected]
  (let [actual-counts   (record-frequencies config actual)
        expected-counts (record-frequencies config expected)]
    (every? (fn [[record expected-count]]
              (<= expected-count (get actual-counts record 0)))
            expected-counts)))

(defn- rrset-key [record]
  [(str/lower-case (:name record))
   (str/upper-case (:type record))])

(defn- records-in-rrset [records target]
  (filterv #(= (rrset-key target) (rrset-key %)) records))

(defn- reserved-record? [{:keys [name]}]
  (let [effective-label (->> (str/split name #"\.")
                             (drop-while #(str/starts-with? % "_"))
                             first)]
    (and effective-label
         (str/starts-with? (str/lower-case effective-label) "p53test-"))))

(defn- outcome-value [label result-key outcome]
  (if-let [_error (:ol.protocol53/error outcome)]
    (do
      (test/is false (str label " failed: " (pr-str outcome)))
      nil)
    (get-in outcome [:ol.protocol53/result result-key])))

(defn- get-records [config label]
  (outcome-value label
                 :records (p53/get-records! (:provider config)
                                            (:zone config)
                                            (operation-opts config))))

(defn- append-records [config label records]
  (outcome-value label
                 :records (p53/append-records! (:provider config)
                                               (:zone config)
                                               records
                                               (operation-opts config))))

(defn- set-records [config label records]
  (outcome-value label
                 :records (p53/set-records! (:provider config)
                                            (:zone config)
                                            records
                                            (operation-opts config))))

(defn- delete-records [config label records]
  (outcome-value label
                 :records (p53/delete-records! (:provider config)
                                               (:zone config)
                                               records
                                               (operation-opts config))))

(defn- cleanup* [config]
  (if-let [records (get-records config "cleanup get-records")]
    (let [reserved (filterv reserved-record? records)]
      (if (empty? reserved)
        true
        (if-let [_deleted (delete-records config
                                          "cleanup delete-records"
                                          (mapv portable-record reserved))]
          (if-let [remaining (get-records config
                                          "cleanup verification get-records")]
            (let [survivors (filterv reserved-record? remaining)]
              (test/is (empty? survivors)
                       (str "cleanup removes reserved records; survivors: "
                            (pr-str survivors)))
              (empty? survivors))
            false)
          false)))
    false))

(defn cleanup!
  "Removes reserved lifecycle records and verifies their absence.

  Use this after an interrupted run. The function changes DNS and rejects the
  call unless `:allow-live-changes?` is exactly `true`.

  Options:

  | key                    | description                                                          |
  |------------------------|----------------------------------------------------------------------|
  | `:provider`            | Provider implementing all four record capabilities (required).       |
  | `:zone`                | Dedicated absolute zone name ending in `.` (required).               |
  | `:allow-live-changes?` | Destructive-operation acknowledgement; must be `true` (required).    |
  | `:timeout`             | Positive [[java.time.Duration]] for each call (default 30 seconds).  |
  | `:skip-record-types`   | Set of nonessential uppercase or lowercase types (default `#{}`).    |
  | `:skip-empty-txt?`     | Omit the exact-empty TXT fixture when unsupported (default `false`). |"
  [config]
  (cleanup* (normalized-config config)))

(defn- records-absent? [config actual unexpected]
  (let [actual-counts (record-frequencies config actual)]
    (every? #(zero? (get actual-counts % 0))
            (keys (record-frequencies config unexpected)))))

(defn- list-zones-phase! [config]
  (when (satisfies? protocols/ZoneLister (:provider config))
    (when-let [zones (outcome-value "list-zones"
                                    :zones (p53/list-zones!
                                            (:provider config)
                                            (operation-opts config)))]
      (doseq [zone zones]
        (test/is (and (string? (:name zone)) (seq (:name zone)))
                 (str "list-zones returns complete zones: " (pr-str zone))))
      (test/is (contains? (set (map :name zones)) (:zone config))
               (str "list-zones includes the configured zone "
                    (pr-str (:zone config)))))))

(defn- get-records-phase! [config]
  (when-let [records (get-records config "get-records")]
    (doseq [record records]
      (test/is (and (string? (:name record)) (seq (:name record)))
               (str "get-records returns a nonempty owner: "
                    (pr-str record)))
      (test/is (and (string? (:type record)) (seq (:type record)))
               (str "get-records returns a nonempty type: "
                    (pr-str record))))))

(defn- append-phase! [config prefix]
  (let [sentinel (record (str prefix "-append-preserve")
                         300
                         "TXT"
                         "should-survive-append")
        target   (filtered-corpus config (append-corpus prefix))]
    (when-let [appended-sentinel
               (append-records config "append sentinel" [sentinel])]
      (test/is (same-records? config [sentinel] appended-sentinel)
               "append returns the requested sentinel"))
    (when-let [appended (append-records config "append records" target)]
      (test/is (same-records? config target appended)
               "append returns the requested record multiset")
      (when-let [current (get-records config "append verification get-records")]
        (test/is (contains-records? config current target)
                 "append stores every requested record")
        (test/is (contains-records? config current [sentinel])
                 "append preserves the sentinel")))))

(defn- set-phase! [config prefix]
  (let [{:keys [preserved initial updated]} (set-corpora prefix)
        initial (filtered-corpus config initial)
        updated (filtered-corpus config updated)
        initial-a (filterv #(= "A" (str/upper-case (:type %))) initial)
        updated-a (filterv #(= "A" (str/upper-case (:type %))) updated)]
    (when-let [appended (append-records config
                                        "set preserved record"
                                        [preserved])]
      (test/is (same-records? config [preserved] appended)
               "set setup appends the preserved record"))
    (when-let [set-initial (set-records config "set initial records" initial)]
      (test/is (same-records? config initial set-initial)
               "set returns the requested initial record multiset")
      (when-let [current (get-records config
                                      "set initial verification get-records")]
        (test/is (contains-records? config current initial)
                 "set installs complete RRsets")
        (test/is (contains-records? config current [preserved])
                 "set preserves unrelated records")))
    (when-let [set-updated (set-records config "set updated records" updated)]
      (test/is (same-records? config updated set-updated)
               "set returns the requested updated record multiset")
      (when-let [current (get-records config
                                      "set updated verification get-records")]
        (test/is (contains-records? config current updated)
                 "set stores every updated record")
        (test/is (contains-records? config current [preserved])
                 "set preserves unrelated records after an update")
        (test/is (same-records?
                  config
                  updated-a
                  (records-in-rrset current (first updated-a)))
                 (str "set replaces complete RRsets and removes old members: "
                      (pr-str initial-a)))))))

(defn- delete-phase! [config prefix]
  (let [target (filtered-corpus config
                                (delete-corpus prefix (:zone config)))]
    (when-let [created (append-records config "delete setup append" target)]
      (test/is (same-records? config target created)
               "delete setup returns every created record")
      (when-let [deleted (delete-records config "delete records" created)]
        (test/is (same-records? config created deleted)
                 "delete reports the records it removed")
        (when-let [current (get-records config
                                        "delete verification get-records")]
          (test/is (records-absent? config current created)
                   "delete removes every selected record"))))
    (when-let [deleted-miss
               (delete-records
                config
                "delete missing record"
                [(record (str prefix "-delete-missing")
                         300
                         "A"
                         "192.0.2.99")])]
      (test/is (empty? deleted-miss)
               "delete reports an empty result for a selector miss"))))

(defn- wildcard-delete-phase! [config prefix]
  (let [target       (filterv #(not (and (:skip-empty-txt? config)
                                         (= "TXT" (:type %))
                                         (empty? (:data %))))
                              (wildcard-corpus prefix))
        address      (first target)
        txt-records  (filterv #(= "TXT" (:type %)) target)
        empty-txt    (first (filter #(empty? (:data %)) txt-records))
        nonempty-txt (first (filter #(seq (:data %)) txt-records))
        owner        (:name address)]
    (when-let [created (append-records config
                                       "wildcard delete setup append"
                                       target)]
      (test/is (same-records? config target created)
               "wildcard delete setup returns every created record"))
    (when-let [deleted (delete-records config
                                       "wildcard TXT delete"
                                       [{:name owner :type "TXT"}])]
      (test/is (same-records? config txt-records deleted)
               "an omitted data selector deletes every TXT value"))
    (when-let [current (get-records config
                                    "wildcard TXT verification get-records")]
      (test/is (contains-records? config current [address])
               "wildcard TXT deletion preserves another record type")
      (test/is (records-absent? config current txt-records)
               "wildcard TXT deletion removes every TXT value"))
    (when-not (:skip-empty-txt? config)
      (when-let [recreated (append-records config
                                           "exact empty TXT setup append"
                                           txt-records)]
        (test/is (same-records? config txt-records recreated)
                 "exact empty TXT setup recreates both values"))
      (when-let [deleted-empty
                 (delete-records config
                                 "exact empty TXT delete"
                                 [{:name owner :type "TXT" :data ""}])]
        (test/is (same-records? config [empty-txt] deleted-empty)
                 "an explicit empty data selector deletes only empty TXT"))
      (when-let [current (get-records config
                                      "exact empty TXT verification get-records")]
        (test/is (contains-records? config current [address nonempty-txt])
                 "exact empty TXT deletion preserves other values")
        (test/is (records-absent? config current [empty-txt])
                 "exact empty TXT deletion removes the empty value")))))

(defn- strict-zone-phase! [config]
  (when-let [records (get-records config "strict zone get-records")]
    (let [unexpected (filterv #(contains? strict-record-types
                                          (str/upper-case (:type %)))
                              records)]
      (test/is (empty? unexpected)
               (str "strict zone contains active records: "
                    (pr-str unexpected))))))

(defn- mutation-phase! [config phase]
  (let [clean? (volatile! false)]
    (try
      (phase)
      (finally
        (vreset! clean? (cleanup* config))))
    @clean?))

(defn run!
  "Runs the provider lifecycle suite against a dedicated DNS zone.

  The suite validates configuration, cleans stale `p53test-` records, and then
  runs list, get, append, set, delete, wildcard-delete, and final cleanup
  checks sequentially. It emits [[clojure.test]] assertions in the active test.

  Options:

  | key                    | description                                                          |
  |------------------------|----------------------------------------------------------------------|
  | `:provider`            | Provider implementing all four record capabilities (required).       |
  | `:zone`                | Dedicated absolute zone name ending in `.` (required).               |
  | `:allow-live-changes?` | Destructive-operation acknowledgement; must be `true` (required).    |
  | `:timeout`             | Positive [[java.time.Duration]] for each call (default 30 seconds).  |
  | `:skip-record-types`   | Set of nonessential uppercase or lowercase types (default `#{}`).    |
  | `:skip-empty-txt?`     | Omit the exact-empty TXT fixture when unsupported (default `false`). |
  | `:ignore-ttl?`         | Ignore TTL differences in lifecycle checks (default `false`).        |
  | `:expect-empty-zone?`  | Reject remaining active non-`NS` records (default `false`).          |
  "
  [config]
  (let [config (normalized-config config)
        prefix (run-prefix)]
    (when (cleanup* config)
      (list-zones-phase! config)
      (get-records-phase! config)
      (when (and (mutation-phase! config #(append-phase! config prefix))
                 (mutation-phase! config #(set-phase! config prefix))
                 (mutation-phase! config #(delete-phase! config prefix))
                 (mutation-phase! config
                                  #(wildcard-delete-phase! config prefix)))
        (when (:expect-empty-zone? config)
          (strict-zone-phase! config)))))
  nil)
