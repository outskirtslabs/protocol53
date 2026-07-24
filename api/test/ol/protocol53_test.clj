(ns ol.protocol53-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [fulcro-spec.core :refer [=> =throws=> assertions behavior specification]]
   [ol.protocol53 :as p53]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.fixtures :as fixtures]
   [ol.protocol53.protocols :as protocols])
  (:import
   [java.time Duration]))

(defn- operation-opts []
  {:ol.protocol53/deadline (deadline/after (Duration/ofSeconds 5))
   :future-option          true})

(defrecord AllCapabilitiesProvider [calls]
  protocols/RecordGetter
  (-get-records! [_ opts zone]
    (swap! calls conj {:operation :get-records
                       :opts      opts
                       :zone      zone})
    fixtures/record-outcome)

  protocols/RecordAppender
  (-append-records! [_ opts zone records]
    (swap! calls conj {:operation :append-records
                       :opts      opts
                       :zone      zone
                       :records   records})
    fixtures/record-outcome)

  protocols/RecordSetter
  (-set-records! [_ opts zone records]
    (swap! calls conj {:operation :set-records
                       :opts      opts
                       :zone      zone
                       :records   records})
    fixtures/record-outcome)

  protocols/RecordDeleter
  (-delete-records! [_ opts zone records]
    (swap! calls conj {:operation :delete-records
                       :opts      opts
                       :zone      zone
                       :records   records})
    fixtures/record-outcome)

  protocols/ZoneLister
  (-list-zones! [_ opts]
    (swap! calls conj {:operation :list-zones
                       :opts      opts})
    fixtures/zone-outcome))

(defrecord GetterOnlyProvider [calls]
  protocols/RecordGetter
  (-get-records! [_ _opts _zone]
    (swap! calls conj :get-records)
    fixtures/record-outcome))

(defrecord ReturningProvider [outcome]
  protocols/RecordGetter
  (-get-records! [_ _opts _zone]
    outcome))

(defrecord ThrowingProvider [failure]
  protocols/RecordGetter
  (-get-records! [_ _opts _zone]
    (throw failure)))

(specification "Provider protocols"
  (behavior "preserve independent capabilities"
    (let [all-capabilities (map->AllCapabilitiesProvider {:calls (atom [])})
          getter-only      (map->GetterOnlyProvider {:calls (atom [])})]
      (assertions
        "for a provider implementing every protocol"
        (mapv #(satisfies? % all-capabilities)
              [protocols/RecordGetter
               protocols/RecordAppender
               protocols/RecordSetter
               protocols/RecordDeleter
               protocols/ZoneLister])
        => [true true true true true]
        "for a provider implementing only record retrieval"
        (mapv #(satisfies? % getter-only)
              [protocols/RecordGetter
               protocols/RecordAppender
               protocols/RecordSetter
               protocols/RecordDeleter
               protocols/ZoneLister])
        => [true false false false false]))))

(specification "The facade"
  (behavior "dispatches supported operations"
    (let [calls    (atom [])
          provider (map->AllCapabilitiesProvider
                    {:calls                  calls
                     :ol.protocol53/provider :test-dns})
          opts     (operation-opts)]
      (assertions
        "returns provider outcomes"
        [(p53/get-records! provider opts "example.com.")
         (p53/append-records! provider opts "example.com." [fixtures/record])
         (p53/set-records! provider opts "example.com." [fixtures/record])
         (p53/delete-records! provider opts "example.com." [fixtures/selector])
         (p53/list-zones! provider opts)]
        => [fixtures/record-outcome
            fixtures/record-outcome
            fixtures/record-outcome
            fixtures/record-outcome
            fixtures/zone-outcome]
        "passes complete arguments to each provider operation"
        @calls
        => [{:operation :get-records
             :opts      opts
             :zone      "example.com."}
            {:operation :append-records
             :opts      opts
             :zone      "example.com."
             :records   [fixtures/record]}
            {:operation :set-records
             :opts      opts
             :zone      "example.com."
             :records   [fixtures/record]}
            {:operation :delete-records
             :opts      opts
             :zone      "example.com."
             :records   [fixtures/selector]}
            {:operation :list-zones
             :opts      opts}])))

  (behavior "reports unsupported capabilities without dispatch"
    (let [calls     (atom [])
          provider  (map->GetterOnlyProvider
                     {:calls                  calls
                      :ol.protocol53/provider :test-dns})
          anonymous (map->GetterOnlyProvider {:calls calls})
          opts      (operation-opts)]
      (assertions
        [(p53/append-records! provider opts "example.com." [fixtures/record])
         (p53/list-zones! provider opts)
         (p53/list-zones! anonymous opts)]
        => [{:ol.protocol53/error
             {:type       :unsupported-operation
              :message    "Provider does not support append-records"
              :operation  :append-records
              :provider   :test-dns
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :unsupported-operation
              :message    "Provider does not support list-zones"
              :operation  :list-zones
              :provider   :test-dns
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :unsupported-operation
              :message    "Provider does not support list-zones"
              :operation  :list-zones
              :provider   :unknown
              :retryable? false}}]
        "does not invoke the supported getter"
        @calls => [])))

  (behavior "reports invalid input without dispatch"
    (let [calls    (atom [])
          provider (map->AllCapabilitiesProvider
                    {:calls                  calls
                     :ol.protocol53/provider :test-dns})
          opts     (operation-opts)]
      (assertions
        [(p53/get-records! provider {} "example.com.")
         (p53/get-records! provider opts "")
         (p53/append-records! provider opts "example.com." [fixtures/selector])
         (p53/delete-records! provider opts "example.com." [{}])
         (p53/list-zones! provider {})]
        => [{:ol.protocol53/error
             {:type       :invalid-record
              :message    "Invalid operation options"
              :operation  :get-records
              :provider   :test-dns
              :zone       "example.com."
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :invalid-record
              :message    "Invalid zone"
              :operation  :get-records
              :provider   :test-dns
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :invalid-record
              :message    "Invalid records"
              :operation  :append-records
              :provider   :test-dns
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :invalid-record
              :message    "Invalid record selectors"
              :operation  :delete-records
              :provider   :test-dns
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :invalid-record
              :message    "Invalid operation options"
              :operation  :list-zones
              :provider   :test-dns
              :retryable? false}}]
        "does not invoke the provider"
        @calls => [])))

  (behavior "reports expired deadlines without dispatch"
    (let [calls              (atom [])
          provider           (map->AllCapabilitiesProvider
                              {:calls                  calls
                               :ol.protocol53/provider :test-dns})
          operation-deadline (System/nanoTime)
          opts               {:ol.protocol53/deadline operation-deadline}]
      (assertions
        [(p53/get-records! provider opts "example.com.")
         (p53/set-records! provider opts "example.com." [fixtures/record])]
        => [{:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded before provider operation"
              :operation  :get-records
              :provider   :test-dns
              :zone       "example.com."
              :retryable? false}}
            {:ol.protocol53/error
             {:type       :deadline-exceeded
              :message    "Deadline exceeded before provider operation"
              :operation  :set-records
              :provider   :test-dns
              :zone       "example.com."
              :zone-state :unchanged
              :retryable? false}}]
        "does not invoke the provider"
        @calls => [])))

  (behavior "validates provider outcomes"
    (let [opts          (operation-opts)
          valid-error   (map->ReturningProvider
                         {:outcome                fixtures/error-outcome
                          :ol.protocol53/provider :test-dns})
          invalid-value (map->ReturningProvider
                         {:outcome                fixtures/zone-outcome
                          :ol.protocol53/provider :test-dns})
          mixed-value   (map->ReturningProvider
                         {:outcome                fixtures/mixed-outcome
                          :ol.protocol53/provider :test-dns})]
      (assertions
        "returns a valid error outcome"
        (p53/get-records! valid-error opts "example.com.")
        => fixtures/error-outcome
        "rejects the wrong result shape"
        (p53/get-records! invalid-value opts "example.com.")
        =throws=> #"Provider returned an invalid get-records outcome"
        "rejects a mixed result shape"
        (p53/get-records! mixed-value opts "example.com.")
        =throws=> #"Provider returned an invalid get-records outcome")))

  (behavior "preserves programming faults"
    (let [failure  (IllegalStateException. "provider bug")
          provider (map->ThrowingProvider
                    {:failure                failure
                     :ol.protocol53/provider :test-dns})]
      (assertions
        (identical? failure
                    (try
                      (p53/get-records! provider
                                        (operation-opts)
                                        "example.com.")
                      (catch Throwable cause
                        cause)))
        => true)))

  (behavior "has instrumentable public function specs"
    (let [symbols  '[ol.protocol53/get-records!
                     ol.protocol53/append-records!
                     ol.protocol53/set-records!
                     ol.protocol53/delete-records!
                     ol.protocol53/list-zones!]
          provider (map->AllCapabilitiesProvider
                    {:calls                  (atom [])
                     :ol.protocol53/provider :test-dns})
          opts     (operation-opts)]
      (assertions
        "registers all public function specs"
        (filterv s/get-spec symbols) => symbols)
      (try
        (assertions
          "instruments every function"
          (set (stest/instrument symbols)) => (set symbols)
          "accepts valid calls"
          [(p53/get-records! provider opts "example.com.")
           (p53/list-zones! provider opts)]
          => [fixtures/record-outcome fixtures/zone-outcome])
        (finally
          (stest/unstrument symbols))))))
