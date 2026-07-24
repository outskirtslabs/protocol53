(ns ol.protocol53.schema-parity-test
  (:require
   [clojure.spec.alpha :as s]
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [malli.core :as m]
   [ol.protocol53.fixtures :as fixtures]
   [ol.protocol53.malli :as schemas]
   [ol.protocol53.specs :as specs]))

(def data-schemas
  {:record             {:spec  ::specs/record
                        :malli schemas/record}
   :record-selector    {:spec  ::specs/record-selector
                        :malli schemas/record-selector}
   :records            {:spec  ::specs/records
                        :malli schemas/records}
   :record-selectors   {:spec  ::specs/record-selectors
                        :malli schemas/record-selectors}
   :zone               {:spec  ::specs/zone
                        :malli schemas/zone}
   :zones              {:spec  ::specs/zones
                        :malli schemas/zones}
   :opts               {:spec  ::specs/opts
                        :malli schemas/opts}
   :record-result-data {:spec  ::specs/record-result-data
                        :malli schemas/record-result-data}
   :zone-result-data   {:spec  ::specs/zone-result-data
                        :malli schemas/zone-result-data}
   :error-data         {:spec  ::specs/error-data
                        :malli schemas/error-data}
   :record-success     {:spec  ::specs/record-success
                        :malli schemas/record-success}
   :zone-success       {:spec  ::specs/zone-success
                        :malli schemas/zone-success}
   :error-outcome      {:spec  ::specs/error-outcome
                        :malli schemas/error-outcome}
   :record-outcome     {:spec  ::specs/record-outcome
                        :malli schemas/record-outcome}
   :zone-outcome       {:spec  ::specs/zone-outcome
                        :malli schemas/zone-outcome}})

(def operation-schemas
  {:get-records    {:args  ::specs/get-records-args
                    :ret   ::specs/record-outcome
                    :malli schemas/get-records-fn}
   :append-records {:args  ::specs/append-records-args
                    :ret   ::specs/record-outcome
                    :malli schemas/append-records-fn}
   :set-records    {:args  ::specs/set-records-args
                    :ret   ::specs/record-outcome
                    :malli schemas/set-records-fn}
   :delete-records {:args  ::specs/delete-records-args
                    :ret   ::specs/record-outcome
                    :malli schemas/delete-records-fn}
   :list-zones     {:args  ::specs/list-zones-args
                    :ret   ::specs/zone-outcome
                    :malli schemas/list-zones-fn}})

(specification "Data schemas"
  (doseq [{:keys [schema valid invalid]} fixtures/schema-corpus
          :let [{spec-schema  :spec
                 malli-schema :malli} (get data-schemas schema)]
          [expected values]              [[true valid] [false invalid]]
          value values]
    (behavior (str (name schema) " " (if expected "accepts" "rejects")
                   " " (pr-str value))
      (assertions
        [(s/valid? spec-schema value)
         (m/validate malli-schema value)]
        => [expected expected]))))

(specification "Operation schemas"
  (doseq [{:keys [operation valid-args invalid-args valid-ret invalid-ret]}
          fixtures/operation-corpus
          :let [{:keys [args ret malli]} (get operation-schemas operation)
                [malli-args malli-ret]    (m/children malli)]
          [kind spec-schema malli-schema expected values]
          [[:args args malli-args true valid-args]
           [:args args malli-args false invalid-args]
           [:ret ret malli-ret true valid-ret]
           [:ret ret malli-ret false invalid-ret]]
          value values]
    (behavior (str (name operation) " " (name kind) " "
                   (if expected "accepts" "rejects") " " (pr-str value))
      (assertions
        [(s/valid? spec-schema value)
         (m/validate malli-schema value)]
        => [expected expected]))))

(specification "Result data"
  (behavior "accepts only one documented shape"
    (assertions
      (mapv #(s/valid? :ol.protocol53/result %)
            [fixtures/record-result-data
             fixtures/zone-result-data
             {}
             fixtures/mixed-result-data])
      => [true true false false])))

(specification "Time-budget specs"
  (behavior "publish the timeout and deadline representations"
    (let [timeout-spec  (s/get-spec :ol.protocol53/timeout)
          deadline-spec (s/get-spec :ol.protocol53/deadline)]
      (assertions
        [(some? timeout-spec)
         (some? deadline-spec)
         (if timeout-spec
           (s/valid? timeout-spec fixtures/timeout)
           false)
         (s/valid? deadline-spec fixtures/operation-deadline)]
        => [true true true true]))))

(specification "Normalized provider options"
  (behavior "require an unqualified deadline and reject a timeout"
    (let [spec-schema  (s/get-spec ::specs/provider-opts)
          malli-var    (ns-resolve 'ol.protocol53.malli 'provider-opts)
          malli-schema (some-> malli-var deref)
          validate     (fn [value]
                         (if (and spec-schema malli-schema)
                           [(s/valid? spec-schema value)
                            (m/validate malli-schema value)]
                           [false false]))]
      (assertions
        "publish both schemas"
        [(some? spec-schema) (some? malli-schema)] => [true true]
        "accept normalized open options"
        (validate (assoc fixtures/provider-opts :future-option true))
        => [true true]
        "reject caller timeout options and mixed budgets"
        (mapv validate
              [fixtures/opts
               (assoc fixtures/provider-opts :timeout fixtures/timeout)])
        => [[false false] [false false]]))))
