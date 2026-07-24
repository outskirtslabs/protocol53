(ns ol.protocol53.fixtures
  "Shared schema-parity fixtures for protocol53 API data."
  (:require
   [ol.protocol53.deadline :as deadline])
  (:import
   [java.time Duration]))

(def timeout
  (Duration/ofSeconds 30))

(def operation-deadline
  (deadline/after timeout))

(def opts
  {:timeout timeout})

(def deadline-opts
  {:deadline operation-deadline})

(def provider-opts
  deadline-opts)

(def record
  {:name "www"
   :ttl  300
   :type "A"
   :data "192.0.2.1"})

(def selector
  {:name "www"})

(def zone
  {:name "example.com."})

(def error-data
  {:type       :deadline-exceeded
   :message    "Deadline exceeded while replacing RRset"
   :operation  :set-records
   :provider   :cloudflare
   :zone       "example.com."
   :zone-state :unknown
   :retryable? false})

(def record-result-data
  {:records [record]})

(def zone-result-data
  {:zones [zone]})

(def error-outcome
  {:ol.protocol53/error error-data})

(def record-outcome
  {:ol.protocol53/result record-result-data})

(def zone-outcome
  {:ol.protocol53/result zone-result-data})

(def mixed-result-data
  (merge record-result-data zone-result-data))

(def mixed-outcome
  {:ol.protocol53/result mixed-result-data})

(def schema-corpus
  [{:schema  :record
    :valid   [record
              (assoc record :provider-metadata "ignored")]
    :invalid [(dissoc record :name)
              (assoc record :name "")
              (assoc record :ttl -1)
              (assoc record :ttl 1.5)
              (assoc record :type "")
              (assoc record :data :not-a-string)]}
   {:schema  :record-selector
    :valid   [selector
              record
              (assoc selector :future-option true)]
    :invalid [{}
              {:name ""}
              {:name "www" :ttl -1}
              {:name "www" :type ""}
              {:name "www" :data :not-a-string}]}
   {:schema  :records
    :valid   [[] [record]]
    :invalid [(list record)
              [selector]
              [record (assoc record :ttl -1)]]}
   {:schema  :record-selectors
    :valid   [[] [selector] [selector record]]
    :invalid [(list selector)
              [{}]
              [(assoc selector :type "")]]}
   {:schema  :zone
    :valid   [zone (assoc zone :provider-id "zone-1")]
    :invalid [{} {:name ""} {:name :example.com}]}
   {:schema  :zones
    :valid   [[] [zone]]
    :invalid [(list zone) [{}] [(assoc zone :name "")]]}
   {:schema  :opts
    :valid   [opts
              deadline-opts
              (assoc opts :future-option true)
              (assoc deadline-opts :future-option true)]
    :invalid [{}
              {:timeout nil}
              {:timeout Duration/ZERO}
              {:timeout (Duration/ofSeconds -1)}
              {:timeout :not-a-duration}
              {:timeout (Duration/ofNanos Long/MAX_VALUE)}
              {:deadline nil}
              {:deadline :not-a-deadline}
              {:timeout timeout :deadline operation-deadline}
              {:ol.protocol53/deadline operation-deadline}]}
   {:schema  :record-result-data
    :valid   [record-result-data
              (assoc record-result-data :provider-metadata true)]
    :invalid [{} zone-result-data mixed-result-data {:records (list record)}]}
   {:schema  :zone-result-data
    :valid   [zone-result-data
              (assoc zone-result-data :provider-metadata true)]
    :invalid [{} record-result-data mixed-result-data {:zones (list zone)}]}
   {:schema  :error-data
    :valid   [error-data
              (-> error-data
                  (dissoc :zone :zone-state)
                  (assoc :records [record]))
              (assoc error-data :provider-diagnostic "request-1")]
    :invalid [(dissoc error-data :message)
              (assoc error-data :type :unknown)
              (assoc error-data :operation "set-records")
              (assoc error-data :provider "cloudflare")
              (assoc error-data :retryable? nil)
              (assoc error-data :zone "")
              (assoc error-data :zone-state :changed)
              (assoc error-data :records [selector])]}
   {:schema  :record-success
    :valid   [record-outcome
              (assoc record-outcome :provider-metadata true)]
    :invalid [{} zone-outcome mixed-outcome error-outcome]}
   {:schema  :zone-success
    :valid   [zone-outcome
              (assoc zone-outcome :provider-metadata true)]
    :invalid [{} record-outcome mixed-outcome error-outcome]}
   {:schema  :error-outcome
    :valid   [error-outcome
              (assoc error-outcome :provider-metadata true)]
    :invalid [{} record-outcome
              {:ol.protocol53/error (assoc error-data :type :unknown)}]}
   {:schema  :record-outcome
    :valid   [record-outcome error-outcome]
    :invalid [{}
              42
              zone-outcome
              mixed-outcome
              (assoc record-outcome :ol.protocol53/error error-data)]}
   {:schema  :zone-outcome
    :valid   [zone-outcome error-outcome]
    :invalid [{}
              42
              record-outcome
              mixed-outcome
              (assoc zone-outcome :ol.protocol53/error error-data)]}])

(def operation-corpus
  [{:operation    :get-records
    :valid-args   [[:provider "example.com." opts]
                   [:provider "example.com." deadline-opts]]
    :invalid-args [[nil "example.com." opts]
                   [:provider "" opts]
                   [:provider "example.com." {}]
                   [:provider opts "example.com."]
                   [:provider "example.com."]]
    :valid-ret    [record-outcome error-outcome]
    :invalid-ret  [zone-outcome mixed-outcome]}
   {:operation    :append-records
    :valid-args   [[:provider "example.com." [] opts]
                   [:provider "example.com." [record] deadline-opts]]
    :invalid-args [[nil "example.com." [record] opts]
                   [:provider "" [record] opts]
                   [:provider "example.com." (list record) opts]
                   [:provider "example.com." [selector] opts]
                   [:provider opts "example.com." [record]]]
    :valid-ret    [record-outcome error-outcome]
    :invalid-ret  [zone-outcome mixed-outcome]}
   {:operation    :set-records
    :valid-args   [[:provider "example.com." [] opts]
                   [:provider "example.com." [record] deadline-opts]]
    :invalid-args [[nil "example.com." [record] opts]
                   [:provider "" [record] opts]
                   [:provider "example.com." (list record) opts]
                   [:provider "example.com." [selector] opts]
                   [:provider opts "example.com." [record]]]
    :valid-ret    [record-outcome error-outcome]
    :invalid-ret  [zone-outcome mixed-outcome]}
   {:operation    :delete-records
    :valid-args   [[:provider "example.com." [] opts]
                   [:provider "example.com." [selector record] deadline-opts]]
    :invalid-args [[nil "example.com." [selector] opts]
                   [:provider "" [selector] opts]
                   [:provider "example.com." (list selector) opts]
                   [:provider "example.com." [{}] opts]
                   [:provider opts "example.com." [selector]]]
    :valid-ret    [record-outcome error-outcome]
    :invalid-ret  [zone-outcome mixed-outcome]}
   {:operation    :list-zones
    :valid-args   [[:provider opts]
                   [:provider deadline-opts]]
    :invalid-args [[nil opts]
                   [:provider {}]
                   [:provider opts "example.com."]]
    :valid-ret    [zone-outcome error-outcome]
    :invalid-ret  [record-outcome mixed-outcome]}])
