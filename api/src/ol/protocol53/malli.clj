(ns ol.protocol53.malli
  "Optional Malli schemas for protocol53 inputs and outcomes.

  This namespace requires consumers to supply Malli. Core protocol53
  namespaces do not load it."
  (:require
   [malli.core :as m]))

(def record
  "Schema for a complete portable record."
  (m/schema
   [:map
    [:name [:string {:min 1}]]
    [:ttl [:and :int [:>= 0]]]
    [:type [:string {:min 1}]]
    [:data :string]]))

(def record-selector
  "Schema for a delete selector with wildcard fields."
  (m/schema
   [:map
    [:name [:string {:min 1}]]
    [:ttl {:optional true} [:and :int [:>= 0]]]
    [:type {:optional true} [:string {:min 1}]]
    [:data {:optional true} :string]]))

(def records
  "Schema for a vector of complete records."
  (m/schema [:vector record]))

(def record-selectors
  "Schema for a vector of delete selectors."
  (m/schema [:vector record-selector]))

(def zone
  "Schema for one zone."
  (m/schema [:map [:name [:string {:min 1}]]]))

(def zones
  "Schema for a vector of zones."
  (m/schema [:vector zone]))

(def opts
  "Open operation-options schema requiring a deadline."
  (m/schema
   [:map
    [:ol.protocol53/deadline [:fn #(instance? Long %)]]]))

(defn- no-zones? [x]
  (and (map? x) (not (contains? x :zones))))

(defn- no-records? [x]
  (and (map? x) (not (contains? x :records))))

(def record-result-data
  "Schema for the inner result of a record operation."
  (m/schema [:and [:map [:records records]] [:fn no-zones?]]))

(def zone-result-data
  "Schema for the inner result of zone listing."
  (m/schema [:and [:map [:zones zones]] [:fn no-records?]]))

(def error-data
  "Schema for inner error data."
  (m/schema
   [:map
    [:type [:enum :deadline-exceeded
            :invalid-record
            :unsupported-operation
            :provider-request
            :provider-response
            :conflict]]
    [:message :string]
    [:operation :keyword]
    [:provider :keyword]
    [:retryable? :boolean]
    [:zone {:optional true} [:string {:min 1}]]
    [:zone-state {:optional true} [:enum :unknown :unchanged]]
    [:records {:optional true} records]]))

(defn- exactly-one-outcome-tag? [x]
  (and (map? x)
       (= 1 (count (filter #(contains? x %)
                           [:ol.protocol53/result
                            :ol.protocol53/error])))))

(def record-success
  "Schema for a successful record-operation outcome."
  (m/schema
   [:map [:ol.protocol53/result record-result-data]]))

(def zone-success
  "Schema for a successful zone-listing outcome."
  (m/schema
   [:map [:ol.protocol53/result zone-result-data]]))

(def error-outcome
  "Schema for an error outcome."
  (m/schema
   [:map [:ol.protocol53/error error-data]]))

(def record-outcome
  "Schema for exactly one record result or error."
  (m/schema
   [:and
    [:fn exactly-one-outcome-tag?]
    [:or record-success error-outcome]]))

(def zone-outcome
  "Schema for exactly one zone result or error."
  (m/schema
   [:and
    [:fn exactly-one-outcome-tag?]
    [:or zone-success error-outcome]]))

(def get-records-fn
  "Function schema for `ol.protocol53/get-records!`."
  (m/schema
   [:=> [:cat :some opts [:string {:min 1}]] record-outcome]))

(def append-records-fn
  "Function schema for `ol.protocol53/append-records!`."
  (m/schema
   [:=> [:cat :some opts [:string {:min 1}] records]
    record-outcome]))

(def set-records-fn
  "Function schema for `ol.protocol53/set-records!`."
  append-records-fn)

(def delete-records-fn
  "Function schema for `ol.protocol53/delete-records!`."
  (m/schema
   [:=> [:cat :some opts [:string {:min 1}] record-selectors]
    record-outcome]))

(def list-zones-fn
  "Function schema for `ol.protocol53/list-zones!`."
  (m/schema
   [:=> [:cat :some opts] zone-outcome]))
