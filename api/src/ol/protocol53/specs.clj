(ns ol.protocol53.specs
  "Specifications for protocol53 inputs and outcomes."
  (:require
   [clojure.spec.alpha :as s]
   [ol.protocol53.deadline :as deadline])
  (:import
   [java.time Duration]))

(defn- positive-duration? [duration]
  (and (instance? Duration duration)
       (let [^Duration duration duration]
         (and (not (.isZero duration))
              (not (.isNegative duration))
              (try
                (< (.toNanos duration) Long/MAX_VALUE)
                (catch ArithmeticException _
                  false))))))

(defn- non-negative-duration? [duration]
  (and (instance? Duration duration)
       (not (.isNegative ^Duration duration))))

(s/def ::positive-duration positive-duration?)
(s/def ::non-negative-duration non-negative-duration?)

(s/def :ol.protocol53.record/name (s/and string? not-empty))
(s/def :ol.protocol53.record/ttl nat-int?)
(s/def :ol.protocol53.record/type (s/and string? not-empty))
(s/def :ol.protocol53.record/data string?)

(s/def ::record
  (s/keys :req-un [:ol.protocol53.record/name
                   :ol.protocol53.record/ttl
                   :ol.protocol53.record/type
                   :ol.protocol53.record/data]))

(s/def ::record-selector
  (s/keys :req-un [:ol.protocol53.record/name]
          :opt-un [:ol.protocol53.record/ttl
                   :ol.protocol53.record/type
                   :ol.protocol53.record/data]))

(s/def ::records (s/coll-of ::record :kind vector?))
(s/def ::record-selectors
  (s/coll-of ::record-selector :kind vector?))

(s/def ::zone-name (s/and string? not-empty))
(s/def :ol.protocol53.zone/name ::zone-name)
(s/def ::zone
  (s/keys :req-un [:ol.protocol53.zone/name]))
(s/def ::zones (s/coll-of ::zone :kind vector?))

(s/def :ol.protocol53/deadline #(instance? Long %))
(s/def ::opts
  (s/keys :req [:ol.protocol53/deadline]))

(s/def :ol.protocol53.result/records ::records)
(s/def :ol.protocol53.result/zones ::zones)
(s/def ::record-result-data
  (s/and (s/keys :req-un [:ol.protocol53.result/records])
         #(not (contains? % :zones))))
(s/def ::zone-result-data
  (s/and (s/keys :req-un [:ol.protocol53.result/zones])
         #(not (contains? % :records))))
(s/def ::result-data
  (s/or :records ::record-result-data
        :zones ::zone-result-data))
(s/def :ol.protocol53/result ::result-data)

(s/def :ol.protocol53.error/type
  #{:deadline-exceeded
    :invalid-record
    :unsupported-operation
    :provider-request
    :provider-response
    :conflict})
(s/def :ol.protocol53.error/message string?)
(s/def :ol.protocol53.error/operation keyword?)
(s/def :ol.protocol53.error/provider keyword?)
(s/def :ol.protocol53.error/retryable? boolean?)
(s/def :ol.protocol53.error/zone ::zone-name)
(s/def :ol.protocol53.error/zone-state #{:unknown :unchanged})
(s/def :ol.protocol53.error/records ::records)

(s/def ::error-data
  (s/keys :req-un [:ol.protocol53.error/type
                   :ol.protocol53.error/message
                   :ol.protocol53.error/operation
                   :ol.protocol53.error/provider
                   :ol.protocol53.error/retryable?]
          :opt-un [:ol.protocol53.error/zone
                   :ol.protocol53.error/zone-state
                   :ol.protocol53.error/records]))
(s/def :ol.protocol53/error ::error-data)

(defn- exactly-one-outcome-tag? [x]
  (and (map? x)
       (= 1 (count (filter #(contains? x %)
                           [:ol.protocol53/result
                            :ol.protocol53/error])))))

(s/def ::record-success
  (s/and
   #(s/valid? ::record-result-data (:ol.protocol53/result %))
   (s/keys :req [:ol.protocol53/result])))
(s/def ::zone-success
  (s/and
   #(s/valid? ::zone-result-data (:ol.protocol53/result %))
   (s/keys :req [:ol.protocol53/result])))
(s/def ::error-outcome
  (s/keys :req [:ol.protocol53/error]))
(s/def ::record-outcome
  (s/and exactly-one-outcome-tag?
         (s/or :result ::record-success
               :error ::error-outcome)))
(s/def ::zone-outcome
  (s/and exactly-one-outcome-tag?
         (s/or :result ::zone-success
               :error ::error-outcome)))

(s/def ::provider some?)
(s/def ::get-records-args
  (s/cat :provider ::provider :opts ::opts :zone ::zone-name))
(s/def ::append-records-args
  (s/cat :provider ::provider :opts ::opts :zone ::zone-name
         :records ::records))
(s/def ::set-records-args ::append-records-args)
(s/def ::delete-records-args
  (s/cat :provider ::provider :opts ::opts :zone ::zone-name
         :records ::record-selectors))
(s/def ::list-zones-args
  (s/cat :provider ::provider :opts ::opts))

(s/fdef deadline/after
  :args (s/cat :duration ::positive-duration)
  :ret :ol.protocol53/deadline)
(s/fdef deadline/remaining
  :args (s/cat :operation-deadline :ol.protocol53/deadline)
  :ret ::non-negative-duration)
(s/fdef deadline/expired?
  :args (s/cat :operation-deadline :ol.protocol53/deadline)
  :ret boolean?)
(s/fdef deadline/within?
  :args (s/cat :operation-deadline :ol.protocol53/deadline
               :duration ::positive-duration)
  :ret boolean?)
