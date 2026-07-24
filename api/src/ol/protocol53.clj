(ns ol.protocol53
  "Validated caller-facing operations for portable DNS providers.

  Provider implementations extend the capabilities in
  [[ol.protocol53.protocols]]. Every operation requires an options map with an
  absolute monotonic deadline from [[ol.protocol53.deadline]]."
  (:require
   [clojure.spec.alpha :as s]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.protocols :as protocols]
   [ol.protocol53.specs :as specs]))

(defn- provider-id [provider]
  (let [id (get provider :ol.protocol53/provider)]
    (if (keyword? id) id :unknown)))

(defn- error-outcome
  [provider operation type message details]
  {:ol.protocol53/error
   (merge {:type       type
           :message    message
           :operation  operation
           :provider   (provider-id provider)
           :retryable? false}
          details)})

(defn- checked-provider-outcome
  [provider operation outcome-spec outcome]
  (if (s/valid? outcome-spec outcome)
    outcome
    (throw
     (ex-info (str "Provider returned an invalid " (name operation) " outcome")
              {:ol.protocol53/operation operation
               :ol.protocol53/provider  (provider-id provider)}))))

(defn- invoke-provider!
  [provider opts operation capability outcome-spec error-details invoke]
  (cond
    (not (s/valid? ::specs/opts opts))
    (error-outcome provider operation :invalid-record
                   "Invalid operation options" error-details)

    (deadline/expired? (:ol.protocol53/deadline opts))
    (error-outcome provider operation :deadline-exceeded
                   "Deadline exceeded before provider operation" error-details)

    (not (satisfies? capability provider))
    (error-outcome provider operation :unsupported-operation
                   (str "Provider does not support " (name operation))
                   error-details)

    :else
    (checked-provider-outcome provider operation outcome-spec (invoke))))

(defn get-records!
  "Returns all portable records in `zone` through `provider`.

  Options:

  | key                       | description
  | ------------------------- | -----------
  | `:ol.protocol53/deadline` | Required absolute monotonic deadline."
  [provider opts zone]
  (if-not (s/valid? ::specs/zone-name zone)
    (error-outcome provider :get-records :invalid-record "Invalid zone" {})
    (invoke-provider! provider opts :get-records protocols/RecordGetter
                      ::specs/record-outcome {:zone zone}
                      #(protocols/-get-records! provider opts zone))))

(defn append-records!
  "Appends `records` in `zone` without changing existing records.

  Options:

  | key                       | description
  | ------------------------- | -----------
  | `:ol.protocol53/deadline` | Required absolute monotonic deadline."
  [provider opts zone records]
  (cond
    (not (s/valid? ::specs/zone-name zone))
    (error-outcome provider :append-records :invalid-record
                   "Invalid zone" {:zone-state :unchanged})

    (not (s/valid? ::specs/records records))
    (error-outcome provider :append-records :invalid-record
                   "Invalid records" {:zone zone :zone-state :unchanged})

    :else
    (invoke-provider! provider opts :append-records protocols/RecordAppender
                      ::specs/record-outcome
                      {:zone zone :zone-state :unchanged}
                      #(protocols/-append-records! provider opts zone records))))

(defn set-records!
  "Makes `records` the complete selected RRsets in `zone`.

  Options:

  | key                       | description
  | ------------------------- | -----------
  | `:ol.protocol53/deadline` | Required absolute monotonic deadline."
  [provider opts zone records]
  (cond
    (not (s/valid? ::specs/zone-name zone))
    (error-outcome provider :set-records :invalid-record
                   "Invalid zone" {:zone-state :unchanged})

    (not (s/valid? ::specs/records records))
    (error-outcome provider :set-records :invalid-record
                   "Invalid records" {:zone zone :zone-state :unchanged})

    :else
    (invoke-provider! provider opts :set-records protocols/RecordSetter
                      ::specs/record-outcome
                      {:zone zone :zone-state :unchanged}
                      #(protocols/-set-records! provider opts zone records))))

(defn delete-records!
  "Deletes records matching `records` from `zone` and ignores misses.

  Options:

  | key                       | description
  | ------------------------- | -----------
  | `:ol.protocol53/deadline` | Required absolute monotonic deadline."
  [provider opts zone records]
  (cond
    (not (s/valid? ::specs/zone-name zone))
    (error-outcome provider :delete-records :invalid-record
                   "Invalid zone" {:zone-state :unchanged})

    (not (s/valid? ::specs/record-selectors records))
    (error-outcome provider :delete-records :invalid-record
                   "Invalid record selectors"
                   {:zone zone :zone-state :unchanged})

    :else
    (invoke-provider! provider opts :delete-records protocols/RecordDeleter
                      ::specs/record-outcome
                      {:zone zone :zone-state :unchanged}
                      #(protocols/-delete-records! provider opts zone records))))

(defn list-zones!
  "Returns zones available to record operations through `provider`.

  Options:

  | key                       | description
  | ------------------------- | -----------
  | `:ol.protocol53/deadline` | Required absolute monotonic deadline."
  [provider opts]
  (invoke-provider! provider opts :list-zones protocols/ZoneLister
                    ::specs/zone-outcome {}
                    #(protocols/-list-zones! provider opts)))

(s/fdef get-records!
  :args ::specs/get-records-args
  :ret ::specs/record-outcome)
(s/fdef append-records!
  :args ::specs/append-records-args
  :ret ::specs/record-outcome)
(s/fdef set-records!
  :args ::specs/set-records-args
  :ret ::specs/record-outcome)
(s/fdef delete-records!
  :args ::specs/delete-records-args
  :ret ::specs/record-outcome)
(s/fdef list-zones!
  :args ::specs/list-zones-args
  :ret ::specs/zone-outcome)
