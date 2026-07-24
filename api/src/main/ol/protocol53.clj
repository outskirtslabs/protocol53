(ns ol.protocol53
  "The DNS operations you call independent of any one provider.

  protocol53 gives you a small, fixed set of operations — [[get-records!]],
  [[append-records!]], [[set-records!]], [[delete-records!]], and
  [[list-zones!]] — that behave the same behind every supported DNS provider.
  Write against these functions once and switch providers, such as Cloudflare,
  without changing your call sites.

  A provider is any value that implements the capabilities in
  [[ol.protocol53.protocols]]. You build one from its own namespace, for
  example `ol.protocol53.cloudflare/provider`, and pass it as the first
  argument to every operation.

  Each operation takes trailing `opts` with exactly one time budget. A positive
  [[java.time.Duration]] under `:timeout` starts a fresh budget for the call. A
  process-local `:deadline`, created with [[ol.protocol53.deadline/after]], lets
  several calls share one budget. The facade normalizes both forms to a deadline
  before provider dispatch.

  Operations do not throw on expected failures. Each returns a map with exactly
  one of these keys:

  - `:ol.protocol53/result` on success, holding the records or zones produced.
  - `:ol.protocol53/error` on failure, describing what went wrong and whether a
    retry might succeed.

  So a typical call checks for `:ol.protocol53/error` before using the result."
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

(defn- normalized-provider-opts [opts]
  (if-let [timeout (:timeout opts)]
    (-> opts
        (dissoc :timeout)
        (assoc :deadline (deadline/after timeout)))
    opts))

(defn- invoke-provider!
  [provider opts operation capability outcome-spec error-details invoke]
  (if-not (s/valid? ::specs/opts opts)
    (error-outcome provider operation :invalid-record
                   "Invalid operation options" error-details)
    (let [provider-opts (normalized-provider-opts opts)]
      (cond
        (deadline/expired? (:deadline provider-opts))
        (error-outcome provider operation :deadline-exceeded
                       "Deadline exceeded before provider operation" error-details)

        (not (satisfies? capability provider))
        (error-outcome provider operation :unsupported-operation
                       (str "Provider does not support " (name operation))
                       error-details)

        :else
        (checked-provider-outcome provider operation outcome-spec
                                  (invoke provider-opts))))))

(defn get-records!
  "Reads every record in `zone` and returns them in the result.

  Read-only; it never changes the zone. On success the result holds a vector of
  records under `:records`. Supply exactly one time-budget option.

  Options:

  | key         | description
  | ----------- | -----------
  | `:timeout`  | Positive [[java.time.Duration]] starting a fresh budget.
  | `:deadline` | Process-local deadline for sharing an existing budget."
  [provider zone opts]
  (if-not (s/valid? ::specs/zone-name zone)
    (error-outcome provider :get-records :invalid-record "Invalid zone" {})
    (invoke-provider! provider opts :get-records protocols/RecordGetter
                      ::specs/record-outcome {:zone zone}
                      #(protocols/-get-records! provider zone %))))

(defn append-records!
  "Adds `records` to `zone`, leaving every existing record in place.

  Use this to create records without disturbing what is already there. On
  success the result lists the records as the provider stored them. Supply
  exactly one time-budget option.

  Options:

  | key         | description
  | ----------- | -----------
  | `:timeout`  | Positive [[java.time.Duration]] starting a fresh budget.
  | `:deadline` | Process-local deadline for sharing an existing budget."
  [provider zone records opts]
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
                      #(protocols/-append-records! provider zone records %))))

(defn set-records!
  "Replaces whole RRsets in `zone` with `records`.

  For each `name`/`type` pair present in `records`, this removes every existing
  record of that name and type and installs the ones you gave. Names and types
  you do not mention are left untouched. Use it to declare the exact desired
  state of the RRsets you care about. Supply exactly one time-budget option.

  Options:

  | key         | description
  | ----------- | -----------
  | `:timeout`  | Positive [[java.time.Duration]] starting a fresh budget.
  | `:deadline` | Process-local deadline for sharing an existing budget."
  [provider zone records opts]
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
                      #(protocols/-set-records! provider zone records %))))

(defn delete-records!
  "Removes records in `zone` that match `records` and ignores the rest.

  Each entry in `records` is a selector: it must name a record and may narrow
  the match by `type`, `ttl`, or `data`. A selector that matches nothing is not
  an error. On success the result lists the records that were actually removed.
  Supply exactly one time-budget option.

  Options:

  | key         | description
  | ----------- | -----------
  | `:timeout`  | Positive [[java.time.Duration]] starting a fresh budget.
  | `:deadline` | Process-local deadline for sharing an existing budget."
  [provider zone records opts]
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
                      #(protocols/-delete-records! provider zone records %))))

(defn list-zones!
  "Lists the zones `provider` can operate on.

  On success the result holds a vector of zone maps under `:zones`. Use it to
  discover which zones the other operations may target. Supply exactly one
  time-budget option.

  Options:

  | key         | description
  | ----------- | -----------
  | `:timeout`  | Positive [[java.time.Duration]] starting a fresh budget.
  | `:deadline` | Process-local deadline for sharing an existing budget."
  [provider opts]
  (invoke-provider! provider opts :list-zones protocols/ZoneLister
                    ::specs/zone-outcome {}
                    #(protocols/-list-zones! provider %)))

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
