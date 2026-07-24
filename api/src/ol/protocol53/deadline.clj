(ns ol.protocol53.deadline
  "Construction and queries for process-local monotonic deadlines.

  Deadline values are raw `long` positions on the current process's monotonic
  `System/nanoTime` timeline. Construct them with [[after]] and inspect them
  with the other helpers in this namespace. They must not be serialized,
  persisted, or compared with wall-clock values."
  (:import
   [java.time Duration]))

(defn- nano-time []
  (System/nanoTime))

(defn- duration-nanos [duration]
  (when-not (instance? Duration duration)
    (throw (IllegalArgumentException. "Expected a java.time.Duration")))
  (let [^Duration duration duration]
    (when (or (.isZero duration) (.isNegative duration))
      (throw (IllegalArgumentException. "Duration must be positive")))
    (let [nanos (try
                  (.toNanos duration)
                  (catch ArithmeticException cause
                    (throw (IllegalArgumentException.
                            "Duration exceeds the supported deadline range"
                            cause))))]
      (when (= Long/MAX_VALUE nanos)
        (throw (IllegalArgumentException.
                "Duration must be less than Long/MAX_VALUE nanoseconds")))
      nanos)))

(defn after
  "Returns a process-local deadline one positive `duration` from now.

  `duration` must be shorter than `Long/MAX_VALUE` nanoseconds, approximately
  292 years."
  [duration]
  (unchecked-add (long (nano-time))
                 (long (duration-nanos duration))))

(defn remaining
  "Returns the non-negative duration remaining before `operation-deadline`."
  [operation-deadline]
  (when-not (instance? Long operation-deadline)
    (throw (IllegalArgumentException. "Expected a java.lang.Long deadline")))
  (let [nanos (unchecked-subtract (long operation-deadline)
                                  (long (nano-time)))]
    (if (pos? nanos)
      (Duration/ofNanos nanos)
      Duration/ZERO)))

(defn expired?
  "Returns whether no budget remains before `operation-deadline`."
  [operation-deadline]
  (.isZero ^Duration (remaining operation-deadline)))

(defn within?
  "Returns whether positive `duration` fits within `operation-deadline`."
  [operation-deadline duration]
  (<= (duration-nanos duration)
      (.toNanos ^Duration (remaining operation-deadline))))
