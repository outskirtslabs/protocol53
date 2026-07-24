(ns ol.protocol53.deadline
  "Time budgets for protocol53 operations.

  A deadline is a fixed point in the future that answers one question: has an
  operation run out of time? Callers usually pass a positive
  [[java.time.Duration]] under `:timeout`; the protocol53 facade converts it to
  the `:deadline` in normalized provider options. Callers may instead create a
  deadline when several operations must share one budget.

  Deadlines are monotonic. They ride on `System/nanoTime`, so clock changes —
  NTP corrections, daylight saving, someone setting the system clock — cannot
  move them. That makes them reliable for timeouts and meaningless for anything
  else: a deadline is just a number that only makes sense inside the process
  that created it. Do not serialize it, store it, share it across processes, or
  compare it against wall-clock time.

  Build one with [[after]]. Check it with [[remaining]], [[expired?]], and
  [[within?]]."
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
  "Returns a deadline that falls `duration` from now.

  `duration` is a positive [[java.time.Duration]]; it must be shorter than
  `Long/MAX_VALUE` nanoseconds, roughly 292 years.

  ```clojure
  (deadline/after (java.time.Duration/ofSeconds 30))
  ```"
  [duration]
  (unchecked-add (long (nano-time))
                 (long (duration-nanos duration))))

(defn remaining
  "Returns how much time is left before `operation-deadline`.

  The value is a [[java.time.Duration]] that never goes negative; once the
  deadline has passed it is `Duration/ZERO`."
  [operation-deadline]
  (when-not (instance? Long operation-deadline)
    (throw (IllegalArgumentException. "Expected a java.lang.Long deadline")))
  (let [nanos (unchecked-subtract (long operation-deadline)
                                  (long (nano-time)))]
    (if (pos? nanos)
      (Duration/ofNanos nanos)
      Duration/ZERO)))

(defn expired?
  "Returns `true` once `operation-deadline` has no time left."
  [operation-deadline]
  (.isZero ^Duration (remaining operation-deadline)))

(defn within?
  "Returns `true` when `duration` still fits before `operation-deadline`.

  Use it to decide whether to start work that needs at least `duration` of
  budget. `duration` is a positive [[java.time.Duration]]."
  [operation-deadline duration]
  (<= (duration-nanos duration)
      (.toNanos ^Duration (remaining operation-deadline))))
