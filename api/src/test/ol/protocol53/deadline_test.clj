(ns ol.protocol53.deadline-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [fulcro-spec.core :refer [=> =throws=> assertions behavior specification]]
   [ol.protocol53.deadline :as deadline]
   [ol.protocol53.specs :as specs])
  (:import
   [java.time Duration]))

(defn- with-nano-time [nanos f]
  (with-redefs-fn
    {(ns-resolve 'ol.protocol53.deadline 'nano-time) (constantly nanos)}
    f))

(specification "A positive deadline"
  (behavior "has and reports its remaining budget"
    (let [operation-deadline
          (with-nano-time 100
            #(deadline/after (Duration/ofNanos 10)))]
      (assertions
        "is represented by the controlled raw deadline value"
        operation-deadline => 110
        "reports the budget at a later clock sample"
        {:expired?  (with-nano-time 105
                      #(deadline/expired? operation-deadline))
         :remaining (with-nano-time 105
                      #(deadline/remaining operation-deadline))
         :within-5? (with-nano-time 105
                      #(deadline/within? operation-deadline
                                         (Duration/ofNanos 5)))
         :within-6? (with-nano-time 105
                      #(deadline/within? operation-deadline
                                         (Duration/ofNanos 6)))}
        => {:expired?  false
            :remaining (Duration/ofNanos 5)
            :within-5? true
            :within-6? false}))))

(specification "An expired deadline"
  (behavior "has no negative budget"
    (let [operation-deadline
          (with-nano-time 100
            #(deadline/after (Duration/ofNanos 10)))]
      (assertions
        {:at-expiry
         (with-nano-time 110
           #(hash-map :expired? (deadline/expired? operation-deadline)
                      :remaining (deadline/remaining operation-deadline)))
         :after-expiry
         (with-nano-time 115
           #(hash-map :expired? (deadline/expired? operation-deadline)
                      :remaining (deadline/remaining operation-deadline)))}
        => {:at-expiry    {:expired?  true
                           :remaining Duration/ZERO}
            :after-expiry {:expired?  true
                           :remaining Duration/ZERO}}))))

(specification "Deadline arithmetic"
  (behavior "survives nanoTime wraparound"
    (let [now                (- Long/MAX_VALUE 4)
          operation-deadline (with-nano-time now
                               #(deadline/after (Duration/ofNanos 10)))
          three-nanos-later  (unchecked-add (long now) (long 3))
          expiry             (unchecked-add (long now) (long 10))]
      (assertions
        {:remaining (with-nano-time three-nanos-later
                      #(deadline/remaining operation-deadline))
         :expired?  (with-nano-time expiry
                      #(deadline/expired? operation-deadline))}
        => {:remaining (Duration/ofNanos 7)
            :expired?  true}))))

(specification "Relative deadline durations"
  (doseq [duration [nil
                    "one second"
                    Duration/ZERO
                    (Duration/ofNanos -1)
                    (Duration/ofNanos Long/MAX_VALUE)
                    (Duration/ofSeconds Long/MAX_VALUE)]]
    (behavior (str "rejects " (pr-str duration) " in after")
      (assertions
        (deadline/after duration) =throws=> IllegalArgumentException)))
  (let [operation-deadline (deadline/after (Duration/ofSeconds 1))]
    (doseq [duration [nil
                      Duration/ZERO
                      (Duration/ofNanos -1)
                      (Duration/ofNanos Long/MAX_VALUE)]]
      (behavior (str "rejects " (pr-str duration) " in within?")
        (assertions
          (deadline/within? operation-deadline duration)
          =throws=> IllegalArgumentException)))))

(specification "Deadline duration specs"
  (behavior "match the runtime contracts"
    (assertions
      "for positive durations"
      (mapv #(s/valid? ::specs/positive-duration %)
            [(Duration/ofNanos 1)
             Duration/ZERO
             (Duration/ofNanos -1)
             (Duration/ofNanos Long/MAX_VALUE)
             (Duration/ofSeconds Long/MAX_VALUE)])
      => [true false false false false]
      "for non-negative durations"
      (mapv #(s/valid? ::specs/non-negative-duration %)
            [Duration/ZERO
             (Duration/ofNanos 1)
             (Duration/ofNanos -1)
             :not-a-duration])
      => [true true false false])))

(specification "Deadline arguments"
  (doseq [[label f] [["expired?" deadline/expired?]
                     ["remaining" deadline/remaining]]]
    (behavior (str "are validated by " label)
      (assertions
        (f :not-a-deadline) =throws=> IllegalArgumentException)))
  (behavior "are validated by within?"
    (assertions
      #_{:clj-kondo/ignore [:type-mismatch]}
      (deadline/within? :not-a-deadline (Duration/ofNanos 1))
      =throws=> IllegalArgumentException)))

(specification "Public deadline functions"
  (behavior "have instrumentable specs"
    (let [symbols            '[ol.protocol53.deadline/after
                               ol.protocol53.deadline/remaining
                               ol.protocol53.deadline/expired?
                               ol.protocol53.deadline/within?]
          operation-deadline (deadline/after (Duration/ofSeconds 1))]
      (assertions
        "register all four specs"
        (filterv s/get-spec symbols) => symbols)
      (try
        (assertions
          "instrument all four functions"
          (set (stest/instrument symbols)) => (set symbols)
          "reject an invalid after duration"
          (deadline/after Duration/ZERO) =throws=> clojure.lang.ExceptionInfo
          "reject an invalid within? duration"
          (deadline/within? operation-deadline Duration/ZERO)
          =throws=> clojure.lang.ExceptionInfo)
        (finally
          (stest/unstrument symbols))))))
