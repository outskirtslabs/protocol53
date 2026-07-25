(ns ol.protocol53.porkbun-lifecycle-test
  (:require
   [clojure.string :as str]
   [fulcro-spec.core :refer [specification]]
   [ol.protocol53.porkbun :as porkbun]
   [ol.protocol53.testkit :as testkit]
   [ol.protocol53.testkit.dotenv :as dotenv])
  (:import
   [java.time Duration]))

(defn- environment []
  (merge (dotenv/load-env-file ".env")
         (into {} (System/getenv))))

(defn- live-config []
  (let [env        (environment)
        api-key    (get env "PORKBUN_API_KEY")
        secret-key (get env "PORKBUN_SECRET_KEY")
        domain     (get env "PORKBUN_DOMAIN")
        zone       (when-not (str/blank? domain)
                     (if (str/ends-with? domain ".")
                       domain
                       (str domain ".")))]
    (when (and (not (str/blank? api-key))
               (not (str/blank? secret-key))
               (not (str/blank? zone)))
      {:provider            (porkbun/provider {:api-key    api-key
                                               :secret-key secret-key})
       :zone                zone
       :allow-live-changes? true
       :ignore-ttl?         true
       :skip-empty-txt?     true
       :timeout             (Duration/ofMinutes 2)})))

(when-not (live-config)
  (println "Skipping Porkbun integration tests: credentials not configured")
  (alter-meta! *ns* assoc :kaocha/pending true))

(specification "The live Porkbun provider lifecycle"
  (when-let [config (live-config)]
    (testkit/run! config)))
