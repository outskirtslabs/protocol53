(ns ol.protocol53.desec-lifecycle-test
  (:require
   [clojure.string :as str]
   [fulcro-spec.core :refer [specification]]
   [ol.protocol53.desec :as desec]
   [ol.protocol53.testkit :as testkit]
   [ol.protocol53.testkit.dotenv :as dotenv]))

(defn- environment []
  (merge (dotenv/load-env-file ".env")
         (into {} (System/getenv))))

(defn- live-config []
  (let [env    (environment)
        token  (get env "DESEC_TOKEN")
        domain (get env "DESEC_DOMAIN")
        zone   (when-not (str/blank? domain)
                 (if (str/ends-with? domain ".")
                   domain
                   (str domain ".")))]
    (when (and (not (str/blank? token))
               (not (str/blank? zone)))
      {:provider            (desec/provider {:token token})
       :zone                zone
       :allow-live-changes? true
       :ignore-ttl?         true})))

(when-not (live-config)
  (println "Skipping deSEC integration tests: credentials not configured")
  (alter-meta! *ns* assoc :kaocha/pending true))

(specification "The live deSEC provider lifecycle"
  (when-let [config (live-config)]
    (testkit/run! config)))
