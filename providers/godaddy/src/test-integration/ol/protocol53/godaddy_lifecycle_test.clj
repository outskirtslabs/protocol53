(ns ol.protocol53.godaddy-lifecycle-test
  (:require
   [clojure.string :as str]
   [fulcro-spec.core :refer [specification]]
   [ol.protocol53.godaddy :as godaddy]
   [ol.protocol53.testkit :as testkit]
   [ol.protocol53.testkit.dotenv :as dotenv]))

(defn- environment []
  (merge (dotenv/load-env-file ".env")
         (into {} (System/getenv))))

(defn- live-config []
  (let [env        (environment)
        api-key    (get env "GODADDY_API_KEY")
        api-secret (get env "GODADDY_API_SECRET")
        shopper-id (get env "GODADDY_SHOPPER_ID")
        zone-name  (get env "GODADDY_DOMAIN")
        zone       (when-not (str/blank? zone-name)
                     (if (str/ends-with? zone-name ".")
                       zone-name
                       (str zone-name ".")))]
    (when (and (not (str/blank? api-key))
               (not (str/blank? api-secret))
               (not (str/blank? zone)))
      {:provider            (godaddy/provider
                             (cond-> {:api-key    api-key
                                      :api-secret api-secret}
                               (not (str/blank? shopper-id))
                               (assoc :shopper-id shopper-id)))
       :zone                zone
       :allow-live-changes? true
       :ignore-ttl?         true
       :skip-record-types   #{"HTTPS" "NS" "SVCB"}})))

(when-not (live-config)
  (println "Skipping GoDaddy integration tests: credentials not configured")
  (alter-meta! *ns* assoc :kaocha/pending true))

(specification "The live GoDaddy provider lifecycle"
  (when-let [config (live-config)]
    (testkit/run! config)))
