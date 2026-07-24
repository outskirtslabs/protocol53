(ns ol.protocol53.cloudflare-lifecycle-test
  (:require
   [clojure.string :as str]
   [fulcro-spec.core :refer [specification]]
   [ol.protocol53.cloudflare :as cloudflare]
   [ol.protocol53.testkit :as testkit]
   [ol.protocol53.testkit.dotenv :as dotenv]))

(defn- environment []
  (merge (dotenv/load-env-file ".env")
         (into {} (System/getenv))))

(defn- live-config []
  (let [env        (environment)
        api-token  (get env "CLOUDFLARE_API_TOKEN")
        zone-token (get env "CLOUDFLARE_ZONE_TOKEN")
        domain     (or (get env "CLOUDFLARE_TEST_ZONE")
                       (get env "CLOUDFLARE_DOMAIN"))
        zone       (when-not (str/blank? domain)
                     (if (str/ends-with? domain ".")
                       domain
                       (str domain ".")))]
    (when (and (not (str/blank? api-token))
               (not (str/blank? zone)))
      {:provider            (cloudflare/provider
                             (cond-> {:api-token api-token}
                               (not (str/blank? zone-token))
                               (assoc :zone-token zone-token)))
       :zone                zone
       :allow-live-changes? true
       :skip-empty-txt?     true})))

(when-not (live-config)
  (println "Skipping Cloudflare integration tests: credentials not configured")
  (alter-meta! *ns* assoc :kaocha/pending true))

(specification "The live Cloudflare provider lifecycle"
  (when-let [config (live-config)]
    (testkit/run! config)))