(ns ol.protocol53.testkit.in-memory-provider
  (:require
   [clojure.string :as str]
   [ol.protocol53.protocols :as protocols]))

(defn- record-result [records]
  {:ol.protocol53/result {:records (vec records)}})

(defn- zone-result [zones]
  {:ol.protocol53/result {:zones (mapv (fn [zone] {:name zone}) zones)}})

(defn- error-result [operation zone]
  {:ol.protocol53/error
   {:type       :provider-request
    :message    "Deliberate in-memory provider failure"
    :operation  operation
    :provider   :in-memory
    :retryable? false
    :zone       zone}})

(defn- rrset-key [{:keys [name type]}]
  [(str/lower-case name) (str/upper-case type)])

(defn- selector-match? [selector record]
  (and (= (str/lower-case (:name selector))
          (str/lower-case (:name record)))
       (or (not (contains? selector :type))
           (= (str/upper-case (:type selector))
              (str/upper-case (:type record))))
       (or (not (contains? selector :ttl))
           (= (:ttl selector) (:ttl record)))
       (or (not (contains? selector :data))
           (= (:data selector) (:data record)))))

(defn- first-per-rrset [records]
  (vals
   (reduce (fn [by-rrset record]
             (let [key (rrset-key record)]
               (if (contains? by-rrset key)
                 by-rrset
                 (assoc by-rrset key record))))
           (array-map)
           records)))

(defn- returned-records [{:keys [metadata?]} records]
  (if metadata?
    (mapv #(assoc % :in-memory/id "provider-extension") records)
    (vec records)))

(defn- record-call! [{:keys [calls]} operation opts details]
  (swap! calls conj (merge {:operation operation :opts opts} details)))

(defn- get-records [{:keys [fault zones] :as provider} zone opts]
  (record-call! provider :get-records opts {:zone zone})
  (if (= :get-error fault)
    (error-result :get-records zone)
    (record-result (returned-records provider (get @zones zone [])))))

(defn- append-records [{:keys [fault zones] :as provider} zone records opts]
  (record-call! provider :append-records opts {:records records :zone zone})
  (case fault
    :destructive-append
    (reset! zones {zone (vec records)})

    (swap! zones update zone (fnil into []) records))
  (record-result
   (returned-records
    provider
    (cond-> records
      (= :duplicate-append-return fault) (conj (first records))))))

(defn- set-records [{:keys [fault zones] :as provider} zone records opts]
  (record-call! provider :set-records opts {:records records :zone zone})
  (let [selected-rrsets (set (map rrset-key records))
        installed       (if (= :incomplete-set fault)
                          (first-per-rrset records)
                          records)]
    (if (= :destructive-set fault)
      (reset! zones {zone (vec installed)})
      (swap! zones update zone
             (fn [existing]
               (into (filterv #(not (contains? selected-rrsets
                                               (rrset-key %)))
                              existing)
                     installed)))))
  (record-result (returned-records provider records)))

(defn- delete-records [{:keys [fault zones] :as provider} zone selectors opts]
  (record-call! provider :delete-records opts
                {:records selectors :zone zone})
  (if (= :no-op-delete fault)
    (record-result [])
    (let [existing  (get @zones zone [])
          deleted   (filterv #(some (fn [selector]
                                      (selector-match? selector %))
                                    selectors)
                             existing)
          remaining (filterv #(not (some (fn [selector]
                                           (selector-match? selector %))
                                         selectors))
                             existing)]
      (swap! zones assoc zone remaining)
      (record-result
       (if (= :dishonest-delete fault)
         []
         (returned-records provider deleted))))))

(defrecord InMemoryProvider [zones calls fault metadata?]
  protocols/RecordGetter
  (-get-records! [this zone opts]
    (get-records this zone opts))

  protocols/RecordAppender
  (-append-records! [this zone records opts]
    (append-records this zone records opts))

  protocols/RecordSetter
  (-set-records! [this zone records opts]
    (set-records this zone records opts))

  protocols/RecordDeleter
  (-delete-records! [this zone records opts]
    (delete-records this zone records opts))

  protocols/ZoneLister
  (-list-zones! [this opts]
    (record-call! this :list-zones opts {})
    (zone-result (keys @zones))))

(defrecord NoZoneListerProvider [provider]
  protocols/RecordGetter
  (-get-records! [_ zone opts]
    (protocols/-get-records! provider zone opts))

  protocols/RecordAppender
  (-append-records! [_ zone records opts]
    (protocols/-append-records! provider zone records opts))

  protocols/RecordSetter
  (-set-records! [_ zone records opts]
    (protocols/-set-records! provider zone records opts))

  protocols/RecordDeleter
  (-delete-records! [_ zone records opts]
    (protocols/-delete-records! provider zone records opts)))

(defrecord MissingDeleterProvider [provider]
  protocols/RecordGetter
  (-get-records! [_ zone opts]
    (protocols/-get-records! provider zone opts))

  protocols/RecordAppender
  (-append-records! [_ zone records opts]
    (protocols/-append-records! provider zone records opts))

  protocols/RecordSetter
  (-set-records! [_ zone records opts]
    (protocols/-set-records! provider zone records opts)))

(defn provider
  ([]
   (provider {}))
  ([{:keys [fault metadata? records zone]
     :or   {records []
            zone    "example.com."}}]
   (map->InMemoryProvider
    {:zones                  (atom {zone (vec records)})
     :calls                  (atom [])
     :fault                  fault
     :metadata?              metadata?
     :ol.protocol53/provider :in-memory})))

(defn provider-without-zone-lister []
  (let [provider (provider)]
    (assoc (->NoZoneListerProvider provider)
           :ol.protocol53/provider :in-memory)))

(defn provider-missing-deleter []
  (let [provider (provider)]
    (assoc (->MissingDeleterProvider provider)
           :ol.protocol53/provider :in-memory)))

(defn delegate [provider]
  (or (:provider provider) provider))

(defn calls [provider]
  @(-> provider delegate :calls))

(defn zone-records
  ([provider]
   (zone-records provider "example.com."))
  ([provider zone]
   (get @(-> provider delegate :zones) zone [])))

(defn seed! [provider records]
  (swap! (-> provider delegate :zones)
         update "example.com." (fnil into []) records)
  provider)
