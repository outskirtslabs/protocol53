(ns run-integration-tests
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.string :as str]))

(defn- parsed-args [args]
  (loop [remaining      args
         provider-names nil
         kaocha-args    []]
    (if (empty? remaining)
      {:provider-names provider-names
       :kaocha-args    kaocha-args}
      (let [[arg & more] remaining]
        (if (= arg "--provider")
          (do
            (when provider-names
              (throw (ex-info "--provider may only be specified once" {})))
            (let [value (first more)
                  names (when value
                          (mapv str/trim (str/split value #"," -1)))]
              (when (or (nil? value)
                        (str/starts-with? value "--")
                        (some str/blank? names))
                (throw
                 (ex-info
                  "--provider requires comma-separated provider names"
                  {})))
              (recur (rest more) (set names) kaocha-args)))
          (recur more provider-names (conj kaocha-args arg)))))))

(defn- provider-name [integration-path]
  (str (fs/file-name (fs/parent (fs/parent integration-path)))))

(defn- run-integration-tests! [args]
  (let [{:keys [provider-names kaocha-args]} (parsed-args args)
        discovered-paths (->> (fs/glob "providers" "*/src/test-integration")
                              (filter fs/directory?)
                              (map str)
                              sort
                              vec)
        available-names (mapv provider-name discovered-paths)
        unknown-names (->> provider-names
                           (remove (set available-names))
                           sort
                           vec)
        _ (when (seq unknown-names)
            (throw
             (ex-info
              (str "Unknown integration provider(s): "
                   (str/join ", " unknown-names)
                   "\nAvailable providers: "
                   (str/join ", " available-names))
              {})))
        integration-paths                    (if provider-names
                                               (filterv #(contains? provider-names
                                                                    (provider-name %))
                                                        discovered-paths)
                                               discovered-paths)]
    (if (empty? integration-paths)
      (println "No provider integration test suites found")
      (let [provider-paths (mapv (comp str fs/parent fs/parent)
                                 integration-paths)
            provider-deps  (into {}
                                 (map (fn [provider-path]
                                        [(symbol "ol.protocol53.integration"
                                                 (str (fs/file-name provider-path)))
                                         {:local/root provider-path}])
                                      provider-paths))
            source-paths   (into ["testkit/src/main"]
                                 (map (fn [provider-path]
                                        (str provider-path "/src/main"))
                                      provider-paths))
            config         {:color?     false
                            :fail-fast? false
                            :tests      [{:id                  :provider-integration
                                          :kaocha/source-paths source-paths
                                          :kaocha/test-paths   integration-paths}]}
            sdeps          {:deps  provider-deps
                            :paths integration-paths}
            config-file    (fs/create-temp-file {:prefix "protocol53-integration-"
                                                 :suffix ".edn"})]
        (try
          (spit (str config-file)
                (str "#kaocha/v1\n" (pr-str config) "\n"))
          (apply process/shell
                 "clojure"
                 "-Sdeps"
                 (pr-str sdeps)
                 "-M:integration-test:kaocha"
                 "--config-file"
                 (str config-file)
                 "--no-fail-fast"
                 kaocha-args)
          (finally
            (fs/delete-if-exists config-file)))))))

(run-integration-tests! *command-line-args*)
