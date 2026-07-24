(ns run-integration-tests
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]))

(defn- run-integration-tests! [args]
  (let [integration-paths (->> (fs/glob "providers" "*/integration-test")
                               (filter fs/directory?)
                               (map str)
                               sort
                               vec)]
    (if (empty? integration-paths)
      (println "No provider integration test suites found")
      (let [provider-paths (mapv (comp str fs/parent) integration-paths)
            provider-deps  (into {}
                                 (map (fn [provider-path]
                                        [(symbol "ol.protocol53.integration"
                                                 (str (fs/file-name provider-path)))
                                         {:local/root provider-path}])
                                      provider-paths))
            source-paths   (into ["testkit/src"]
                                 (map (fn [provider-path]
                                        (str provider-path "/src"))
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
                 args)
          (finally
            (fs/delete-if-exists config-file)))))))

(run-integration-tests! *command-line-args*)
