(ns run-integration-tests-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]))

(def script-path
  (.getCanonicalPath (io/file "scripts/run_integration_tests.clj")))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(defn- value-after [value values]
  (second (drop-while #(not= value %) values)))

(defn- parsed-config [file]
  (->> (slurp file)
       str/split-lines
       rest
       (str/join "\n")
       edn/read-string))

(defn- fake-clojure! [root]
  (let [bin  (doto (io/file root "bin") .mkdirs)
        file (io/file bin "clojure")]
    (spit file
          (str "#!/bin/sh\n"
               "printf '%s\\n' \"$@\" > \"$CAPTURE_ARGS\"\n"
               "while [ \"$#\" -gt 0 ]; do\n"
               "  if [ \"$1\" = \"--config-file\" ]; then\n"
               "    shift\n"
               "    cp \"$1\" \"$CAPTURE_CONFIG\"\n"
               "  fi\n"
               "  shift\n"
               "done\n"))
    (.setExecutable file true)
    bin))

(defn- run-script [providers args]
  (let [root         (doto (io/file (System/getProperty "java.io.tmpdir")
                                    (str "protocol53-integration-runner-"
                                         (random-uuid)))
                       .mkdirs)
        capture-args (io/file root "captured-args")
        capture-conf (io/file root "captured-config")]
    (try
      (doseq [provider providers]
        (.mkdirs (io/file root "providers" provider "src/test-integration")))
      (let [^java.io.File bin (fake-clojure! root)
            env               (assoc (into {} (System/getenv))
                                     "PATH" (str (.getPath bin) ":" (System/getenv "PATH"))
                                     "CAPTURE_ARGS" (.getPath capture-args)
                                     "CAPTURE_CONFIG" (.getPath capture-conf))
            result            (apply shell/sh
                                     (concat ["bb" script-path]
                                             args
                                             [:dir (.getPath root) :env env]))
            called?           (.exists capture-args)
            command-args      (when called?
                                (str/split-lines (slurp capture-args)))
            sdeps             (when called?
                                (edn/read-string (value-after "-Sdeps" command-args)))
            config            (when (.exists capture-conf)
                                (parsed-config capture-conf))]
        {:exit            (:exit result)
         :err             (:err result)
         :clojure-called? called?
         :provider-paths  (some->> sdeps
                                   :deps
                                   vals
                                   (map :local/root)
                                   sort
                                   vec)
         :source-paths    (get-in config [:tests 0 :kaocha/source-paths])
         :test-paths      (get-in config [:tests 0 :kaocha/test-paths])
         :forwarded       (when called?
                            (->> command-args
                                 (drop-while #(not= "--no-fail-fast" %))
                                 rest
                                 vec))})
      (finally
        (delete-tree! root)))))

(specification "The integration test runner provider selector"
  (behavior "runs every discovered provider when omitted"
    (let [result (run-script ["powerdns" "desec" "cloudflare"]
                             ["--reporter" "documentation"])]
      (assertions
        (dissoc result :err)
        => {:exit            0
            :clojure-called? true
            :provider-paths  ["providers/cloudflare"
                              "providers/desec"
                              "providers/powerdns"]
            :source-paths    ["testkit/src/main"
                              "providers/cloudflare/src/main"
                              "providers/desec/src/main"
                              "providers/powerdns/src/main"]
            :test-paths      ["providers/cloudflare/src/test-integration"
                              "providers/desec/src/test-integration"
                              "providers/powerdns/src/test-integration"]
            :forwarded       ["--reporter" "documentation"]})))

  (behavior "runs comma-separated providers and forwards remaining Kaocha arguments"
    (let [result (run-script ["powerdns" "desec" "cloudflare"]
                             ["--provider" "desec,cloudflare"
                              "--focus" ":some-test"])]
      (assertions
        (dissoc result :err)
        => {:exit            0
            :clojure-called? true
            :provider-paths  ["providers/cloudflare"
                              "providers/desec"]
            :source-paths    ["testkit/src/main"
                              "providers/cloudflare/src/main"
                              "providers/desec/src/main"]
            :test-paths      ["providers/cloudflare/src/test-integration"
                              "providers/desec/src/test-integration"]
            :forwarded       ["--focus" ":some-test"]})))

  (behavior "rejects unknown providers before starting Clojure"
    (let [result (run-script ["desec" "cloudflare"]
                             ["--provider" "desec,missing"])]
      (assertions
        (-> result
            (select-keys [:exit :clojure-called?])
            (assoc :unknown? (str/includes? (:err result)
                                            "Unknown integration provider(s): missing")
                   :available? (str/includes? (:err result)
                                              "Available providers: cloudflare, desec")))
        => {:exit            1
            :clojure-called? false
            :unknown?        true
            :available?      true})))

  (behavior "rejects a missing or empty provider value"
    (let [missing          (run-script ["desec"] ["--provider"])
          empty            (run-script ["desec" "cloudflare"]
                                       ["--provider" "desec,,cloudflare"])
          following-option (run-script ["desec"]
                                       ["--provider" "--focus" ":probe"])]
      (assertions
        (mapv (fn [result]
                {:exit            (:exit result)
                 :clojure-called? (:clojure-called? result)
                 :helpful?        (str/includes?
                                   (:err result)
                                   "--provider requires comma-separated provider names")})
              [missing empty following-option])
        => [{:exit 1 :clojure-called? false :helpful? true}
            {:exit 1 :clojure-called? false :helpful? true}
            {:exit 1 :clojure-called? false :helpful? true}]))))
