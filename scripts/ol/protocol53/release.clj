(ns ol.protocol53.release
  "Reads release metadata from the API and provider modules."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(defn read-deps
  "Reads the dependency configuration at `file`."
  [file]
  (edn/read-string (slurp file)))

(defn release-tag
  "Returns the artifact-prefixed tag for a module's project metadata."
  [{:keys [name version]}]
  (str (str/replace-first (clojure.core/name name) #"^protocol53(?=-|$)" "p53")
       "/v" version))

(defn modules
  "Discovers the API and providers below `root`, with the API first.

  Artifact metadata and paths come from each module's `deps.edn`.
  Repository SCM metadata comes from the root project's `:scm` map."
  [root]
  (let [shared (get-in (read-deps (io/file root "deps.edn")) [:aliases :neil :project])
        dirs   (cons (io/file root "api")
                     (sort-by #(.getName ^java.io.File %)
                              (filter #(.isFile (io/file % "deps.edn"))
                                      (.listFiles (io/file root "providers")))))]
    (mapv (fn [dir]
            (let [deps-file (io/file dir "deps.edn")
                  deps      (read-deps deps-file)
                  project   (merge (select-keys shared [:scm])
                                   (get-in deps [:aliases :neil :project]))]
              (when-not (and (qualified-symbol? (:name project))
                             (string? (:version project))
                             (re-matches #"\d+\.\d+\.\d+(?:-[A-Za-z0-9.-]+)?" (:version project))
                             (seq (:description project))
                             (seq (:url project))
                             (seq (get-in project [:license :id]))
                             (seq (get-in project [:scm :url]))
                             (seq (:paths deps)))
                (throw (ex-info "Incomplete release metadata" {:file (str deps-file)})))
              {:id        (keyword (.getName ^java.io.File dir))
               :dir       (str dir)
               :deps-file (str deps-file)
               :deps      deps
               :project   project
               :tag       (release-tag project)
               :changelog (str (io/file dir "CHANGELOG.adoc"))
               :src-dirs  (mapv #(str (io/file dir %)) (:paths deps))}))
          dirs)))

(defn selected
  "Selects modules by `ids`, or by `:api`, `:providers`, or `:all`.

  Returns modules in dependency order. Unknown ids and empty selections throw."
  [available ids]
  (let [ids     (cond
                  (= :all ids)       (mapv :id available)
                  (= :providers ids) (mapv :id (rest available))
                  (keyword? ids)     [ids]
                  :else              ids)
        unknown (remove (set (map :id available)) ids)]
    (when (or (empty? ids) (seq unknown))
      (throw (ex-info "Select known artifacts" {:selected  ids                  :unknown (vec unknown)
                                                :available (mapv :id available)})))
    (filterv #(contains? (set ids) (:id %)) available)))

(defn publication-deps
  "Returns direct Maven dependencies for `module`.

  Local module coordinates are translated for build/install rehearsals only.
  [[deployable!]] rejects them before remote deployment. Maven versions remain
  unchanged, even when the checkout contains a newer API."
  [available module]
  (into (sorted-map)
        (map (fn [[lib coord]]
               (if-let [local-root (:local/root coord)]
                 (let [dir    (.getCanonicalPath (io/file (:dir module) local-root))
                       target (some #(when (= dir (.getCanonicalPath (io/file (:dir %)))) %)
                                    available)]
                   (when-not (= lib (get-in target [:project :name]))
                     (throw (ex-info "Local dependency does not match a release module"
                                     {:module (:id module) :lib lib :coordinate coord})))
                   [lib (-> coord
                            (dissoc :local/root)
                            (assoc :mvn/version (get-in target [:project :version])))])
                 (if (seq (:mvn/version coord))
                   [lib coord]
                   (throw (ex-info "Publication requires Maven dependencies"
                                   {:module (:id module) :lib lib :coordinate coord}))))))
        (get-in module [:deps :deps])))

(defn deployable!
  "Rejects local/Git dependencies and snapshot versions before deployment."
  [module]
  (when (or (str/includes? (get-in module [:project :version]) "SNAPSHOT")
            (some (fn [[_ coord]]
                    (or (not (seq (:mvn/version coord)))
                        (:local/root coord) (:git/url coord) (:git/sha coord)
                        (str/includes? (:mvn/version coord) "SNAPSHOT")))
                  (get-in module [:deps :deps])))
    (throw (ex-info "Deployment requires released Maven dependencies; publish the API and switch local roots first"
                    {:module (:id module) :file (:deps-file module)})))
  module)

(defn release-notes
  "Returns the changelog body for a module's configured version.

  The section must have a dated `== VERSION - YYYY-MM-DD` heading.
  The heading and surrounding blank lines are excluded."
  [{:keys [project changelog]}]
  (let [heading (re-pattern (str "== " (java.util.regex.Pattern/quote (:version project))
                                 " - \\d{4}-\\d{2}-\\d{2}"))
        lines   (str/split-lines (slurp changelog))
        section (->> lines
                     (drop-while #(not (re-matches heading %)))
                     rest
                     (take-while #(not (str/starts-with? % "== ")))
                     (str/join "\n")
                     str/trim)]
    (when (str/blank? section)
      (throw (ex-info "Missing dated changelog entry" {:file changelog :version (:version project)})))
    (str section "\n")))

(defn set-versions!
  "Sets every releasable module to `version`; dependency versions stay unchanged.

  This explicit operation rewrites module EDN formatting. Builds never call it."
  [root version]
  (when-not (and (string? version) (re-matches #"\d+\.\d+\.\d+" version))
    (throw (ex-info "Expected a release version such as 0.0.1" {:version version})))
  (doseq [{:keys [deps-file deps]} (modules root)]
    (spit deps-file (with-out-str
                      (pprint/pprint (assoc-in deps [:aliases :neil :project :version] version))))))
