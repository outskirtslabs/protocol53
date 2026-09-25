;; Copyright © 2026 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.tools.build.api :as b]
   [ol.protocol53.release :as release]))

(defn- git [& args]
  (let [{:keys [exit out err]} (apply shell/sh "git" args)]
    (when-not (zero? exit)
      (throw (ex-info "Git command failed" {:args args :error err})))
    (str/trim out)))

(defn- revision []
  (or (some-> (System/getenv "GIT_REV") str/trim not-empty)
      (git "rev-parse" "HEAD")))

(defn- build-jar! [available {:keys [project src-dirs] :as module} rev]
  (let [{lib :name :keys [version description url license scm]} project
        class-dir (str "target/" (name lib) "/classes")
        jar-file (str "target/" (name lib) "-" version ".jar")
        repo-url (:url scm)
        license-file (or (:file license) "LICENSE")
        ;; Source jars need direct dependency metadata, not a resolved classpath.
        basis {:libs      (release/publication-deps available module)
               :mvn/repos (get-in module [:deps :mvn/repos])}]
    (doseq [path (conj src-dirs license-file)]
      (when-not (.exists (io/file path))
        (throw (ex-info "Missing artifact input" {:module (:id module) :path path}))))
    (b/delete {:path class-dir})
    (b/copy-dir {:src-dirs src-dirs :target-dir class-dir})
    (b/copy-file {:src license-file :target (str class-dir "/LICENSE")})
    (b/write-pom {:class-dir class-dir
                  :lib       lib
                  :version   version
                  :basis     basis
                  :src-pom   :none
                  :src-dirs  (get-in module [:deps :paths])
                  :scm       {:url                 repo-url
                              :connection          (str "scm:git:" repo-url ".git")
                              :developerConnection (str "scm:git:" repo-url ".git")
                              :tag                 rev}
                  :pom-data  [[:description description]
                              [:url url]
                              [:licenses
                               [:license
                                [:name (:id license)]
                                [:url (or (:url license)
                                          (str repo-url "/blob/" rev "/" license-file))]]]]})
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (println "Built" jar-file)
    (assoc module :lib lib :version version :basis basis
           :class-dir class-dir :jar-file jar-file
           :pom-file (b/pom-path {:lib lib :class-dir class-dir}))))

(defn clean
  "Deletes the root `target` directory."
  [_]
  (b/delete {:path "target"}))

(defn jar
  "Builds source jars and returns their artifact metadata.

  | key | description |
  | --- | --- |
  | `:artifacts` | `:api` (default), `:providers`, `:all`, or a vector of module ids |

  Local module dependencies are translated to Maven versions for rehearsals.
  This does not make a module eligible for remote deployment."
  [{:keys [artifacts] :or {artifacts :api}}]
  (let [available (release/modules ".")
        selected  (release/selected available artifacts)
        rev       (revision)]
    (mapv #(build-jar! available % rev) selected)))

(defn install
  "Builds and installs selected artifacts into a local Maven repository.

  | key | description |
  | --- | --- |
  | `:artifacts` | Selection accepted by [[jar]]; defaults to `:api` |
  | `:local-repo` | Optional Maven repository directory for isolated rehearsals |"
  [{:keys [local-repo] :as opts}]
  (doseq [artifact (jar opts)]
    (b/install (cond-> (select-keys artifact [:lib :version :basis :jar-file :class-dir])
                 local-repo (assoc-in [:basis :mvn/local-repo] local-repo)))))

(defn- check-release! [module rev]
  (release/deployable! module)
  (release/release-notes module)
  (let [ref (str "refs/tags/" (:tag module))]
    (when-not (and (= "tag" (git "cat-file" "-t" ref))
                   (= rev (git "rev-parse" (str ref "^{commit}"))))
      (throw (ex-info "Expected an annotated artifact tag at HEAD" {:tag (:tag module)})))))

(defn deploy
  "Builds and deploys selected artifacts to Clojars, API first.

  | key | description |
  | --- | --- |
  | `:artifacts` | Selection accepted by [[jar]]; defaults to `:api` |

  Requires a clean checkout at each selected artifact's annotated release tag,
  released Maven dependencies, and Clojars credentials in the environment.
  Checks every selection and builds all jars before the first upload. No tags
  or GitHub releases are created. A failed upload stops the batch."
  [{:keys [artifacts] :or {artifacts :api}}]
  (let [available (release/modules ".")
        selected  (release/selected available artifacts)
        rev       (git "rev-parse" "HEAD")]
    (when-not (str/blank? (git "status" "--porcelain"))
      (throw (ex-info "Deploy from a clean tagged checkout" {})))
    (when-not (= rev (revision))
      (throw (ex-info "GIT_REV must match HEAD for deployment" {})))
    (doseq [module selected]
      (check-release! module rev))
    (let [built   (mapv #(build-jar! available % rev) selected)
          upload! (requiring-resolve 'deps-deploy.deps-deploy/deploy)]
      (doseq [{:keys [jar-file pom-file tag]} built]
        (println "Deploying" tag)
        (upload! {:installer :remote :artifact jar-file :pom-file pom-file})))))
