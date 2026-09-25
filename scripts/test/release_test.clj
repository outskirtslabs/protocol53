(ns release-test
  (:require
   [build :as build]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [fulcro-spec.core :refer [=> =throws=> assertions behavior specification]]
   [ol.protocol53.release :as release])
  (:import
   [java.util.jar JarFile]))

(defn- child [node tag]
  (first (filter #(= tag (:tag %)) (:content node))))

(defn- text-at [node & tags]
  (first (:content (reduce child node tags))))

(defn- pom-deps [pom]
  (into (sorted-map)
        (map (fn [dep]
               [(symbol (text-at dep :groupId) (text-at dep :artifactId))
                (text-at dep :version)]))
        (:content (child pom :dependencies))))

(specification "Artifact release metadata"
  (let [modules (release/modules ".")]
    (behavior "discovers API and providers without publishing testkit"
      (assertions
        (mapv :id modules) => [:api :cloudflare :desec :godaddy :porkbun]
        (mapv :id (release/selected modules [:porkbun :api :porkbun])) => [:api :porkbun]
        (release/selected modules [:cloudflrae]) =throws=> #"Select known artifacts"
        (release/selected modules []) =throws=> #"Select known artifacts"))
    (behavior "derives independent tag versions from Maven names"
      (assertions
        (release/release-tag {:name 'com.outskirtslabs/protocol53 :version "0.0.1"})
        => "p53/v0.0.1"
        (release/release-tag {:name 'com.outskirtslabs/protocol53-cloudflare :version "0.0.2"})
        => "p53-cloudflare/v0.0.2"))
    (behavior "translates local dependencies only for build rehearsals"
      (let [modules  (assoc-in modules [0 :project :version] "0.0.3")
            provider (-> (second modules)
                         (assoc-in [:project :version] "0.0.7")
                         (assoc-in [:deps :deps 'com.outskirtslabs/protocol53]
                                   {:local/root "../../api"}))]
        (assertions
          (release/publication-deps modules provider)
          => {'com.outskirtslabs/protocol53 {:mvn/version "0.0.3"}
              'org.babashka/json            {:mvn/version "0.1.6"}}
          (release/deployable! provider) =throws=> #"switch local roots first")))
    (behavior "preserves the API dependency version on a provider-only release"
      (let [provider (-> (second modules)
                         (assoc-in [:project :version] "0.0.7")
                         (assoc-in [:deps :deps 'com.outskirtslabs/protocol53]
                                   {:mvn/version "0.0.1" :exclusions ['example/excluded]}))]
        (assertions
          (release/publication-deps modules provider)
          => {'com.outskirtslabs/protocol53 {:mvn/version "0.0.1"
                                             :exclusions  ['example/excluded]}
              'org.babashka/json            {:mvn/version "0.1.6"}}
          (release/deployable! provider) => provider)))
    (behavior "rejects unpublishable dependencies instead of omitting them"
      (let [provider (second modules)]
        (assertions
          (release/publication-deps modules
                                    (assoc-in provider [:deps :deps 'unknown/local]
                                              {:local/root "../../testkit"}))
          =throws=> #"does not match"
          (release/publication-deps modules
                                    (assoc-in provider [:deps :deps 'unknown/git]
                                              {:git/url "https://example.com/repo" :git/sha "abc"}))
          =throws=> #"requires Maven"
          (release/deployable! (assoc-in (first modules) [:project :version] "0.0.2-SNAPSHOT"))
          =throws=> #"released Maven")))))

(specification "Explicit release preparation"
  (behavior "changes module versions without changing dependencies"
    (let [root (doto (io/file (System/getProperty "java.io.tmpdir") (str "p53-versions-" (random-uuid)))
                 .mkdirs)]
      (try
        (io/copy (io/file "deps.edn") (io/file root "deps.edn"))
        (doseq [{:keys [dir deps-file]} (release/modules ".")]
          (let [target (io/file root dir "deps.edn")]
            (io/make-parents target)
            (io/copy (io/file deps-file) target)))
        (release/set-versions! root "0.0.8")
        (assertions
          (mapv #(get-in % [:project :version]) (release/modules root))
          => (vec (repeat 5 "0.0.8"))
          (mapv #(get-in % [:deps :deps]) (release/modules root))
          => (mapv #(get-in % [:deps :deps]) (release/modules "."))
          (release/set-versions! root "TODO") =throws=> #"Expected a release version")
        (finally
          (doseq [file (reverse (file-seq root))] (io/delete-file file))))))
  (behavior "extracts only the selected dated changelog section"
    (let [file (java.io.File/createTempFile "p53-changelog-" ".adoc")]
      (try
        (spit file "= Changelog\n\n== UNRELEASED\n\n* Next\n\n== 0.0.2 - 2026-09-25\n\n* Fix\n\n== 0.0.1 - 2026-09-24\n\n* Initial\n")
        (assertions
          (release/release-notes {:project {:version "0.0.2"} :changelog file}) => "* Fix\n"
          (release/release-notes {:project {:version "0.0.3"} :changelog file})
          =throws=> #"Missing dated changelog entry")
        (finally (io/delete-file file))))))

(specification "Release jars"
  (let [modules (release/modules ".")
        stale   (io/file "target/protocol53/classes/stale.clj")]
    (io/make-parents stale)
    (spit stale "stale")
    (doseq [{:keys [lib version jar-file pom-file project src-dirs] :as artifact}
            (build/jar {:artifacts :all})]
      (behavior (str "package only the declared sources for " lib)
        (with-open [jar (JarFile. jar-file)]
          (let [entries (->> (enumeration-seq (.entries jar))
                             (remove #(.isDirectory ^java.util.jar.JarEntry %))
                             (map #(.getName ^java.util.jar.JarEntry %))
                             set)
                sources (set (for [dir   src-dirs
                                   file  (file-seq (io/file dir))
                                   :when (.isFile file)]
                               (str (.relativize (.toPath (io/file dir)) (.toPath file)))))
                pom     (xml/parse pom-file)]
            (assertions
              (set (remove #(str/starts-with? % "META-INF/") (disj entries "LICENSE"))) => sources
              (contains? entries "LICENSE") => true
              (with-open [stream (.getInputStream jar (.getJarEntry jar "LICENSE"))]
                (slurp stream)) => (slurp "LICENSE")
              {:coordinate (symbol (text-at pom :groupId) (text-at pom :artifactId))
               :version    (text-at pom :version)
               :url        (text-at pom :url)
               :scm-url    (text-at pom :scm :url)
               :deps       (pom-deps pom)}
              => {:coordinate lib
                  :version    version
                  :url        (:url project)
                  :scm-url    (get-in project [:scm :url])
                  :deps       (into (sorted-map)
                                    (map (fn [[lib coord]] [lib (:mvn/version coord)]))
                                    (release/publication-deps modules artifact))}
              (text-at pom :scm :tag) => (or (System/getenv "GIT_REV")
                                             (str/trim (:out (shell/sh "git" "rev-parse" "HEAD")))))))))))
