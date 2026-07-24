(ns ol.protocol53.testkit.dotenv
  "Minimal `.env` file loading for development lifecycle suites."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- unquote-doublequoted-string [string]
  (-> string
      (str/replace #"^\"|\"$" "")
      (str/replace #"\\\"" "\"")))

(defn- unquote-singlequoted-string [string]
  (-> string
      (str/replace #"^'|'$" "")
      (str/replace #"\\'" "'")))

(defn- unquote-string [string]
  (cond
    (str/starts-with? string "\"") (unquote-doublequoted-string string)
    (str/starts-with? string "'") (unquote-singlequoted-string string)
    :else string))

(defn- pairs [contents]
  (->> contents
       str/split-lines
       (map str/trim)
       (remove #(or (str/blank? %)
                    (str/starts-with? % "#")))
       (map #(str/split % #"=" 2))
       (map (fn [[head & tail]]
              [(str/replace head #"^export\s*" "")
               (str/join "=" tail)]))
       (map (fn [pair]
              (mapv (comp unquote-string str/trim) pair)))))

(defn load-env-file
  "Loads assignments from `filename` into a string map.

  Returns an empty map when `filename` does not name an existing file."
  [filename]
  (if (.isFile (io/file filename))
    (into {} (pairs (slurp filename)))
    {}))
