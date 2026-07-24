(ns ol.protocol53.testkit.dotenv-test
  (:require
   [clojure.string :as str]
   [fulcro-spec.core :refer [=> assertions behavior specification]]))

(defn- load-env-file [filename]
  (if-let [loader (try
                    (requiring-resolve
                     'ol.protocol53.testkit.dotenv/load-env-file)
                    (catch Throwable _cause
                      nil))]
    (loader filename)
    ::missing))

(defn- with-env-file [contents f]
  (let [file (java.io.File/createTempFile "protocol53-dotenv-" ".env")]
    (try
      (spit file contents)
      (f (.getPath file))
      (finally
        (.delete file)))))

(specification "The testkit dotenv reader"
  (behavior "returns an empty map when the file does not exist"
    (assertions
      (load-env-file "/no/such/protocol53-dotenv-file") => {}))

  (behavior "reads assignments without losing value content"
    (let [contents (str/join
                    "\n"
                    ["# ignored"
                     ""
                     " TOKEN = plain "
                     "export DOMAIN = example.com"
                     "EQUALS=first=second"
                     "EMPTY="
                     "NO_EQUALS"
                     "HASH=value # retained"])]
      (assertions
        (with-env-file contents load-env-file)
        => {"TOKEN"     "plain"
            "DOMAIN"    "example.com"
            "EQUALS"    "first=second"
            "EMPTY"     ""
            "NO_EQUALS" ""
            "HASH"      "value # retained"})))

  (behavior "unquotes matching quote styles"
    (let [contents (str/join
                    "\n"
                    ["DOUBLE=\"say \\\"hello\\\"\""
                     "SINGLE='it\\'s fine'"
                     "SPACED=\"  padded  \""])]
      (assertions
        (with-env-file contents load-env-file)
        => {"DOUBLE" "say \"hello\""
            "SINGLE" "it's fine"
            "SPACED" "  padded  "})))

  (behavior "uses the last assignment for duplicate names"
    (assertions
      (with-env-file "VALUE=old\nVALUE=new" load-env-file)
      => {"VALUE" "new"})))
