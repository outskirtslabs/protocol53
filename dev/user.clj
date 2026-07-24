(ns user
  (:require
   [clj-reload.core :as clj-reload]))

((requiring-resolve 'hashp.install/install!))

(set! *warn-on-reflection* true)

;; Configure the paths containing clojure sources we want clj-reload to reload
(clj-reload/init {:dirs      ["src" "dev" "test"]
                  :no-reload #{'user 'dev 'ol.dev.portal}})


