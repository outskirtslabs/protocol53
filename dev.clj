(ns dev
  (:require
   [clj-reload.core :as clj-reload]
   [ol.dev.portal :as portal]))

((requiring-resolve 'hashp.install/install!))

;; Configure the paths containing clojure sources we want clj-reload to reload
(clj-reload/init {:dirs      ["api/src/main"
                              "api/src/test"
                              "dev"
                              "providers/cloudflare/src/main"
                              "providers/cloudflare/src/test"
                              "testkit/src/main"
                              "testkit/src/test"]
                  :no-reload #{'user 'dev 'ol.dev.portal}})

(set! *warn-on-reflection* true)

(defonce portal! (portal/open-portals))

(comment
  (portal/logs 5)
  (portal/last-log)
  (portal/clear-logs!)

  (clj-reload/reload)
  (clj-reload/reload {:only :all}) ;; rcf
  (clojure.repl.deps/sync-deps)
  ;;;
  )

