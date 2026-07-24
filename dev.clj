(ns dev
  (:require
   [ol.dev.portal :as portal]))

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

