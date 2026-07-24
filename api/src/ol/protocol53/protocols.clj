(ns ol.protocol53.protocols
  "Provider capability extension points.")

(defprotocol RecordGetter
  "Retrieves all records in one zone."
  (-get-records! [provider zone opts]))

(defprotocol RecordAppender
  "Appends records without modifying existing records."
  (-append-records! [provider zone records opts]))

(defprotocol RecordSetter
  "Replaces the selected complete RRsets."
  (-set-records! [provider zone records opts]))

(defprotocol RecordDeleter
  "Deletes matching records and ignores misses."
  (-delete-records! [provider zone records opts]))

(defprotocol ZoneLister
  "Lists zones supported by record operations."
  (-list-zones! [provider opts]))
