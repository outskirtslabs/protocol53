(ns ol.protocol53.protocols)

(defprotocol RecordGetter
  "Retrieves all records in one zone."
  (-get-records! [provider opts zone]))

(defprotocol RecordAppender
  "Appends records without modifying existing records."
  (-append-records! [provider opts zone records]))

(defprotocol RecordSetter
  "Replaces the selected complete RRsets."
  (-set-records! [provider opts zone records]))

(defprotocol RecordDeleter
  "Deletes matching records and ignores misses."
  (-delete-records! [provider opts zone records]))

(defprotocol ZoneLister
  "Lists zones supported by record operations."
  (-list-zones! [provider opts]))
