(ns ol.protocol53.desec-txt-test
  (:require
   [fulcro-spec.core :refer [=> =throws=> assertions behavior specification]]
   [ol.protocol53.desec :as desec]))

(specification "deSEC TXT presentation"
  (behavior "encodes portable text as one quoted DNS character string"
    (assertions
      (#'desec/encode-txt "") => "\"\""
      (#'desec/encode-txt "a\"b\\c") => "\"a\\\"b\\\\c\""
      (#'desec/encode-txt "café") => "\"caf\\195\\169\""
      (#'desec/encode-txt "x\u0000y") => "\"x\\000y\""))

  (behavior "decodes adjacent strings and DNS decimal escapes"
    (assertions
      (#'desec/decode-txt "\"hello \" \"caf\\195\\169\"") => "hello café"
      (#'desec/decode-txt "\"a\\\"b\\\\c\"") => "a\"b\\c"
      (#'desec/decode-txt "\"\"") => ""))

  (behavior "rejects malformed provider values"
    (assertions
      (#'desec/decode-txt "unquoted") =throws=> #"invalid response"
      (#'desec/decode-txt "\"unterminated") =throws=> #"invalid response"
      (#'desec/decode-txt "\"\\999\"") =throws=> #"invalid response"))

  (behavior "round-trips mixed portable text"
    (let [text "café has \"quotes\", \\slashes, and \u0000 bytes"]
      (assertions
        (#'desec/decode-txt (#'desec/encode-txt text)) => text))))
