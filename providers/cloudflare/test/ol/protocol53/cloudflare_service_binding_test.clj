(ns ol.protocol53.cloudflare-service-binding-test
  (:require
   [fulcro-spec.core :refer [=> assertions behavior specification]]
   [ol.protocol53.cloudflare :as cloudflare]))

(specification "Cloudflare service-binding presentation"
  (doseq [[label type content expected]
          [["removes redundant quotes around an ALPN list"
            "HTTPS"
            "1 svc.example.net. alpn=\"h2,h3\""
            "1 svc.example.net. alpn=h2,h3"]
           ["removes redundant quotes around a single ALPN value"
            "SVCB"
            "1 . alpn=\"dot\""
            "1 . alpn=dot"]
           ["preserves quotes required by whitespace"
            "HTTPS"
            "1 svc.example.net. dohpath=\"/dns query\""
            "1 svc.example.net. dohpath=\"/dns query\""]
           ["preserves already portable content"
            "SVCB"
            "1 svc.example.net. port=853"
            "1 svc.example.net. port=853"]]]
    (behavior label
      (assertions
        (#'cloudflare/response-data {:content content} type)
        => expected))))
