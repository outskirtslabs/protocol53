(ns ol.protocol53.cloudflare-txt-test
  (:require
   [fulcro-spec.core :refer [=> =throws=> assertions behavior specification]]
   [ol.protocol53.cloudflare :as cloudflare]
   [ol.protocol53.cloudflare-txt-fixtures :as fixtures]))

(specification "Cloudflare TXT presentation"
  (doseq [{:keys [label data content]} fixtures/encoding-cases]
    (behavior label
      (assertions
        "encodes portable data for Cloudflare"
        (#'cloudflare/encode-txt data) => content)))

  (doseq [{:keys [label data content]} fixtures/decoding-cases]
    (behavior label
      (assertions
        "decodes Cloudflare content to portable data"
        (#'cloudflare/decoded-txt content) => data)))

  (doseq [{:keys [label content]} fixtures/malformed-contents]
    (behavior label
      (assertions
        "is rejected"
        (#'cloudflare/decoded-txt content) =throws=> #"invalid response")))

  (doseq [{:keys [label data]} fixtures/round-trip-values]
    (behavior label
      (assertions
        "round-trips byte exactly"
        (#'cloudflare/decoded-txt (#'cloudflare/encode-txt data)) => data))))
