# Cloudflare provider

The Cloudflare provider implements all protocol53 operations with Cloudflare's
v4 API.

## Authentication

Use scoped API tokens, not legacy global API keys.

For one token, grant `Zone:Read` and `Zone.DNS:Write` across the zones you
manage:

```clojure
(require '[ol.protocol53.cloudflare :as cloudflare])

(def dns
  (cloudflare/provider {:api-token "token"}))
```

For separate read and write credentials, grant `Zone:Read` to `:zone-token`
and `Zone.DNS:Write` to `:api-token`:

```clojure
(def dns
  (cloudflare/provider {:api-token  "dns-write-token"
                        :zone-token "zone-read-token"}))
```

Pass `:http-client` to use a client accepted by `babashka.http-client`.
