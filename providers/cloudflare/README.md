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

## Live lifecycle suite
> [!WARNING]
> Run the lifecycle suite only against a dedicated disposable zone. It creates,
> replaces, and deletes DNS records. A provider or testkit defect could affect
> records outside the reserved names.

Set these variables in the process environment or the repository `.env` file.
Process values take precedence over `.env` values.

| variable | required | description |
| --- | ---: | --- |
| `CLOUDFLARE_API_TOKEN` | yes | Scoped token with DNS write access. |
| `CLOUDFLARE_TEST_ZONE` | one of | Dedicated absolute zone name. |
| `CLOUDFLARE_DOMAIN` | one of | Alternative zone name; a trailing dot is added when absent. |
| `CLOUDFLARE_ZONE_TOKEN` | no | Separate scoped token with zone read access. |

`CLOUDFLARE_ZONE_ID` is not required because the provider discovers the zone
by name.
The wrapper sets `:skip-empty-txt? true` because Cloudflare rejects empty TXT
RDATA; it still runs ordinary and wildcard TXT lifecycle checks.

Run every discovered provider integration suite from the repository root:

```shell
bb test:integration
```

The runner discovers `providers/*/src/test-integration`. Cloudflare is skipped when
its credentials are missing; another provider failure does not stop the remaining
suites. Cloudflare creates and deletes records whose effective
owner starts with `p53test-`. Integration tests do not run from `bb test`,
`bb qa`, or `bb ci`. Do not run concurrent lifecycle suites against one zone.
