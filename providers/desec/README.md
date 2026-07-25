# deSEC provider

The [deSEC][desec] provider implements all [protocol53][protocol53] operations
with deSEC's v1 API.

## Authentication

Create a basic token through [deSEC's token management interface][desec-tokens].

```clojure
(require '[ol.protocol53.desec :as desec])

(def dns
  (desec/provider {:token "token"}))
```

Pass a caller-built `java.net.http.HttpClient` as `:http-client` when advanced
HTTP policy requires Java interop.

## TTL policy

A deSEC RRset cannot have a TTL below 3600 seconds. The provider always clamps
lower requested values to 3600 and returns the value deSEC stored. TTL belongs
to the complete RRset, so all records with the same owner and type share one
TTL. When setting an RRset whose input records disagree, the first record's TTL
is used.

## RRset and rate-limit behavior

Append and delete operations read the zone before applying one atomic bulk
write. As with other protocol53 providers, callers must coordinate concurrent
mutations of the same RRset. HTTP 429 responses are retried according to
deSEC's `Retry-After` header while the operation deadline permits.

## Integration Tests

> [!WARNING]
> Run the lifecycle suite only against a dedicated disposable zone. It creates,
> replaces, and deletes DNS records whose effective owner starts with
> `p53test-`.

Set these variables in the process environment or the repository `.env` file.
Process values take precedence over `.env` values.

| variable       | required | description                                          |
|----------------|---------:|------------------------------------------------------|
| `DESEC_TOKEN`  |      yes | Basic deSEC token with access to the test domain.    |
| `DESEC_DOMAIN` |      yes | Dedicated zone name, with or without a trailing dot. |

Run every configured provider integration suite from the repository root:

```shell
bb test:integration
```

The deSEC wrapper ignores TTL differences in the shared lifecycle comparison
because all fixture TTLs below 3600 are clamped. Provider-specific tests still
assert the exact 3600-second policy. Do not run concurrent lifecycle suites
against one zone.

[desec]: https://desec.io
[desec-tokens]: https://desec.io/tokens
[protocol53]: https://github.com/outskirtslabs/protocol53
