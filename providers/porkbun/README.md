# Porkbun provider

The [Porkbun][porkbun] provider implements all [protocol53][protocol53]
operations with Porkbun's v3 API.

## Authentication

Enable API access for the Porkbun account and each domain the provider should
manage, then create an API key and secret key.

```clojure
(require '[ol.protocol53.porkbun :as porkbun])

(def dns
  (porkbun/provider {:api-key    "pk1_..."
                     :secret-key "sk1_..."}))
```

Pass a caller-built `java.net.http.HttpClient` as `:http-client` when advanced
HTTP policy requires Java interop.

## Record behavior

Porkbun derives its minimum record TTL from account settings; 600 seconds is the
typical floor. The provider raises lower requests to 600, then rereads the zone
after append and set operations so it returns the TTL Porkbun actually stored.
MX and SRV priorities are translated between protocol53's portable record data
and Porkbun's separate `prio` field.

Set operations validate every desired value with Porkbun's `dryRun` mode before
writing. Within each selected RRset they preserve matching records, edit
replacements by ID, create surplus values before deleting stale ones, and stop
before later RRsets if a write fails. Delete operations first retrieve the zone
and then remove matching records by ID, so partial selectors and multi-value
RRsets retain protocol53 semantics. An error after any dispatched write reports
`:zone-state :unknown`; reread the zone before reconciliation. Callers
must coordinate concurrent mutations of the same RRset.

## Integration Tests

> [!WARNING]
> Run the lifecycle suite only against a dedicated disposable zone. It creates,
> replaces, and deletes DNS records whose effective owner starts with
> `p53test-`.

Set these variables in the process environment or the repository `.env` file.
Process values take precedence over `.env` values.

| variable             | required | description                                      |
|----------------------|---------:|--------------------------------------------------|
| `PORKBUN_API_KEY`    |      yes | Porkbun API key with access to the test domain.  |
| `PORKBUN_SECRET_KEY` |      yes | Secret key paired with the API key.              |
| `PORKBUN_DOMAIN`     |      yes | Dedicated zone, with or without a trailing dot.  |

Run the Porkbun lifecycle suite from the repository root:

```shell
bb test:integration --provider porkbun
```

The wrapper ignores TTL differences because Porkbun applies the test account's
minimum to fixture TTLs. Do not run concurrent lifecycle suites against one
zone.

[porkbun]: https://porkbun.com
[protocol53]: https://github.com/outskirtslabs/protocol53
