# protocol53 provider lifecycle testkit

`com.outskirtslabs/protocol53-testkit` checks a DNS provider against the shared
protocol53 lifecycle contract.

> [!WARNING]
> Run this suite only against a dedicated disposable zone. It creates, replaces,
> and deletes DNS records. A provider or testkit defect could delete records
> outside the reserved names.

## Usage

```clojure
(ns example.provider-lifecycle-test
  (:require
   [example.provider :as provider]
   [fulcro-spec.core :refer [specification]]
   [ol.protocol53.testkit :as testkit]))

(specification "The example provider lifecycle"
  (testkit/run!
   {:provider            (provider/provider provider-options)
    :zone                "example.com."
    :allow-live-changes? true}))
```

The suite calls only the public `ol.protocol53` facade. It runs zone listing
when available, followed by get, append, set, delete, wildcard-delete, and
cleanup phases. Providers without `ZoneLister` skip only zone listing.

## Configuration

| key                    | required | default    | description                                               |
|------------------------|---------:|------------|-----------------------------------------------------------|
| `:provider`            |      yes | —          | Provider implementing all four record capabilities.       |
| `:zone`                |      yes | —          | Dedicated absolute zone name ending in `.`.               |
| `:allow-live-changes?` |      yes | —          | Must equal `true` before any provider call.               |
| `:timeout`             |       no | 30 seconds | Fresh `java.time.Duration` budget for each operation.     |
| `:skip-record-types`   |       no | `#{}`      | Nonessential record types to omit; matching ignores case. |
| `:skip-empty-txt?`     |       no | `false`    | Omit exact-empty TXT checks when a provider rejects them. |
| `:ignore-ttl?`         |       no | `false`    | Ignore provider-normalized TTL differences.               |
| `:expect-empty-zone?`  |       no | `false`    | Reject active non-`NS` test record types after cleanup.   |

The suite always tests `A`, `CNAME`, and `TXT`; these types cannot be skipped.
Other portable corpus types are `AAAA`, `CAA`, `HTTPS`, `MX`, `NS`, `SRV`, and
`SVCB`.
Set `:skip-empty-txt?` only when the provider cannot create empty TXT RDATA;
ordinary TXT and wildcard TXT deletion remain required.

Lifecycle record comparisons check `:ttl` by default. Set `:ignore-ttl? true`
only for providers such as deSEC that normalize or clamp requested values.
Provider-specific tests should still verify TTL behavior.

## Development `.env` files

`ol.protocol53.testkit.dotenv/load-env-file` reads simple dotenv assignments
without adding a runtime dependency. It supports blank lines, comments, optional
`export`, quoted values, embedded `=`, and both JVM Clojure and Babashka.
Process environment values should take precedence when callers merge both
sources.

## Cleanup and isolation

Every generated effective owner starts with `p53test-`. `run!` removes stale
reserved records before testing and cleans after every mutation phase. It runs
phases sequentially. Do not run two lifecycle suites concurrently against one
zone.

Use `cleanup!` to recover after an interrupted run:

```clojure
(testkit/cleanup!
 {:provider            provider
  :zone                "example.com."
  :allow-live-changes? true})
```

`cleanup!` deletes only records whose effective owner starts with `p53test-`,
then reads the zone again to verify their absence.

## Scope

The testkit checks provider-visible lifecycle behavior and portable record
round trips. It does not check authentication, HTTP payloads, pagination,
provider error decoding, retries, deadlines inside a transport, or
provider-specific TXT codecs. Keep those checks in the provider project.
