# ol.protocol53

> One Clojure API for managing DNS records across providers.

`ol.protocol53` is a Clojure library for working with DNS services through a
consistent, provider-independent API. Applications can query and modify DNS
records without being tightly coupled to a particular vendor.

A provider is any service or system responsible for managing one or more DNS
zones.

The shared API is defined in the `api/` directory. It establishes the
operations and data structures that each provider implementation is expected to
support.

Provider-specific integrations live under `providers/`. Each integration
translates the common protocol53 operations into calls understood by its
corresponding DNS service.

The API supports:

* retrieving records from a zone
* adding new records
* creating or replacing records
* removing records
* listing available zones

Provider capabilities and limitations may vary, but applications can use the same core abstractions across all supported integrations.

See the [API documentation][docs] for complete usage details.

## Scope

`ol.protocol53` aims to cover the common DNS record operations, not every possible one.
DNS record types and provider APIs vary too much to fit 100% of cases into a provider-agnostic API, and that's fine.
The goal is to fully serve the ~99% of use cases you'll actually hit, not the last 1%.

## Available Providers

* [Cloudflare](providers/cloudflare)

## Example

TODO


## License

Copyright © 2026 Casey Link <casey@outskirtslabs.com>

Distributed under the [MIT](https://spdx.org/licenses/MIT.html).

[docs]: https://docs.outskirtslabs.com/ol.protocol53
