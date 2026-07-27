# Protocol53

Protocol53 provides a common language for managing DNS Zones and Records across DNS Services. It defines a portable core while allowing DNS Services to differ in capabilities and constraints.

## Language

### Services and Zones

**DNS Service**:
An external service or system that manages DNS Zones.
_Avoid_: Provider

**Provider**:
A configured protocol53 integration through which callers access one DNS Service.

**Provider Capability**:
One of five independently supported operations: Get Records, Append Records, Set Records, Delete Records, or List Zones.

**Zone**:
A portion of the DNS namespace managed as one unit by a DNS Service.
_Avoid_: Domain

**Zone Name**:
The absolute DNS name that identifies a Zone.

**Zone Apex**:
The DNS name at the root of a Zone; it is the same absolute name as the Zone Name.

### Records

**Record**:
A provider-independent DNS resource record within a Zone, described by its Record Name, type, TTL, and data.

**Record Name**:
The Zone-relative DNS name where a Record exists; `@` denotes the Zone Apex.

**RRset**:
All Records in a Zone that share the same Record Name and type.

**Record Selector**:
Criteria identifying Records to delete. A Record Name is required; type, TTL, and data may narrow the match, while omitted fields match any value.

**Stored Record**:
A Record as a DNS Service persisted it after applying its constraints. It may differ from the supplied Record, such as when the DNS Service enforces a minimum TTL.

### Operations

**Get Records**:
A read-only operation that returns every Record in one Zone.

**List Zones**:
A read-only operation that returns the Zones a Provider can use for Record operations.

**Mutation**:
Any Append Records, Set Records, or Delete Records operation.

**Append Records**:
A Mutation that adds supplied Records while preserving all existing Records.

**Set Records**:
A Mutation that makes supplied Records the complete contents of every RRset they address while preserving all other RRsets.
_Avoid_: Upsert

**Delete Records**:
A Mutation that removes Records matched by Record Selectors; a selector matching nothing is a successful no-op.

**Failed Mutation State**:
A classification of whether a failed Mutation changed the Zone: unchanged means it certainly did not, while unknown means it may have.
