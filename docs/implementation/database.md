# Database Implementation

## Overview

The database architecture of Blaze is influenced by [Datomic][1] and [Crux][2]. Both databases have a strong foundation in functional programming and [persistent data structures][3] leading to immutable databases.

## Immutable Database

The idea behind an immutable database is, that the whole database content at a point in time acts like one big immutable value. The database itself evolves over time by transitioning database values through transactions. The time is modelled explicitly, assigning each database value a value `t` which increases monotonously.

```
+-------------+                   +-------------+
| Value (t=1) | -> Transaction -> | Value (t=2) |
+-------------+                   +-------------+
```

Database values are not copied entirely from one version to the next. Instead, like in persistent data structures, structural sharing is used. As such each database value can be seen as a complete copy of the database from the outside, but at the inside, the implementation is efficient enough to be feasible nowadays.

**Note:** In contrast, relational databases, which where designed in the 80's, use an update in-place model, because storage was expensive then.

A similar technic is [copy-on-write][4], used in many areas of computing like modern filesystems.

Having a database architecture which consists of immutable database values evolving over time, were past values are kept either forever or at least sufficiently long, has the property that reads don't need coordination. Because database values can be referenced by `t`, an arbitrarily number queries, spread over arbitrarily periods of time, can access the same immutable snapshot of the database. In case data is replicated over multiple nodes, queries can even run in parallel, all accessing the same coherent database value.

As one example, in paging of FHIR searchset or history bundles, Blaze simply refers to the database value's `t` of the first page, in order to calculate every remaining page based on the same stable database value.

In practise, each FHIR RESTful API read request will obtain the newest known database value and use that for all queries necessary to answer the request.

## Logical Data Model

Datomic uses a fact based data model, were each fact is the triple `(entity, attribute, value)` for example `(<patient-id>, birthDate, 2020)`. This model has one big advantage, the minimum change between two database values will be a fact, which is quite small. The disadvantage is, that bigger structures like resources have to be reconstructed from individual facts. In addition to that, because in FHIR, updates are always whole resources, the actual changed facts have to be determined by diffing the old and new resource.

Another data model is the document model used in document stores like [MongoDB][5] but also Crux. With documents, the smallest structure in the database is a document. Blaze uses that document data model were each resource version is a document. Because Blaze uses resource versions as documents, instead of only the current version, documents can't be indexed by resource identifier. Instead, as Crux does, content hashes will be used. Aside the version based document store, several indices are used to build up the database values and enable queries. 

## Indices

There are two different sets of indices, ones which depend on the database value point in time `t` and the ones which only depend on the content hash of resource versions. The former indices are used to build up the database values were the latter are used to enable queries based on search parameters.

### Indices depending on t

| Name | Key Parts | Value |
|---|---|---|
| ResourceAsOf | type id t | content-hash, num-changes, op |
| TypeAsOf | type t id | content-hash, num-changes, op |
| SystemAsOf | t type id | content-hash, num-changes, op |
| TxSuccess | t | instant |
| TxError | t | anomaly |
| TByInstant | instant | t |
| TypeStats | type t | total, num-changes |
| SystemStats | t | total, num-changes |

#### ResourceAsOf

The `ResourceAsOf` index is the primary index which maps the resource identifier `(type, id)` together with the `t` to the `content-hash` of the resource version. In addition to that, the index contains the number of changes `num-changes` to the resource and the operator `op` of the change leading to the index entry.

The `ResourceAsOf` index is used to access the version of a resource at a particular point in time `t`. In other words, given a point in time `t`, the database value with that `t`, allows to access the resource version at that point in time by its identifier. Because the index only contains entries with `t` values of changes to each resource, the most current resource version is determined by querying the index for the greatest `t` less or equal to the `t` of the database value.

##### Example 

The following `ResourceAsOf` index:

| Key (type, id, t) | Value (content-hash, num-changes, op) |
|---|---|
| Patient, 0, 1 | ba9c9b24, 1, create |
| Patient, 0, 3 | b7e3e5f8, 2, update |
| Patient, 1, 2 | 6744ed32, 1, create |
| Patient, 0, 4 | -, 3, delete |

provides the basis for the following database values:

| t | type | id | content-hash |
|---|---|---|---|
| 1 | Patient | 0 | ba9c9b24 |
| 2 | Patient | 0 | ba9c9b24 | 
| 2 | Patient | 1 | 6744ed32 |
| 3 | Patient | 0 | b7e3e5f8 |
| 3 | Patient | 1 | 6744ed32 |
| 4 | Patient | 1 | 6744ed32 |

The database value with `t=1` contains one patient with `id=0` and content hash `ba9c9b24`, because the second patient was created later at `t=2`. The index access algorithm will not find an entry for the patient with `id=1` on a database value with `t=1` because there is no index key with `type=Patient`, `id=1` and `t<=1`. However, the database value with `t=2` will contain the patient with `id=1` and additionally contains the patient with `id=0` because there is a key with `type=Patient`, `id=0` and `t<=2`. Next, the database value with `t=3` still contains the same content hash for the patient with `id=1` and reflects the update on patient with `id=0` because the key `(Patient, 0, 3)` is now the one with the greatest `t<=3`, resulting in the content hash `b7e3e5f8`. Finally, the database value with `t=4` doesn't contain the patient with `id=0` anymore, because it was deleted. As can be seen in the index, deleting a resource is done by adding the information that it was deleted at some point in time.

In addition to direct resource lookup, the `ResourceAsOf` index is used for listing all versions of a particular resource, listing all resources of a particular type and listing all resources at all. Listings are done by scanning through the index and for the non-history case, skipping versions not appropriate for the `t` of the database value. 

#### TypeAsOf

The `TypeAsOf` index contains the same information as the `ResourceAsOf` index with the difference that the components of the key are ordered `type`,  `t` and  `id` instead of `type`, `id` and `t`. The index is used for listing all versions of all resources of a particular type. Such history listings start with the `t` of the database value going into the past. This is done by not only choosing the resource version with the latest `t` less or equal the database values `t` but instead using all older versions. Such versions even include deleted versions because in FHIR it is allowed to bring back a resource to a new life after it was already deleted. The listing is done by simply scanning through the index in reverse. Because the key is ordered by `type`,  `t` and  `id`, the entries will be first ordered by time, newest first, and second by resource identifier.

#### SystemAsOf

In the same way the `TypeAsOf` index uses a different key ordering in comparison to the `ResourceAsOf` index, the `SystemAsOf` index will use the key order `t`, `type` and `id` in order to provide a global time axis order by resource type and by identifier secondarily.

#### TxSuccess

The `TxSuccess` index contains the real point in time, as `java.time.Instant`, successful transactions happened. In other words, this index maps each `t` which is just a monotonically increasing number to a real point in time. 

**Note:** Other than Crux, Blaze is not a bitemporal. That means the time recorded in the history of resources is the transaction time, not a business time. That also means that one can't fix the history, because the history only reflects the transactions happened. 

#### TxError

The `TxError` index will keep track of all failed transactions. TODO: explain why

#### TByInstant

The `TByInstant` index is used to determine the `t` be a real point in time. This functionality is needed to support the `since` parameter in history queries.

#### TypeStats

The `TypeStats` index keeps track of the total number of resources, and the number of changes to resources of a particular type at a particular point in time `t`. This statistic is used to populate the total count property in type listings and type history listings in case there are no filters applied.

#### SystemStats

The `SystemStats` index keeps track of the total number of resources, and the number of changes to all resources at a particular point in time `t`. This statistic is used to populate the total count property in system listings and system history listings in case there are no filters applied.

### Indices not depending on t

The indices not depending on `t` directly point to the resource versions by their content hash. 

| Name | Key Parts | Value |
|---|---|---|
| SearchParamValueResource | search-param, type, value, id, content-hash | - |
| ResourceSearchParamValue | type, id, content-hash, search-param, value | - |
| CompartmentSearchParamValueResource | co-c-hash, co-res-id, sp-c-hash, tid, value, id, hash-prefix | - |
| CompartmentResource | co-c-hash, co-res-id, tid, id | - |
| SearchParam | code, tid | id |
| ActiveSearchParams | id | - |

#### SearchParamValueResource

The `SearchParamValueResource` index contains all values from resources that are reachable from search parameters. The components of its key are:
* `search-param` - a 4-byte hash of the search parameters code used to identify the search parameter
* `type` - a 4-byte hash of the resource type
* `value` - the encoded value of the resource reachable by the search parameters FHIRPath expression. The encoding depends on the search parameters type.
* `id` - the logical id of the resource
* `content-hash` - a 4-byte prefix of the content-hash of the resource version

The way the `SearchParamValueResource` index is used, depends on the type of the search parameter. The following sections will explain this in detail for each type:

##### Number

**TODO: continue...**

##### Date/DateTime

Search parameters of type `date` are used to search in data elements with date/time and period data types. All date types stand for an interval of a start point in time up to an end point in time. So date search uses interval arithmetic to find hits. For each value of a resource, both the lower bound and the upper bound are stored in the index with different prefixes. For the different modifier, the search works the following way:

###### Equal (eq)

The value interval `v` has to intersect with the query interval `q`. In order to do so, the following must hold:
* the lower bound or upper bound of `v` is in `q`, or
* `v` completely encloses `q`.

We start by scanning through the index with the lower bound prefix, starting at the lower bound of `q` and ending at the upper bound of `q`. After that, we repeat the same process with the upper bound prefix, not adding any duplicates.

###### Less Than or Equal

**TODO: continue...**

##### String

**TODO: continue...**

##### Token

Search parameters of type `token` are used for exact matches of terminology codes or identifiers potentially scoped by a URI which represents the code system or naming system.

In order to facilitate different forms of searches specified in the [FHIR Spec][6], the value is encoded in the following 4-ways:
* `code` - the code without system
* `system|code` - the code scoped by the system
* `|code` - the code if the resource doesn't specify a system
* `system|` - the system independent of the code, used to find all resources with any code in that system

After concatenation, the strings are hashed with the [Murmur3][7] algorithm in its 32-bit variant, yielding a 4-byte wide value. The hashing is done to save space and ensure that all values are of the same length.

###### Example

For this example, we don't use the hashed versions of the key parts except for the content-hash.

| Key (search-param, type, value, id, content-hash) |
|---|
| gender, Patient, female, 1, 6744ed32 |
| gender, Patient, female, 2, b7e3e5f8 |
| gender, Patient, male, 0, ba9c9b24 |

In case one searches for female patients, Blaze will seek into the index with the key prefix (gender, Patient, female) and scan over it while the prefix stays the same. The result will be the `[id, hash]` tuples:
* `[1, 6744ed32]` and
* `[2, b7e3e5f8]`.

That tuples are further processed against the `ResourceAsOf` index in order to check whether the resource versions are valid regarding to the current `t`.

##### Reference

**TODO: continue...**

##### Composite

**TODO: continue...**

##### Quantity

**TODO: continue...**

##### URI

**TODO: continue...**

##### Special

**TODO: continue...**

## Transaction Handling

* a transaction bundle is POST'ed to one arbitrary node
* this node submits the transaction commands to the central transaction log
* all nodes (inkl. the transaction submitter) receive the transaction commands from the central transaction log

**TODO: continue...**

[1]: <https://www.datomic.com>
[2]: <https://opencrux.com/main/index.html>
[3]: <https://en.wikipedia.org/wiki/Persistent_data_structure>
[4]: <https://en.wikipedia.org/wiki/Copy-on-write>
[5]: <https://www.mongodb.com>
[6]: <https://www.hl7.org/fhir/search.html#token>
[7]: <https://en.wikipedia.org/wiki/MurmurHash>
