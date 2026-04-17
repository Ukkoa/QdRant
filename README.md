# QdRant

An R client for [Qdrant](https://qdrant.tech/) — the high-performance vector database.

> Work in progress. Contributions welcome! =)

---

## Installation

```r
# From GitHub
devtools::install_github("Ukkoa/QdRant", subdir = "QdRant")

# Or from a local clone
devtools::install("path/to/QdRant/QdRant")
```

---

## Quick Start

```r
library(QdRant)

# Local Qdrant (default: localhost:6333)
client <- QdrantClient$new()

# Qdrant Cloud
client <- QdrantClient$new(
  host    = "your-cluster-id.us-east-1-1.aws.cloud.qdrant.io",
  port    = 6333,
  api_key = Sys.getenv("QDRANT_API_KEY")
)

# Create helper objects
col  <- Collections$new(client)
pts  <- Points$new(client)
srch <- Search$new(client)
idx  <- Indexes$new(client)
als  <- Aliases$new(client)
snap <- Snapshots$new(client)
svc  <- Service$new(client)
```

---

## Collections

```r
col <- Collections$new(client)

# Create — vectors_config sets the embedding size and distance metric
col$create_collection("my_col",
  vectors_config = list(size = 384, distance = "Cosine"))

# With sparse vectors (e.g. BM25 / SPLADE)
col$create_collection("my_col",
  vectors_config        = list(size = 384, distance = "Cosine"),
  sparse_vectors_config = list(sparse = list()))

# List / info / exists / delete
col$list_collections()
col$get_collection("my_col")
col$collection_exists("my_col")
col$delete_collection("my_col")

# Update HNSW / optimizer settings
col$update_collection("my_col",
  optimizers_config = list(indexing_threshold = 20000))

# Aliases are managed separately — see the Aliases section below
```

---

## Points

```r
pts <- Points$new(client)

# Upsert (insert or overwrite)
pts$upsert_points("my_col",
  ids      = c(1, 2, 3),
  vectors  = list(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6), c(0.7, 0.8, 0.9)),
  payloads = list(list(color = "red"), list(color = "blue"), list(color = "green"))
)

# Retrieve single / multiple points
pts$retrieve_point("my_col", id = 1)
pts$retrieve_points("my_col", ids = c(1, 2, 3))

# Scroll through all points (with optional filter)
pts$scroll_points("my_col", limit = 100)
pts$scroll_points("my_col",
  filter = list(must = list(list(key = "color", match = list(value = "red")))),
  limit  = 50
)

# Count points
pts$count_points("my_col")
pts$count_points("my_col",
  filter = list(must = list(list(key = "color", match = list(value = "blue")))))

# Payload operations
pts$set_payload("my_col",
  payload = list(color = "purple"),
  ids     = c(1))

pts$overwrite_payload("my_col",
  payload = list(color = "yellow"),
  ids     = c(1))

pts$delete_payload("my_col",
  keys = c("color"),
  ids  = c(1))

pts$clear_payload("my_col", ids = c(1, 2))

# Update / delete vectors
pts$update_vectors("my_col",
  points = list(list(id = 1, vector = c(0.9, 0.8, 0.7))))

pts$delete_vectors("my_col",
  ids          = c(1),
  vector_names = c("image"))

# Delete points
pts$delete_points_by_id("my_col", ids = c(2, 3))
pts$delete_points_by_filter("my_col",
  filter = list(must = list(list(key = "color", match = list(value = "red")))))

# Batch operations
pts$batch_update_points("my_col", operations = list(
  list(upsert = list(points = list(list(id = 10, vector = c(0.1, 0.2, 0.3)))))
))

# Payload facets (requires a keyword index on the field)
pts$payload_field_facets("my_col", key = "color", limit = 10L)
```

---

## Search

```r
srch <- Search$new(client)

# Basic ANN search
srch$search_points("my_col",
  vector          = c(0.1, 0.2, 0.3),
  limit           = 5,
  with_payload    = TRUE,
  score_threshold = 0.7)

# Search with a filter
srch$search_points("my_col",
  vector = c(0.1, 0.2, 0.3),
  limit  = 5,
  filter = list(must = list(list(key = "color", match = list(value = "red")))))

# Batch search
srch$search_batch_points("my_col", searches = list(
  list(vector = c(0.1, 0.2, 0.3), limit = 3),
  list(vector = c(0.4, 0.5, 0.6), limit = 3)
))

# Grouped search
srch$search_point_groups("my_col",
  vector     = c(0.1, 0.2, 0.3),
  group_by   = "category",
  group_size = 2,
  limit      = 5)

# Modern query API (recommended — supports fusion, re-ranking, prefetch)
srch$query_points("my_col",
  query = list(nearest = c(0.1, 0.2, 0.3)),
  limit = 5)

# Reciprocal Rank Fusion across multiple prefetches
srch$query_points("my_col",
  query    = list(fusion = "rrf"),
  prefetch = list(
    list(query = list(nearest = c(0.1, 0.2)), limit = 20),
    list(query = list(nearest = c(0.9, 0.8)), limit = 20)
  ),
  limit = 5)

# Batch and grouped query
srch$query_points_batch("my_col", searches = list(
  list(query = list(nearest = c(0.1, 0.2)), limit = 3),
  list(query = list(nearest = c(0.4, 0.5)), limit = 3)
))

srch$query_points_groups("my_col",
  query      = list(nearest = c(0.1, 0.2)),
  group_by   = "category",
  group_size = 2)

# Recommendation
srch$recommend_points("my_col",
  positive = list(1L, 2L),
  negative = list(3L),
  limit    = 5)

srch$recommend_batch("my_col", searches = list(
  list(positive = list(1L), limit = 5)
))

srch$recommend_groups("my_col",
  positive   = list(1L),
  group_by   = "category",
  group_size = 2)

# Discovery (context-based search)
srch$discover_points("my_col",
  target  = 1L,
  context = list(list(positive = 2L, negative = 3L)),
  limit   = 5)

srch$discover_batch("my_col", searches = list(
  list(target  = 1L,
       context = list(list(positive = 2L, negative = 3L)))
))
```

---

## Payload Indexes

Indexes are required for filtered searches and counts on Qdrant Cloud.

```r
idx <- Indexes$new(client)

# Create a keyword index (for exact match filters)
idx$create_payload_index("my_col",
  field_name   = "color",
  field_schema = "keyword")

# Other supported types: "integer", "float", "bool", "geo", "datetime", "text"

# Full-text index with custom tokenizer
idx$create_payload_index("my_col",
  field_name   = "description",
  field_schema = list(type = "text", tokenizer = "word"))

# Delete an index
idx$delete_payload_index("my_col", field_name = "color")
```

---

## Aliases

```r
als <- Aliases$new(client)

# Atomic blue/green swap (both actions in one API call)
als$update_aliases(actions = list(
  list(delete_alias = list(alias_name = "prod")),
  list(create_alias = list(collection_name = "my_col_v2", alias_name = "prod"))
))

# Convenience wrappers
als$create_alias("my_col_v2", "prod")
als$rename_alias("prod", "prod_archived")
als$delete_alias("prod_archived")

# List aliases
als$list_all_aliases()
als$list_collection_aliases("my_col")
```

---

## Snapshots

```r
snap <- Snapshots$new(client)

# Collection snapshots
snap$list_snapshots("my_col")
snap$create_snapshot("my_col")
snap$delete_snapshot("my_col", snapshot_name = "my_col-1234.snapshot")

# Recover a collection from a remote snapshot URL
snap$recover_from_snapshot("my_col",
  location = "https://storage.example.com/my_col.snapshot",
  priority = "snapshot")

# Full-storage snapshots
snap$list_full_snapshots()
snap$create_full_snapshot()
snap$delete_full_snapshot(snapshot_name = "full-1234.snapshot")
```

---

## Service & Health

```r
svc <- Service$new(client)

svc$get_root()                           # version info
svc$healthz()                            # Kubernetes liveness probe
svc$readyz()                             # Kubernetes readiness probe
svc$get_metrics()                        # Prometheus metrics
svc$get_telemetry()                      # telemetry data
svc$retrieve_instance_details()          # cluster / node info
svc$get_locks()                          # check write-protection lock
svc$set_write_protection(TRUE)           # enable write lock
svc$set_write_protection(FALSE)          # disable write lock
svc$get_issues()                         # list cluster issues
svc$clear_issues()                       # clear issues
```

---

## Filters Reference

Filters follow Qdrant's [filter syntax](https://qdrant.tech/documentation/concepts/filtering/).

```r
# Match exact value
filter <- list(must = list(
  list(key = "color", match = list(value = "red"))
))

# Range
filter <- list(must = list(
  list(key = "price", range = list(gte = 10, lte = 100))
))

# Combined must + must_not
filter <- list(
  must     = list(list(key = "in_stock", match = list(value = TRUE))),
  must_not = list(list(key = "color",    match = list(value = "black")))
)

# Geo radius
filter <- list(must = list(
  list(key = "location", geo_radius = list(
    center = list(lon = -73.9857, lat = 40.7484),
    radius = 500
  ))
))
```

---

## Running Tests

### In RStudio

1. Open the **QdRant/QdRant** project (File → Open Project → select `QdRant/QdRant/QdRant.Rproj`)
2. Press **Ctrl+Shift+T** (or go to Build → Test Package) to run all unit tests
3. Results appear in the **Build** pane with a clean pass/fail summary

### From the console

```r
# Unit tests only (no live server needed)
devtools::test()

# With a live Qdrant instance
Sys.setenv(
  QDRANT_HOST    = "your-cluster.qdrant.io",
  QDRANT_PORT    = "6333",
  QDRANT_API_KEY = "your-api-key"
)
devtools::test()
```

Integration tests are automatically skipped when `QDRANT_HOST` is not set, so the unit-test suite always runs offline.

---

## License

MIT
