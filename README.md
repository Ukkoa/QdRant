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
  filter = qdrant_filter(must(be("color", "red"))),
  limit  = 50
)

# Count points
pts$count_points("my_col")
pts$count_points("my_col",
  filter = qdrant_filter(must(be("color", "blue"))))

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
  filter = qdrant_filter(must(be("color", "red"))))

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
  filter = qdrant_filter(must(be("color", "red"))))

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
  query = query_nearest(c(0.1, 0.2, 0.3)),
  limit = 5)

# Reciprocal Rank Fusion across multiple prefetches
srch$query_points("my_col",
  query    = query_fusion("rrf"),
  prefetch = list(
    prefetch(query_nearest(c(0.1, 0.2)), limit = 20),
    prefetch(query_nearest(c(0.9, 0.8)), limit = 20)
  ),
  limit = 5)

# Batch and grouped query
srch$query_points_batch("my_col", searches = list(
  list(query = query_nearest(c(0.1, 0.2)), limit = 3),
  list(query = query_nearest(c(0.4, 0.5)), limit = 3)
))

srch$query_points_groups("my_col",
  query      = query_nearest(c(0.1, 0.2)),
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

```r
# Match a single value
qdrant_filter(must(be("color", "red")))

# Match any of several values
qdrant_filter(must(be("color", c("red", "blue", "green"))))

# Exclude values
qdrant_filter(must_not(be("color", c("black", "brown"))))

# Combined — must be red or blue, must not be black
qdrant_filter(
  must(be("color", c("red", "blue"))),
  must_not(be("color", "black"))
)

# At least one of several conditions (OR logic)
qdrant_filter(should(be("color", "red"), be("size", "large")))

# Numeric range
qdrant_filter(must(range_filter("price", gte = 10, lte = 100)))
qdrant_filter(must(range_filter("score", gt = 0.5)))

# Datetime range
qdrant_filter(must(datetime_range("created_at", gte = "2024-01-01T00:00:00Z")))

# Multiple conditions in one clause
qdrant_filter(
  must(be("in_stock", TRUE), range_filter("price", lte = 50)),
  must_not(be("color", "black"))
)

# Geographic radius
qdrant_filter(must(geo_radius("location", lon = -73.99, lat = 40.73, radius = 500)))

# Geographic bounding box
qdrant_filter(must(geo_bounding_box("location",
  top_left_lon = -74.0, top_left_lat = 40.8,
  bottom_right_lon = -73.9, bottom_right_lat = 40.7)))

# Field is NULL / empty
qdrant_filter(must(is_null_field("description")))
qdrant_filter(must(is_empty_field("tags")))

# Filter by point ID
qdrant_filter(must(has_id(c(1L, 2L, 3L))))
```

Raw lists still work everywhere if you prefer to write them by hand.

---

## Running Tests

### Automatic (Docker)

If [Docker](https://www.docker.com/get-started/) is installed and running, `devtools::test()` will automatically:

1. Pull and start a `qdrant/qdrant` container on `localhost:6333`
2. Run all unit **and** integration tests against it
3. Stop and remove the container when done

```r
devtools::test()
```

No configuration needed — just have Docker running.

### In RStudio

1. Open the **QdRant/QdRant** project (File → Open Project → select `QdRant/QdRant/QdRant.Rproj`)
2. Press **Ctrl+Shift+T** (or go to Build → Test Package)
3. Results appear in the **Build** pane

### Against Qdrant Cloud

```r
Sys.setenv(
  QDRANT_HOST    = "your-cluster.qdrant.io",
  QDRANT_PORT    = "6333",
  QDRANT_API_KEY = "your-api-key"
)
devtools::test()
```

Integration tests are skipped automatically when Docker is unavailable and `QDRANT_HOST` is not set, so the unit-test suite always works offline.

---

## License

MIT
