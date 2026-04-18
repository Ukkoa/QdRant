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
  host    = "your-cluster-id.us-east-1-0.aws.cloud.qdrant.io",
  port    = 6333,
  api_key = Sys.getenv("QDRANT_API_KEY")
)
```

All API namespaces are available directly on the client:

```r
client$collections   # create, list, update, delete collections
client$points        # upsert, retrieve, scroll, delete points & payloads
client$search        # ANN search, query, recommend, discover
client$indexes       # payload indexes
client$aliases       # collection aliases
client$snapshots     # snapshots
client$service       # health, telemetry, metrics
```

---

## Collections

```r
# Create
client$collections$create_collection("my_col", vector_size = 384, distance_metric = "Cosine")

# With sparse vectors (e.g. BM25 / SPLADE)
client$collections$create_collection("my_col",
  vectors_config        = vectors_config(384, "Cosine"),
  sparse_vectors_config = list(sparse = list()))

# List / info / exists / delete
client$collections$list_all_collections()
client$collections$get_collection_details("my_col")
client$collections$check_collection_existence("my_col")
client$collections$delete_collection("my_col")

# Update optimizer settings
client$collections$update_collection("my_col",
  optimizers_config = list(indexing_threshold = 20000))
```

---

## Points

```r
# Upsert (insert or overwrite)
client$points$upsert_points("my_col",
  ids      = as.integer(1:3),
  vectors  = list(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6), c(0.7, 0.8, 0.9)),
  payloads = list(list(color = "red"), list(color = "blue"), list(color = "green"))
)

# Retrieve
client$points$retrieve_point("my_col", id = 1L)
client$points$retrieve_points("my_col", ids = c(1L, 2L, 3L))

# Scroll (with optional filter)
client$points$scroll_points("my_col", limit = 100L)
client$points$scroll_points("my_col",
  filter = qdrant_filter(must(be("color", "red"))),
  limit  = 50L)

# Count
client$points$count_points("my_col")
client$points$count_points("my_col",
  filter = qdrant_filter(must(be("color", "blue"))))

# Payload operations
client$points$set_payload("my_col",       payload = list(verified = TRUE), ids = c(1L))
client$points$overwrite_payload("my_col", payload = list(color = "yellow"), ids = c(1L))
client$points$delete_payload("my_col",    keys = c("verified"), ids = c(1L))
client$points$clear_payload("my_col",     ids = c(1L, 2L))

# Update / delete vectors
client$points$update_vectors("my_col",
  points = list(list(id = 1L, vector = c(0.9, 0.8, 0.7))))
client$points$delete_vectors("my_col", ids = c(1L), vector_names = c("image"))

# Delete points
client$points$delete_points_by_id("my_col", ids = c(2L, 3L))
client$points$delete_points_by_filter("my_col",
  filter = qdrant_filter(must(be("color", "red"))))

# Batch operations
client$points$batch_update_points("my_col", operations = list(
  list(upsert = list(points = list(list(id = 10L, vector = c(0.1, 0.2, 0.3)))))
))

# Payload facets (requires a keyword index on the field)
client$points$payload_field_facets("my_col", key = "color", limit = 10L)
```

---

## Search

```r
# Basic ANN search
client$search$search_points("my_col",
  vector          = c(0.1, 0.2, 0.3),
  limit           = 5L,
  with_payload    = TRUE,
  score_threshold = 0.7)

# Search with a filter
client$search$search_points("my_col",
  vector = c(0.1, 0.2, 0.3),
  limit  = 5L,
  filter = qdrant_filter(must(be("color", "red"))))

# Batch search
client$search$search_batch_points("my_col", searches = list(
  list(vector = c(0.1, 0.2, 0.3), limit = 3L),
  list(vector = c(0.4, 0.5, 0.6), limit = 3L)
))

# Grouped search
client$search$search_point_groups("my_col",
  vector     = c(0.1, 0.2, 0.3),
  group_by   = "category",
  group_size = 2L,
  limit      = 5L)

# Modern query API (recommended — supports fusion, re-ranking, prefetch)
client$search$query_points("my_col",
  query = query_nearest(c(0.1, 0.2, 0.3)),
  limit = 5L)

# Reciprocal Rank Fusion across multiple prefetches
client$search$query_points("my_col",
  query    = query_fusion("rrf"),
  prefetch = list(
    prefetch(query_nearest(c(0.1, 0.2, 0.3)), limit = 20L),
    prefetch(query_nearest(c(0.9, 0.8, 0.7)), limit = 20L)
  ),
  limit = 5L)

# Recommendation
client$search$recommend_points("my_col",
  positive = list(1L, 2L),
  negative = list(3L),
  limit    = 5L)

# Discovery (context-based search)
client$search$discover_points("my_col",
  target  = 1L,
  context = list(list(positive = 2L, negative = 3L)),
  limit   = 5L)
```

---

## Payload Indexes

Indexes are required for filtered searches and counts on Qdrant Cloud.

```r
# Create a keyword index (for exact match filters)
client$indexes$create_payload_index("my_col",
  field_name   = "color",
  field_schema = "keyword")

# Other supported types: "integer", "float", "bool", "geo", "datetime", "text"

# Full-text index with custom tokenizer
client$indexes$create_payload_index("my_col",
  field_name   = "description",
  field_schema = list(type = "text", tokenizer = "word"))

# Delete an index
client$indexes$delete_payload_index("my_col", field_name = "color")
```

---

## Aliases

```r
# Atomic blue/green swap (both actions in one API call)
client$aliases$update_aliases(actions = list(
  list(delete_alias = list(alias_name = "prod")),
  list(create_alias = list(collection_name = "my_col_v2", alias_name = "prod"))
))

# Convenience wrappers
client$aliases$create_alias("my_col_v2", "prod")
client$aliases$rename_alias("prod", "prod_archived")
client$aliases$delete_alias("prod_archived")

# List aliases
client$aliases$list_all_aliases()
client$aliases$list_collection_aliases("my_col")
```

---

## Snapshots

```r
# Collection snapshots
client$snapshots$list_collection_snapshots("my_col")
client$snapshots$create_collection_snapshot("my_col")
client$snapshots$delete_collection_snapshot("my_col", "my_col-1234.snapshot")

# Recover a collection from a remote snapshot URL
client$snapshots$recover_collection_from_snapshot("my_col",
  location = "https://storage.example.com/my_col.snapshot",
  priority = "snapshot")

# Full-storage snapshots
client$snapshots$list_full_snapshots()
client$snapshots$create_full_snapshot()
client$snapshots$delete_full_snapshot("full-1234.snapshot")
```

---

## Service & Health

```r
client$service$retrieve_instance_details()      # version / cluster info
client$service$collect_telemetry_data()         # telemetry
client$service$collect_prometheus_metrics()     # Prometheus metrics
client$service$kubernetes_liveness_probe()      # liveness check
client$service$kubernetes_readiness_probe()     # readiness check
client$service$kubernetes_health_check()        # health check
client$service$check_write_protection()         # check write lock
client$service$set_write_protection(TRUE)       # enable write lock
client$service$set_write_protection(FALSE)      # disable write lock
client$service$get_issues()                     # list cluster issues
client$service$clear_issues()                   # clear issues
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

# At least one condition (OR logic)
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

If [Docker](https://www.docker.com/get-started/) is installed and running, the test suite will automatically:

1. Pull and start a `qdrant/qdrant` container on `localhost:6333`
2. Run all unit **and** integration tests against it
3. Stop and remove the container when done

```r
# In RStudio: Ctrl+Shift+T
# Or directly:
testthat::test_dir("tests/testthat")
```

No configuration needed — just have Docker running.

### Against Qdrant Cloud

```r
Sys.setenv(
  QDRANT_HOST    = "your-cluster.qdrant.io",
  QDRANT_PORT    = "6333",
  QDRANT_API_KEY = "your-api-key"
)
testthat::test_dir("tests/testthat")
```

Integration tests are skipped automatically when Docker is unavailable and `QDRANT_HOST` is not set, so the unit-test suite always works offline.

---

## License

MIT
