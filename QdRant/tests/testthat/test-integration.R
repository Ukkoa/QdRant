# Integration tests — require a live Qdrant instance.
#
# Configure via environment variables before running:
#   QDRANT_HOST     = "your-cluster.cloud.qdrant.io"
#   QDRANT_API_KEY  = "your-api-key"          (optional for local)
#   QDRANT_PORT     = "6333"                  (default)
#
# These tests are automatically skipped when QDRANT_HOST is unset.
# They create and clean up their own uniquely-named collections.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

client_or_skip <- function() {
  cl <- live_client()
  if (is.null(cl)) skip("QDRANT_HOST not set — skipping integration tests.")
  cl
}

sample_vectors <- list(
  c(0.10, 0.20, 0.30),
  c(0.40, 0.50, 0.60),
  c(0.70, 0.80, 0.90),
  c(0.15, 0.25, 0.35),
  c(0.55, 0.65, 0.75)
)
sample_payloads <- list(
  list(color = "red",   score = 1L),
  list(color = "blue",  score = 2L),
  list(color = "green", score = 3L),
  list(color = "red",   score = 4L),
  list(color = "blue",  score = 5L)
)

# ---------------------------------------------------------------------------
# Service
# ---------------------------------------------------------------------------

test_that("[integration] retrieve_instance_details returns version info", {
  cl  <- client_or_skip()
  res <- cl$service$retrieve_instance_details()
  expect_true(!is.null(res$version) || !is.null(res$result$version))
})

test_that("[integration] kubernetes health checks return OK", {
  cl <- client_or_skip()
  expect_no_error(cl$service$kubernetes_health_check())
  expect_no_error(cl$service$kubernetes_liveness_probe())
  expect_no_error(cl$service$kubernetes_readiness_probe())
})

# ---------------------------------------------------------------------------
# Collections — full lifecycle
# ---------------------------------------------------------------------------

test_that("[integration] collection lifecycle: create / inspect / delete", {
  cl       <- client_or_skip()
  col_name <- test_collection()
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  res <- cl$collections$create_collection(col_name, vector_size = 3L,
                                          distance_metric = "Cosine")
  expect_true(isTRUE(res$result) || identical(res$result, TRUE))

  exists <- cl$collections$check_collection_existence(col_name)
  expect_true(isTRUE(exists$result$exists))

  details <- cl$collections$get_collection_details(col_name)
  expect_equal(details$result$config$params$vectors$size, 3L)

  listing   <- cl$collections$list_all_collections()
  col_names <- vapply(listing$result$collections, `[[`, character(1), "name")
  expect_true(col_name %in% col_names)

  expect_no_error(
    cl$collections$update_collection(col_name,
      optimizers_config = list(indexing_threshold = 50000L))
  )

  del  <- cl$collections$delete_collection(col_name)
  expect_true(isTRUE(del$result) || identical(del$result, TRUE))

  gone <- cl$collections$check_collection_existence(col_name)
  expect_false(isTRUE(gone$result$exists))
})

# ---------------------------------------------------------------------------
# Points — full lifecycle
# ---------------------------------------------------------------------------

test_that("[integration] points lifecycle: upsert / retrieve / scroll / delete", {
  cl       <- client_or_skip()
  col_name <- test_collection("_pts")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$indexes$create_payload_index(col_name, "color", field_schema = "keyword")

  res <- cl$points$upsert_points(col_name,
    ids      = as.integer(1:5),
    vectors  = sample_vectors,
    payloads = sample_payloads
  )
  expect_true(res$result$status %in% c("acknowledged", "completed"))

  pt <- cl$points$retrieve_point(col_name, 1L)
  expect_equal(pt$result$id, 1L)

  multi <- cl$points$retrieve_points(col_name, ids = c(1L, 2L, 3L))
  expect_length(multi$result, 3)

  scroll <- cl$points$scroll_points(col_name, limit = 10L)
  expect_gte(length(scroll$result$points), 5L)

  cnt <- cl$points$count_points(col_name)
  expect_gte(cnt$result$count, 5L)

  cl$points$set_payload(col_name, payload = list(verified = TRUE), ids = c(1L))
  pt_updated <- cl$points$retrieve_point(col_name, 1L)
  expect_true(isTRUE(pt_updated$result$payload$verified))

  cl$points$overwrite_payload(col_name, payload = list(color = "yellow"), ids = c(2L))
  pt_ow <- cl$points$retrieve_point(col_name, 2L)
  expect_equal(pt_ow$result$payload$color, "yellow")
  expect_null(pt_ow$result$payload$score)

  cl$points$delete_payload(col_name, keys = c("verified"), ids = c(1L))
  pt_del <- cl$points$retrieve_point(col_name, 1L)
  expect_null(pt_del$result$payload$verified)

  cl$points$clear_payload(col_name, ids = c(3L))
  pt_clear <- cl$points$retrieve_point(col_name, 3L)
  expect_length(pt_clear$result$payload, 0)

  cl$points$delete_points_by_id(col_name, ids = c(4L, 5L))
  cnt_after <- cl$points$count_points(col_name)
  expect_equal(cnt_after$result$count, 3L)

  cl$points$delete_points_by_filter(col_name,
    filter = qdrant_filter(must(be("color", "red")))
  )
  cnt_final <- cl$points$count_points(col_name)
  expect_lt(cnt_final$result$count, 3L)
})

test_that("[integration] update_vectors changes stored vector", {
  cl       <- client_or_skip()
  col_name <- test_collection("_uvec")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L,
                                   distance_metric = "Euclid")
  cl$points$upsert_points(col_name, ids = 1L, vectors = list(c(0.1, 0.2, 0.3)))

  new_vec <- c(0.9, 0.8, 0.7)
  cl$points$update_vectors(col_name,
    points = list(list(id = 1L, vector = new_vec)))

  pt  <- cl$points$retrieve_point(col_name, 1L, with_vector = TRUE)
  vec <- round(as.numeric(unlist(pt$result$vector)), 1)
  expect_equal(vec, new_vec)
})

test_that("[integration] scroll_points paginates correctly", {
  cl       <- client_or_skip()
  col_name <- test_collection("_scroll")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$points$upsert_points(col_name, ids = as.integer(1:5), vectors = sample_vectors)

  page1 <- cl$points$scroll_points(col_name, limit = 3L)
  expect_length(page1$result$points, 3L)

  page2 <- cl$points$scroll_points(col_name, limit = 3L,
                                   offset = page1$result$next_page_offset)
  expect_gte(length(page2$result$points), 2L)
})

test_that("[integration] count_points with filter returns subset count", {
  cl       <- client_or_skip()
  col_name <- test_collection("_cnt")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$indexes$create_payload_index(col_name, "color", field_schema = "keyword")
  cl$points$upsert_points(col_name,
    ids      = as.integer(1:5),
    vectors  = sample_vectors,
    payloads = sample_payloads
  )

  red_count <- cl$points$count_points(col_name,
    filter = qdrant_filter(must(be("color", "red"))))
  expect_equal(red_count$result$count, 2L)
})

test_that("[integration] payload_field_facets returns value counts", {
  cl       <- client_or_skip()
  col_name <- test_collection("_facets")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$indexes$create_payload_index(col_name, "color", field_schema = "keyword")
  cl$points$upsert_points(col_name,
    ids      = as.integer(1:5),
    vectors  = sample_vectors,
    payloads = sample_payloads
  )

  result <- tryCatch(
    cl$points$payload_field_facets(col_name, key = "color", limit = 10L),
    error = function(e) {
      if (grepl("404|Not Found", conditionMessage(e)))
        skip("payload_field_facets endpoint not available on this Qdrant instance")
      stop(e)
    }
  )
  hits   <- result$result$hits
  expect_true(length(hits) > 0)
  values <- vapply(hits, function(h) h$value, character(1))
  expect_true("red" %in% values)
})

# ---------------------------------------------------------------------------
# Search
# ---------------------------------------------------------------------------

test_that("[integration] search_points returns nearest neighbours", {
  cl       <- client_or_skip()
  col_name <- test_collection("_srch")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$points$upsert_points(col_name, ids = as.integer(1:5), vectors = sample_vectors)

  res <- cl$search$search_points(col_name, vector = c(0.1, 0.2, 0.3), limit = 3L)
  expect_length(res$result, 3L)
  expect_true(!is.null(res$result[[1]]$score))
})

test_that("[integration] query_points returns results", {
  cl       <- client_or_skip()
  col_name <- test_collection("_qry")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$points$upsert_points(col_name, ids = as.integer(1:5), vectors = sample_vectors)

  res <- cl$search$query_points(col_name,
    query = query_nearest(c(0.1, 0.2, 0.3)), limit = 3L)
  expect_length(res$result$points, 3L)
})

test_that("[integration] recommend_points returns results", {
  cl       <- client_or_skip()
  col_name <- test_collection("_rec")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$points$upsert_points(col_name, ids = as.integer(1:5), vectors = sample_vectors)

  res <- cl$search$recommend_points(col_name,
    positive = list(1L), negative = list(5L), limit = 3L)
  expect_true(length(res$result) > 0)
})

test_that("[integration] search with filter narrows results", {
  cl       <- client_or_skip()
  col_name <- test_collection("_filter_srch")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)
  cl$indexes$create_payload_index(col_name, "color", field_schema = "keyword")
  cl$points$upsert_points(col_name,
    ids      = as.integer(1:5),
    vectors  = sample_vectors,
    payloads = sample_payloads
  )

  res <- cl$search$search_points(col_name,
    vector = c(0.1, 0.2, 0.3),
    limit  = 10L,
    filter = qdrant_filter(must(be("color", "red")))
  )
  colors <- vapply(res$result, function(r) r$payload$color, character(1))
  expect_true(all(colors == "red"))
})

# ---------------------------------------------------------------------------
# Indexes
# ---------------------------------------------------------------------------

test_that("[integration] payload index can be created and deleted", {
  cl       <- client_or_skip()
  col_name <- test_collection("_idx")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)

  res <- cl$indexes$create_payload_index(col_name, "color", field_schema = "keyword")
  expect_true(res$result$status %in% c("acknowledged", "completed"))

  del <- cl$indexes$delete_payload_index(col_name, "color")
  expect_true(del$result$status %in% c("acknowledged", "completed"))
})

# ---------------------------------------------------------------------------
# Aliases
# ---------------------------------------------------------------------------

test_that("[integration] alias lifecycle: create / list / rename / delete", {
  cl       <- client_or_skip()
  col_name <- test_collection("_alias")
  alias1   <- paste0(col_name, "_alias")
  alias2   <- paste0(col_name, "_alias_v2")
  on.exit({
    try(cl$aliases$delete_alias(alias1),  silent = TRUE)
    try(cl$aliases$delete_alias(alias2),  silent = TRUE)
    try(cl$collections$delete_collection(col_name), silent = TRUE)
  })

  cl$collections$create_collection(col_name, vector_size = 3L)

  res <- cl$aliases$create_alias(col_name, alias1)
  expect_true(isTRUE(res$result) || identical(res$result, TRUE))

  coll_aliases <- cl$aliases$list_collection_aliases(col_name)
  alias_names  <- vapply(coll_aliases$result$aliases,
                         function(a) a$alias_name, character(1))
  expect_true(alias1 %in% alias_names)

  cl$aliases$rename_alias(alias1, alias2)

  del <- cl$aliases$delete_alias(alias2)
  expect_true(isTRUE(del$result) || identical(del$result, TRUE))
})

# ---------------------------------------------------------------------------
# Snapshots
# ---------------------------------------------------------------------------

test_that("[integration] collection snapshot can be created and deleted", {
  cl       <- client_or_skip()
  col_name <- test_collection("_snap")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)

  created   <- cl$snapshots$create_collection_snapshot(col_name)
  snap_name <- created$result$name
  expect_true(nzchar(snap_name))

  listing    <- cl$snapshots$list_collection_snapshots(col_name)
  snap_names <- vapply(listing$result, function(s) s$name, character(1))
  expect_true(snap_name %in% snap_names)

  del <- cl$snapshots$delete_collection_snapshot(col_name, snap_name)
  expect_true(isTRUE(del$result) || identical(del$result, TRUE))
})

# ---------------------------------------------------------------------------
# Batch operations
# ---------------------------------------------------------------------------

test_that("[integration] batch_update_points upserts and deletes atomically", {
  cl       <- client_or_skip()
  col_name <- test_collection("_batch")
  on.exit(try(cl$collections$delete_collection(col_name), silent = TRUE))

  cl$collections$create_collection(col_name, vector_size = 3L)

  res <- cl$points$batch_update_points(col_name, operations = list(
    list(upsert = list(points = list(
      list(id = 1L, vector = c(0.1, 0.2, 0.3)),
      list(id = 2L, vector = c(0.4, 0.5, 0.6))
    ))),
    list(delete = list(points = list(2L)))
  ))
  expect_true(res$result[[1]]$status %in% c("acknowledged", "completed"))

  cnt <- cl$points$count_points(col_name)
  expect_equal(cnt$result$count, 1L)
})
