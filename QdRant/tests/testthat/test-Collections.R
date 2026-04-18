test_that("list_all_collections calls GET /collections", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$list_all_collections()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections")
  expect_null(mock$last_call$body)
})

test_that("create_collection calls PUT /collections/{name}", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$create_collection("test_col", vector_size = 128)

  expect_equal(mock$last_call$method, "PUT")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections/test_col")
  expect_equal(mock$last_call$body$vectors$size, 128)
  expect_equal(mock$last_call$body$vectors$distance, "Cosine")
})

test_that("create_collection passes distance_metric", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$create_collection("test_col", vector_size = 64, distance_metric = "Dot")

  expect_equal(mock$last_call$body$vectors$distance, "Dot")
})

test_that("create_collection passes optional fields", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$create_collection("test_col", vector_size = 128,
                         shard_number = 2,
                         on_disk_payload = TRUE,
                         hnsw_config = list(m = 16))

  expect_equal(mock$last_call$body$shard_number, 2)
  expect_true(mock$last_call$body$on_disk_payload)
  expect_equal(mock$last_call$body$hnsw_config$m, 16)
})

test_that("create_collection passes timeout as query param", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$create_collection("test_col", vector_size = 128, timeout = 30)

  expect_equal(mock$last_call$query$timeout, 30)
})

test_that("create_collection omits NULL optional fields", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$create_collection("test_col", vector_size = 128)

  body <- mock$last_call$body
  expect_false("shard_number" %in% names(body))
  expect_false("hnsw_config" %in% names(body))
})

test_that("create_collection validates distance_metric", {
  cols <- mock_new(Collections, MockQdrantClient$new())
  expect_error(
    cols$create_collection("test_col", vector_size = 128,
                           distance_metric = "BadMetric")
  )
})

test_that("create_collection errors on empty collection name", {
  cols <- mock_new(Collections, MockQdrantClient$new())
  expect_error(cols$create_collection("", vector_size = 128))
})

test_that("update_collection calls PATCH /collections/{name}", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$update_collection("test_col",
                         optimizers_config = list(indexing_threshold = 50000))

  expect_equal(mock$last_call$method, "PATCH")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections/test_col")
  expect_equal(mock$last_call$body$optimizers_config$indexing_threshold, 50000)
})

test_that("update_collection passes timeout as query param", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$update_collection("test_col", timeout = 10)

  expect_equal(mock$last_call$query$timeout, 10)
})

test_that("get_collection_details calls GET /collections/{name}", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$get_collection_details("test_col")

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections/test_col")
})

test_that("check_collection_existence calls correct URL", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$check_collection_existence("test_col")

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/test_col/exists")
})

test_that("delete_collection calls DELETE /collections/{name}", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$delete_collection("test_col")

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections/test_col")
})

test_that("delete_collection passes timeout as query param", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$delete_collection("test_col", timeout = 5)

  expect_equal(mock$last_call$query$timeout, 5)
})

test_that("get_collection_optimizations calls correct URL", {
  mock <- MockQdrantClient$new()
  cols <- mock_new(Collections, mock)
  cols$get_collection_optimizations("test_col")

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/test_col/optimizations")
})
