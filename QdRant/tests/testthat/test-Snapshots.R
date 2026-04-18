test_that("list_collection_snapshots calls GET /collections/{name}/snapshots", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$list_collection_snapshots("my_col")

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/snapshots")
})

test_that("create_collection_snapshot calls POST /collections/{name}/snapshots", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$create_collection_snapshot("my_col")

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/snapshots")
})

test_that("recover_collection_from_snapshot calls PUT /snapshots/recover", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$recover_collection_from_snapshot("my_col",
    location = "https://example.com/snap.snapshot")

  expect_equal(mock$last_call$method, "PUT")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/snapshots/recover")
  expect_equal(mock$last_call$body$location,
               "https://example.com/snap.snapshot")
})

test_that("recover_collection_from_snapshot passes priority", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$recover_collection_from_snapshot("my_col",
    location = "http://x/s.snapshot", priority = "snapshot")

  expect_equal(mock$last_call$body$priority, "snapshot")
})

test_that("delete_collection_snapshot calls DELETE /snapshots/{name}", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$delete_collection_snapshot("my_col", "snap.snapshot")

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/snapshots/snap.snapshot")
})

test_that("list_full_snapshots calls GET /snapshots", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$list_full_snapshots()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/snapshots")
})

test_that("create_full_snapshot calls POST /snapshots", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$create_full_snapshot()

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url, "http://localhost:6333/snapshots")
})

test_that("delete_full_snapshot calls DELETE /snapshots/{name}", {
  mock <- MockQdrantClient$new()
  snp  <- mock_new(Snapshots, mock)
  snp$delete_full_snapshot("full.snapshot")

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/snapshots/full.snapshot")
})
