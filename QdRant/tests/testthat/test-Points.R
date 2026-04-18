test_that("retrieve_point calls GET /collections/{name}/points/{id}", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$retrieve_point("my_col", 42)

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/42")
})

test_that("retrieve_points calls POST /collections/{name}/points", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$retrieve_points("my_col", ids = c(1, 2, 3))

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points")
  expect_equal(unlist(mock$last_call$body$ids), c(1, 2, 3))
})

test_that("retrieve_points omits shard_key when NULL", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$retrieve_points("my_col", ids = c(1))

  expect_false("shard_key" %in% names(mock$last_call$body))
})

test_that("upsert_points builds correct body", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$upsert_points("my_col",
    ids      = c(1, 2),
    vectors  = list(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6)),
    payloads = list(list(color = "red"), list(color = "blue"))
  )

  expect_equal(mock$last_call$method, "PUT")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points")
  pts_body <- mock$last_call$body$points
  expect_length(pts_body, 2)
  expect_equal(pts_body[[1]]$id, 1)
  expect_equal(pts_body[[1]]$vector, c(0.1, 0.2, 0.3))
  expect_equal(pts_body[[1]]$payload$color, "red")
  expect_equal(pts_body[[2]]$id, 2)
})

test_that("upsert_points errors when ids is empty", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(pts$upsert_points("my_col", ids = c()),
               "ids must be provided")
})

test_that("upsert_points errors when both vectors and payloads are NULL", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(
    pts$upsert_points("my_col", ids = c(1)),
    "At least one of vectors or payloads"
  )
})

test_that("upsert_points errors when vector length mismatches ids", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(
    pts$upsert_points("my_col", ids = c(1, 2),
                      vectors = list(c(0.1, 0.2))),
    "Length of vectors"
  )
})

test_that("upsert_points works with vectors only", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$upsert_points("my_col", ids = c(1),
                    vectors = list(c(0.1, 0.2)))

  expect_null(mock$last_call$body$points[[1]]$payload)
  expect_equal(mock$last_call$body$points[[1]]$vector, c(0.1, 0.2))
})

test_that("upsert_points passes ordering as query param", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$upsert_points("my_col", ids = c(1),
                    vectors = list(c(0.1)), ordering = "strong")

  expect_equal(mock$last_call$query$ordering, "strong")
})

test_that("delete_points_by_id sends correct body", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$delete_points_by_id("my_col", ids = c(1, 2, 3))

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/delete")
  expect_equal(unlist(mock$last_call$body$points), c(1, 2, 3))
})

test_that("delete_points_by_id errors on empty ids", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(pts$delete_points_by_id("my_col", ids = c()),
               "ids must be provided")
})

test_that("delete_points_by_filter sends filter body", {
  mock   <- MockQdrantClient$new()
  pts    <- mock_new(Points, mock)
  filter <- list(must = list(list(key = "color",
                                   match = list(value = "red"))))
  pts$delete_points_by_filter("my_col", filter = filter)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$body$filter, filter)
})

test_that("delete_points_by_filter errors when filter is not a list", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(pts$delete_points_by_filter("my_col", filter = "bad"))
})

test_that("update_vectors calls PUT /points/vectors", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$update_vectors("my_col",
    points = list(list(id = 1, vector = c(0.1, 0.2))))

  expect_equal(mock$last_call$method, "PUT")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/vectors")
})

test_that("delete_vectors calls DELETE /points/vectors", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$delete_vectors("my_col", ids = c(1, 2),
                     vector_names = c("image", "text"))

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(unlist(mock$last_call$body$points), c(1, 2))
  expect_equal(mock$last_call$body$vector, c("image", "text"))
})

test_that("set_payload calls POST /points/payload", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$set_payload("my_col", payload = list(color = "green"), ids = c(1))

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/payload")
  expect_equal(mock$last_call$body$payload$color, "green")
  expect_equal(unlist(mock$last_call$body$points), c(1))
})

test_that("set_payload errors when neither ids nor filter provided", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(pts$set_payload("my_col", payload = list(x = 1)),
               "One of ids or filter")
})

test_that("overwrite_payload calls PUT /points/payload", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$overwrite_payload("my_col", payload = list(x = 1), ids = c(1))

  expect_equal(mock$last_call$method, "PUT")
})

test_that("delete_payload calls correct endpoint", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$delete_payload("my_col", keys = c("color", "size"), ids = c(1))

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/payload/delete")
  expect_equal(unlist(mock$last_call$body$keys), c("color", "size"))
})

test_that("clear_payload calls correct endpoint", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$clear_payload("my_col", ids = c(1, 2))

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/payload/clear")
  expect_equal(unlist(mock$last_call$body$points), c(1, 2))
})

test_that("clear_payload errors when neither ids nor filter provided", {
  pts <- mock_new(Points, MockQdrantClient$new())
  expect_error(pts$clear_payload("my_col"), "One of ids or filter")
})

test_that("scroll_points calls POST /points/scroll with defaults", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$scroll_points("my_col")

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/scroll")
  expect_equal(mock$last_call$body$limit, 10)
  expect_true(mock$last_call$body$with_payload)
  expect_false(mock$last_call$body$with_vector)
})

test_that("scroll_points includes filter when provided", {
  mock   <- MockQdrantClient$new()
  pts    <- mock_new(Points, mock)
  filter <- list(must = list(list(key = "x", match = list(value = 1))))
  pts$scroll_points("my_col", filter = filter)

  expect_equal(mock$last_call$body$filter, filter)
})

test_that("count_points calls POST /points/count", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$count_points("my_col")

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/count")
  expect_true(mock$last_call$body$exact)
})

test_that("count_points passes filter", {
  mock   <- MockQdrantClient$new()
  pts    <- mock_new(Points, mock)
  filter <- list(must = list(list(key = "x", range = list(gte = 5))))
  pts$count_points("my_col", filter = filter)

  expect_equal(mock$last_call$body$filter, filter)
})

test_that("payload_field_facets calls POST /points/facets", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  pts$payload_field_facets("my_col", key = "category")

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/facets")
  expect_equal(mock$last_call$body$key, "category")
  expect_equal(mock$last_call$body$limit, 10)
})

test_that("batch_update_points calls POST /points/batch", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)
  ops  <- list(list(upsert = list(points = list(list(id = 1,
                                                     vector = c(0.1))))))
  pts$batch_update_points("my_col", operations = ops)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/batch")
  expect_length(mock$last_call$body$operations, 1)
})
