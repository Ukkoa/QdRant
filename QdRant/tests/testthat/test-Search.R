test_that("search_points calls POST /points/search", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$search_points("my_col", vector = c(0.1, 0.2, 0.3), limit = 5)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/search")
  expect_equal(mock$last_call$body$vector, c(0.1, 0.2, 0.3))
  expect_equal(mock$last_call$body$limit, 5)
})

test_that("search_points passes filter", {
  mock   <- MockQdrantClient$new()
  srch   <- mock_new(Search, mock)
  filter <- list(must = list(list(key = "color", match = list(value = "red"))))
  srch$search_points("my_col", vector = c(0.1, 0.2), filter = filter)

  expect_equal(mock$last_call$body$filter, filter)
})

test_that("search_points omits filter when NULL", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$search_points("my_col", vector = c(0.1, 0.2))

  expect_false("filter" %in% names(mock$last_call$body))
})

test_that("search_points passes score_threshold", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$search_points("my_col", vector = c(0.1), score_threshold = 0.75)

  expect_equal(mock$last_call$body$score_threshold, 0.75)
})

test_that("search_batch_points calls POST /points/search/batch", {
  mock    <- MockQdrantClient$new()
  srch    <- mock_new(Search, mock)
  batches <- list(
    list(vector = c(0.1, 0.2), limit = 3),
    list(vector = c(0.3, 0.4), limit = 3)
  )
  srch$search_batch_points("my_col", searches = batches)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/search/batch")
  expect_length(mock$last_call$body$searches, 2)
})

test_that("search_point_groups calls POST /points/search/groups", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$search_point_groups("my_col", vector = c(0.1, 0.2),
                            group_by = "category")

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/search/groups")
  expect_equal(mock$last_call$body$group_by, "category")
})

test_that("query_points calls POST /points/query", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$query_points("my_col",
    query = list(nearest = c(0.1, 0.2, 0.3)), limit = 5)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/query")
  expect_equal(mock$last_call$body$query$nearest, c(0.1, 0.2, 0.3))
  expect_equal(mock$last_call$body$limit, 5)
})

test_that("query_points passes prefetch", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  pre  <- list(list(query = list(nearest = c(0.5, 0.6)), limit = 20))
  srch$query_points("my_col",
    query = list(fusion = "rrf"), prefetch = pre)

  expect_equal(mock$last_call$body$prefetch, pre)
})

test_that("query_points omits optional fields when NULL", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$query_points("my_col", query = list(nearest = c(0.1)))

  body <- mock$last_call$body
  expect_false("prefetch" %in% names(body))
  expect_false("filter" %in% names(body))
  expect_false("score_threshold" %in% names(body))
})

test_that("query_points_batch calls POST /points/query/batch", {
  mock    <- MockQdrantClient$new()
  srch    <- mock_new(Search, mock)
  searches <- list(
    list(query = list(nearest = c(0.1)), limit = 3),
    list(query = list(nearest = c(0.4)), limit = 3)
  )
  srch$query_points_batch("my_col", searches = searches)

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/query/batch")
  expect_length(mock$last_call$body$searches, 2)
})

test_that("query_points_groups calls POST /points/query/groups", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$query_points_groups("my_col",
    query    = list(nearest = c(0.1, 0.2)),
    group_by = "category",
    group_size = 2)

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/query/groups")
  expect_equal(mock$last_call$body$group_by, "category")
  expect_equal(mock$last_call$body$group_size, 2)
})

test_that("recommend_points calls POST /points/recommend", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$recommend_points("my_col",
    positive = list(1L, 2L), negative = list(3L), limit = 5)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/recommend")
  expect_equal(mock$last_call$body$positive, list(1L, 2L))
  expect_equal(mock$last_call$body$negative, list(3L))
  expect_equal(mock$last_call$body$limit, 5)
})

test_that("recommend_points errors when positive is empty", {
  srch <- mock_new(Search, MockQdrantClient$new())
  expect_error(srch$recommend_points("my_col", positive = list()))
})

test_that("recommend_batch calls POST /points/recommend/batch", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$recommend_batch("my_col", searches = list(
    list(positive = list(1L), limit = 5)
  ))

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/recommend/batch")
})

test_that("recommend_groups calls POST /points/recommend/groups", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$recommend_groups("my_col",
    positive = list(1L), group_by = "category")

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/recommend/groups")
  expect_equal(mock$last_call$body$group_by, "category")
})

test_that("discover_points calls POST /points/discover", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$discover_points("my_col",
    target  = 1L,
    context = list(list(positive = 2L, negative = 3L)),
    limit   = 5)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/discover")
  expect_equal(mock$last_call$body$target, 1L)
  expect_length(mock$last_call$body$context, 1)
})

test_that("discover_batch calls POST /points/discover/batch", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)
  srch$discover_batch("my_col", searches = list(
    list(target = 1L, context = list(list(positive = 2L, negative = 3L)))
  ))

  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/points/discover/batch")
})
