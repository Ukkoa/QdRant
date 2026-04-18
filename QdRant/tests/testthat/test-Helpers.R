# ── be ────────────────────────────────────────────────────────────────────────

test_that("be with single value builds match value", {
  c <- be("color", "red")
  expect_equal(c$key, "color")
  expect_equal(c$match$value, "red")
  expect_null(c$match$any)
})

test_that("be with multiple values builds match any", {
  c <- be("color", c("red", "blue"))
  expect_equal(unlist(c$match$any), c("red", "blue"))
  expect_null(c$match$value)
})

test_that("be works with logical value", {
  c <- be("in_stock", TRUE)
  expect_equal(c$match$value, TRUE)
})

test_that("be works with numeric value", {
  c <- be("count", 42)
  expect_equal(c$match$value, 42)
})

# ── must / should / must_not ──────────────────────────────────────────────────

test_that("must sets correct type and wraps conditions", {
  m <- must(be("color", "red"))
  expect_equal(m$type, "must")
  expect_equal(m$conditions[[1]]$key, "color")
})

test_that("must accepts multiple conditions", {
  m <- must(be("color", "red"), be("size", "large"))
  expect_length(m$conditions, 2)
})

test_that("should sets correct type", {
  s <- should(be("color", "red"), be("color", "blue"))
  expect_equal(s$type, "should")
  expect_length(s$conditions, 2)
})

test_that("must_not sets correct type", {
  mn <- must_not(be("color", "black"))
  expect_equal(mn$type, "must_not")
  expect_equal(mn$conditions[[1]]$key, "color")
})

# ── qdrant_filter ─────────────────────────────────────────────────────────────

test_that("qdrant_filter builds must clause from must()", {
  f <- qdrant_filter(must(be("color", "red")))
  expect_equal(f$must[[1]]$match$value, "red")
  expect_null(f$should)
  expect_null(f$must_not)
})

test_that("qdrant_filter builds must_not clause", {
  f <- qdrant_filter(must_not(be("color", "black")))
  expect_equal(f$must_not[[1]]$match$value, "black")
})

test_that("qdrant_filter combines must and must_not", {
  f <- qdrant_filter(
    must(be("color", "red")),
    must_not(be("color", "black"))
  )
  expect_equal(f$must[[1]]$match$value, "red")
  expect_equal(f$must_not[[1]]$match$value, "black")
})

test_that("qdrant_filter supports multiple conditions in one clause", {
  f <- qdrant_filter(
    must(be("color", "red"), be("price", in_range(gte = 10)))
  )
  expect_length(f$must, 2)
  expect_equal(f$must[[1]]$key, "color")
  expect_equal(f$must[[2]]$range$gte, 10)
})

test_that("qdrant_filter supports should clause", {
  f <- qdrant_filter(should(be("color", "red"), be("color", "blue")))
  expect_length(f$should, 2)
})

# ── in_range ──────────────────────────────────────────────────────────────────

test_that("be with in_range builds gte/lte", {
  c <- be("price", in_range(gte = 10, lte = 100))
  expect_equal(c$range$gte, 10)
  expect_equal(c$range$lte, 100)
  expect_false("gt" %in% names(c$range))
})

test_that("be with in_range omits unset bounds", {
  c <- be("score", in_range(gt = 0.5))
  expect_false("gte" %in% names(c$range))
  expect_false("lte" %in% names(c$range))
  expect_false("lt"  %in% names(c$range))
  expect_equal(c$range$gt, 0.5)
})

# ── geo helpers ───────────────────────────────────────────────────────────────

test_that("be with in_geo_radius builds correct structure", {
  c <- be("location", in_geo_radius(lon = -73.99, lat = 40.73, radius = 500))
  expect_equal(c$geo_radius$center$lon, -73.99)
  expect_equal(c$geo_radius$radius, 500)
})

test_that("be with in_geo_bbox builds correct structure", {
  c <- be("loc", in_geo_bbox(-74.0, 40.8, -73.9, 40.7))
  expect_equal(c$geo_bounding_box$top_left$lon, -74.0)
  expect_equal(c$geo_bounding_box$bottom_right$lat, 40.7)
})

# ── is_null / is_empty / has_id ───────────────────────────────────────────────

test_that("be with is_null builds correct structure", {
  c <- be("description", is_null())
  expect_equal(c$is_null$key, "description")
})

test_that("be with is_empty builds correct structure", {
  c <- be("tags", is_empty())
  expect_equal(c$is_empty$key, "tags")
})

test_that("has_id builds list of ids", {
  c <- has_id(c(1L, 2L, 3L))
  expect_equal(unlist(c$has_id), c(1L, 2L, 3L))
})

# ── vectors_config ────────────────────────────────────────────────────────────

test_that("vectors_config builds correct structure", {
  v <- vectors_config(384, "Cosine")
  expect_equal(v$size, 384)
  expect_equal(v$distance, "Cosine")
  expect_null(v$on_disk)
})

test_that("vectors_config includes on_disk when set", {
  v <- vectors_config(128, "Dot", on_disk = TRUE)
  expect_true(v$on_disk)
})

test_that("multi_vectors_config builds named list", {
  v <- multi_vectors_config(
    image = vectors_config(512, "Cosine"),
    text  = vectors_config(384, "Dot")
  )
  expect_equal(v$image$size, 512)
  expect_equal(v$text$distance, "Dot")
})

# ── point ─────────────────────────────────────────────────────────────────────

test_that("point builds with payload", {
  p <- point(1L, c(0.1, 0.2), list(color = "red"))
  expect_equal(p$id, 1L)
  expect_equal(p$vector, c(0.1, 0.2))
  expect_equal(p$payload$color, "red")
})

test_that("point omits payload when NULL", {
  p <- point(1L, c(0.1, 0.2))
  expect_false("payload" %in% names(p))
})

# ── query helpers ─────────────────────────────────────────────────────────────

test_that("query_nearest builds correct structure", {
  q <- query_nearest(c(0.1, 0.2, 0.3))
  expect_equal(q$nearest, c(0.1, 0.2, 0.3))
})

test_that("query_fusion defaults to rrf", {
  q <- query_fusion()
  expect_equal(q$fusion, "rrf")
})

test_that("query_fusion accepts dbsf", {
  q <- query_fusion("dbsf")
  expect_equal(q$fusion, "dbsf")
})

test_that("prefetch builds correct structure", {
  p <- prefetch(query_nearest(c(0.1, 0.2)), limit = 50)
  expect_equal(p$query$nearest, c(0.1, 0.2))
  expect_equal(p$limit, 50)
  expect_null(p$filter)
})

test_that("prefetch includes filter when provided", {
  f <- qdrant_filter(must(be("x", 1)))
  p <- prefetch(query_nearest(c(0.1)), filter = f)
  expect_equal(p$filter$must[[1]]$key, "x")
})

# ── end-to-end: helpers wire into Search methods ──────────────────────────────

test_that("helpers compose into a search_points call", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)

  srch$search_points("my_col",
    vector = c(0.1, 0.2, 0.3),
    filter = qdrant_filter(
      must(be("color", "red"), be("price", in_range(gte = 10, lte = 100))),
      must_not(be("color", "black"))
    )
  )

  body <- mock$last_call$body
  expect_equal(body$filter$must[[1]]$key, "color")
  expect_equal(body$filter$must[[2]]$range$gte, 10)
  expect_equal(body$filter$must_not[[1]]$match$value, "black")
})

test_that("helpers compose into a query_points call with prefetch", {
  mock <- MockQdrantClient$new()
  srch <- mock_new(Search, mock)

  srch$query_points("my_col",
    query    = query_fusion("rrf"),
    prefetch = list(
      prefetch(query_nearest(c(0.1, 0.2)), limit = 50),
      prefetch(query_nearest(c(0.9, 0.8)), limit = 50)
    ),
    limit = 5
  )

  body <- mock$last_call$body
  expect_equal(body$query$fusion, "rrf")
  expect_length(body$prefetch, 2)
  expect_equal(body$prefetch[[1]]$limit, 50)
})
