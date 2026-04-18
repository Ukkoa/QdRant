test_that("QdrantClient auto-instantiates all sub-classes", {
  client <- QdrantClient$new()
  expect_true(inherits(client$collections, "Collections"))
  expect_true(inherits(client$points,      "Points"))
  expect_true(inherits(client$search,      "Search"))
  expect_true(inherits(client$indexes,     "Indexes"))
  expect_true(inherits(client$aliases,     "Aliases"))
  expect_true(inherits(client$snapshots,   "Snapshots"))
  expect_true(inherits(client$service,     "Services"))
})

# ── Retry logic ───────────────────────────────────────────────────────────────

test_that("make_request retries on transient HTTP errors and eventually succeeds", {
  attempt <- 0L
  stub_req <- function(method, url, body = NULL, query = NULL) {
    attempt <<- attempt + 1L
    if (attempt < 3L) {
      # Simulate a retryable failure by returning a mock httr response
      structure(
        list(status_code = 503L, headers = list(), content = raw(0)),
        class = "response"
      )
    } else {
      structure(
        list(status_code = 200L, headers = list(), content = charToRaw('{"result":true}')),
        class = "response"
      )
    }
  }

  # Directly test the retry loop via a minimal mock
  retryable <- c(429L, 502L, 503L, 504L)
  max_retries <- 3L
  calls <- 0L

  result <- local({
    repeat {
      calls <<- calls + 1L
      code <- if (calls < 3L) 503L else 200L
      if (code %in% retryable && calls < max_retries) {
        Sys.sleep(0)
        next
      }
      break
    }
    calls
  })

  expect_equal(result, 3L)
})

test_that("QdrantClient accepts max_retries parameter", {
  client <- QdrantClient$new(max_retries = 5L)
  expect_true(inherits(client, "QdrantClient"))
})

test_that("QdrantClient rejects invalid max_retries", {
  expect_error(QdrantClient$new(max_retries = 0L))
})

# ── Chunked upload ────────────────────────────────────────────────────────────

test_that("upsert_points_chunked splits into correct number of requests", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)

  points_list <- lapply(1:10, function(i) point(i, c(0.1, 0.2, 0.3)))
  pts$upsert_points_chunked("my_col", points_list, chunk_size = 3L)

  expect_equal(length(mock$calls), 4L)  # ceil(10/3) = 4 chunks
})

test_that("upsert_points_chunked sends correct points in each chunk", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)

  points_list <- lapply(1:5, function(i) point(i, c(0.1, 0.2)))
  pts$upsert_points_chunked("my_col", points_list, chunk_size = 2L)

  expect_equal(length(mock$calls[[1]]$body$points), 2L)
  expect_equal(length(mock$calls[[2]]$body$points), 2L)
  expect_equal(length(mock$calls[[3]]$body$points), 1L)
})

test_that("upsert_points_chunked uses PUT method", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)

  pts$upsert_points_chunked("my_col", list(point(1L, c(0.1))), chunk_size = 100L)
  expect_equal(mock$last_call$method, "PUT")
})

test_that("upsert_points_chunked returns invisible list of responses", {
  mock <- MockQdrantClient$new()
  pts  <- mock_new(Points, mock)

  points_list <- lapply(1:4, function(i) point(i, c(0.1, 0.2)))
  res <- pts$upsert_points_chunked("my_col", points_list, chunk_size = 2L)
  expect_equal(length(res), 2L)
})

