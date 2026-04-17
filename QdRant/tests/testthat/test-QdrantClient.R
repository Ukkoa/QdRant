test_that("get_base_url uses http for localhost", {
  client <- QdrantClient$new()
  expect_equal(client$get_base_url(), "http://localhost:6333")
})

test_that("get_base_url uses http for 127.0.0.1", {
  client <- QdrantClient$new(host = "127.0.0.1")
  expect_equal(client$get_base_url(), "http://127.0.0.1:6333")
})

test_that("get_base_url uses https for remote hosts", {
  client <- QdrantClient$new(host = "xyz.us-east-1.aws.cloud.qdrant.io", port = 6333)
  expect_equal(client$get_base_url(),
               "https://xyz.us-east-1.aws.cloud.qdrant.io:6333")
})

test_that("get_base_url respects custom port", {
  client <- QdrantClient$new(host = "localhost", port = 9999)
  expect_equal(client$get_base_url(), "http://localhost:9999")
})

test_that("QdrantClient stores api_key", {
  client <- QdrantClient$new(api_key = "secret")
  expect_equal(client$api_key, "secret")
})

test_that("QdrantClient stores host and port", {
  client <- QdrantClient$new(host = "example.com", port = 1234)
  expect_equal(client$host, "example.com")
  expect_equal(client$port, 1234)
})

test_that("make_request errors on unsupported method", {
  client <- QdrantClient$new()
  expect_error(
    client$make_request("TRACE", "http://localhost:6333/"),
    "Unsupported HTTP method"
  )
})
