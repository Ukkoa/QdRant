test_that("retrieve_instance_details calls GET on base URL", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$retrieve_instance_details()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333")
})

test_that("collect_telemetry_data calls GET /telemetry with query param", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$collect_telemetry_data()

  expect_equal(mock$last_call$url, "http://localhost:6333/telemetry")
  expect_equal(mock$last_call$query$anonymize, "true")
})

test_that("collect_telemetry_data passes anonymize=false", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$collect_telemetry_data(anonymize = FALSE)

  expect_equal(mock$last_call$query$anonymize, "false")
})

test_that("collect_prometheus_metrics calls GET /metrics with query param", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$collect_prometheus_metrics()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/metrics")
  expect_equal(mock$last_call$query$anonymize, "true")
  expect_null(mock$last_call$body)
})

test_that("check_write_protection calls GET /locks", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$check_write_protection()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/locks")
})

test_that("set_write_protection calls POST /locks with body", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$set_write_protection(write = TRUE)

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url, "http://localhost:6333/locks")
  expect_true(mock$last_call$body$write)
})

test_that("set_write_protection can disable write lock", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$set_write_protection(write = FALSE)

  expect_false(mock$last_call$body$write)
})

test_that("get_issues calls GET /issues", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$get_issues()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/issues")
})

test_that("clear_issues calls DELETE /issues", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$clear_issues()

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(mock$last_call$url, "http://localhost:6333/issues")
})

test_that("kubernetes_health_check calls GET /healthz", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$kubernetes_health_check()

  expect_equal(mock$last_call$url, "http://localhost:6333/healthz")
})

test_that("kubernetes_liveness_probe calls GET /livez", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$kubernetes_liveness_probe()

  expect_equal(mock$last_call$url, "http://localhost:6333/livez")
})

test_that("kubernetes_readiness_probe calls GET /readyz", {
  mock <- MockQdrantClient$new()
  svc  <- Services$new(mock)
  svc$kubernetes_readiness_probe()

  expect_equal(mock$last_call$url, "http://localhost:6333/readyz")
})
