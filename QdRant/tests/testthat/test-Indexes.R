test_that("create_payload_index calls PUT /collections/{name}/index", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$create_payload_index("my_col", field_name = "color")

  expect_equal(mock$last_call$method, "PUT")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/index")
  expect_equal(mock$last_call$body$field_name, "color")
})

test_that("create_payload_index passes field_schema", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$create_payload_index("my_col", field_name = "color",
                            field_schema = "keyword")

  expect_equal(mock$last_call$body$field_schema, "keyword")
})

test_that("create_payload_index passes list field_schema for text", {
  mock   <- MockQdrantClient$new()
  idx    <- Indexes$new(mock)
  schema <- list(type = "text", tokenizer = "word")
  idx$create_payload_index("my_col", field_name = "description",
                            field_schema = schema)

  expect_equal(mock$last_call$body$field_schema, schema)
})

test_that("create_payload_index passes ordering as query param", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$create_payload_index("my_col", field_name = "ts",
                            ordering = "strong")

  expect_equal(mock$last_call$query$ordering, "strong")
})

test_that("create_payload_index omits field_schema when NULL", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$create_payload_index("my_col", field_name = "x")

  expect_false("field_schema" %in% names(mock$last_call$body))
})

test_that("create_payload_index errors on empty field_name", {
  idx <- Indexes$new(MockQdrantClient$new())
  expect_error(idx$create_payload_index("my_col", field_name = ""))
})

test_that("delete_payload_index calls DELETE /index/{field_name}", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$delete_payload_index("my_col", "color")

  expect_equal(mock$last_call$method, "DELETE")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/index/color")
})

test_that("delete_payload_index passes ordering as query param", {
  mock <- MockQdrantClient$new()
  idx  <- Indexes$new(mock)
  idx$delete_payload_index("my_col", "color", ordering = "weak")

  expect_equal(mock$last_call$query$ordering, "weak")
})
