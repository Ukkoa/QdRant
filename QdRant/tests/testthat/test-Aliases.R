test_that("update_aliases calls POST /collections/aliases", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$update_aliases(actions = list(
    list(create_alias = list(collection_name = "col_v2",
                             alias_name      = "col"))
  ))

  expect_equal(mock$last_call$method, "POST")
  expect_equal(mock$last_call$url, "http://localhost:6333/collections/aliases")
  expect_length(mock$last_call$body$actions, 1)
})

test_that("update_aliases errors on empty actions", {
  als <- Aliases$new(MockQdrantClient$new())
  expect_error(als$update_aliases(actions = list()))
})

test_that("list_all_aliases calls GET /aliases", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$list_all_aliases()

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url, "http://localhost:6333/aliases")
})

test_that("list_collection_aliases calls GET /collections/{name}/aliases", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$list_collection_aliases("my_col")

  expect_equal(mock$last_call$method, "GET")
  expect_equal(mock$last_call$url,
               "http://localhost:6333/collections/my_col/aliases")
})

test_that("create_alias wraps update_aliases correctly", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$create_alias("col_v2", "col")

  action <- mock$last_call$body$actions[[1]]
  expect_equal(action$create_alias$collection_name, "col_v2")
  expect_equal(action$create_alias$alias_name, "col")
})

test_that("rename_alias wraps update_aliases correctly", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$rename_alias("col_old", "col_archived")

  action <- mock$last_call$body$actions[[1]]
  expect_equal(action$rename_alias$old_alias_name, "col_old")
  expect_equal(action$rename_alias$new_alias_name, "col_archived")
})

test_that("delete_alias wraps update_aliases correctly", {
  mock <- MockQdrantClient$new()
  als  <- Aliases$new(mock)
  als$delete_alias("col")

  action <- mock$last_call$body$actions[[1]]
  expect_equal(action$delete_alias$alias_name, "col")
})

test_that("create_alias errors on empty alias_name", {
  als <- Aliases$new(MockQdrantClient$new())
  expect_error(als$create_alias("col", ""))
})

test_that("rename_alias errors on empty old_alias_name", {
  als <- Aliases$new(MockQdrantClient$new())
  expect_error(als$rename_alias("", "new"))
})
