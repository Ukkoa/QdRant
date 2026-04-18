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

