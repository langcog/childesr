# Hermetic unit tests for the broken-binary detection predicate used by the
# attach-time check (a redivis binary built on a CI runner bakes the runner's
# HOME into its credential-cache path, breaking OAuth token caching).

test_that("is_foreign_baked_dir classifies credential dirs correctly", {
  home <- withr_home <- tempfile("home")
  dir.create(home, recursive = TRUE)

  # not configured at all -> not broken
  expect_false(childesr:::is_foreign_baked_dir(NULL, home = home))
  expect_false(childesr:::is_foreign_baked_dir(NA_character_, home = home))

  # the normal case: cache inside the current HOME -> not broken (whether or
  # not it exists yet)
  expect_false(childesr:::is_foreign_baked_dir(file.path(home, ".redivis"),
                                               home = home))

  # a foreign path whose parent does not exist -> broken binary
  expect_true(childesr:::is_foreign_baked_dir(
    "/Users/nonexistent-ci-runner/.redivis", home = home))

  # a foreign path whose parent exists (deliberate custom location) -> fine
  custom <- tempfile("custom")
  dir.create(custom, recursive = TRUE)
  expect_false(childesr:::is_foreign_baked_dir(file.path(custom, ".redivis"),
                                               home = home))
})
