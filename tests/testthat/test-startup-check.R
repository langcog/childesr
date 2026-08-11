# Hermetic unit tests for the stale-binary detection used by the attach-time
# check (redivis binaries built before 2026-07-31 baked the CI runner's HOME
# into their credential-cache path, breaking OAuth token caching; fixed
# upstream, so current redivis versions no longer expose auth_vars and the
# check must stay silent).

test_that("is_foreign_baked_dir classifies credential dirs correctly", {
  home <- tempfile("home")
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
    "/childesr-test-nonexistent-home/.redivis", home = home))

  # a foreign path whose parent exists (deliberate custom location) -> fine
  custom <- tempfile("custom")
  dir.create(custom, recursive = TRUE)
  expect_false(childesr:::is_foreign_baked_dir(file.path(custom, ".redivis"),
                                               home = home))
})

test_that("redivis_binary_warning is silent for fixed redivis versions", {
  # fixed redivis (>= 2026-07-31) computes auth paths via accessors and has
  # no auth_vars binding; the check must quietly return NULL
  fixed_ns <- new.env(parent = emptyenv())
  expect_silent(res <- childesr:::redivis_binary_warning(ns = fixed_ns))
  expect_null(res)
})

test_that("redivis_binary_warning flags a stale baked binary", {
  stale_ns <- new.env(parent = emptyenv())
  stale_ns$auth_vars <- list(
    redivis_dir = "/childesr-test-nonexistent-home/.redivis")
  msg <- childesr:::redivis_binary_warning(ns = stale_ns)
  expect_type(msg, "character")
  expect_match(msg, "update the redivis package")
  expect_match(msg, "REDIVIS_API_TOKEN")
})

test_that("redivis_binary_warning is silent for a healthy auth dir", {
  ok_ns <- new.env(parent = emptyenv())
  ok_ns$auth_vars <- list(
    redivis_dir = file.path(Sys.getenv("HOME"), ".redivis"))
  expect_null(childesr:::redivis_binary_warning(ns = ok_ns))
})
