# Characterization-test helpers: compare redivis-backend output against
# golden fixtures generated from the MySQL-backed childesr 0.2.3.9000 pinned
# to db_version "2021.1" (see data-raw/make_fixtures.R). The same database
# states are released on Redivis as childes_db versions:
# 2018.1 = v1.0, 2019.1 = v2.0, 2020.1 = v3.0, 2021.1 = v4.0.

skip_if_no_redivis <- function() {
  # network tests never run on CRAN; locally/CI they need the redivis client
  # and a Redivis token
  skip_on_cran()
  skip_if_not_installed("redivis")
  skip_if(Sys.getenv("REDIVIS_API_TOKEN") == "",
          "REDIVIS_API_TOKEN not set")
}

# check (once per run) whether a childes_db dataset version is released
childes_version_released <- local({
  cache <- new.env(parent = emptyenv())
  function(tag) {
    if (is.null(cache[[tag]])) {
      cache[[tag]] <- tryCatch({
        ds <- redivis::redivis$organization("datapages")$dataset(
          "childes_db:b6q6", version = tag)
        suppressWarnings(ds$get())
        TRUE
      }, error = function(e) FALSE)
    }
    cache[[tag]]
  }
})

skip_if_version_unreleased <- function(tag) {
  skip_if(!childes_version_released(tag),
          paste0("childes_db ", tag, " not yet released on Redivis"))
}

load_fixture <- function(name) {
  readRDS(test_path("fixtures", paste0(name, ".rds")))
}

# normalize a tibble for comparison: align column order to the fixture,
# sort rows by all columns, drop rownames/grouping
normalize <- function(x, col_order, sort_cols) {
  x <- dplyr::ungroup(x)
  x <- x[, col_order]
  x <- dplyr::arrange(x, dplyr::across(dplyr::all_of(sort_cols)))
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}

expect_matches_fixture <- function(actual, fixture_name) {
  expected <- load_fixture(fixture_name)

  # same columns, in the same order
  expect_identical(names(actual), names(expected),
                   label = paste0(fixture_name, " column names"))

  sort_cols <- names(expected)
  act <- normalize(actual, names(expected), sort_cols)
  exp <- normalize(expected, names(expected), sort_cols)

  expect_equal(act, exp, label = fixture_name, tolerance = 1e-8)
}

expect_matches_shape <- function(actual, fixture_name) {
  expected <- load_fixture(fixture_name)
  expect_equal(nrow(actual), expected$nrow,
               label = paste0(fixture_name, " nrow"))
  expect_identical(names(actual), expected$names,
                   label = paste0(fixture_name, " names"))
  expect_equal(purrr::map_int(actual, dplyr::n_distinct),
               expected$n_distinct,
               label = paste0(fixture_name, " n_distinct"))
}
