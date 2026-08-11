# Hermetic tests (no network, no token) of the CRAN-required graceful
# failure behavior: when Redivis is unreachable, every getter must message
# and return NULL, never error. The outage is simulated by mocking the
# dataset accessor; get_db_info is mocked to its built-in fallback so no
# network is touched at all.

with_simulated_outage <- function(code) {
  testthat::local_mocked_bindings(
    get_db_info = function() childes_redivis_fallback,
    childes_dataset = function(tag) stop("simulated outage"),
    .package = "childesr"
  )
  op <- options(childesr.request_tries = 1)
  on.exit(options(op), add = TRUE)
  # make sure earlier (networked) tests haven't populated the table cache
  cache <- asNamespace("childesr")$.childesr_env
  rm(list = ls(cache), envir = cache)
  force(code)
}

test_that("getters message and return NULL when Redivis is unreachable", {
  skip_if_not_installed("redivis")
  with_simulated_outage({
    suppressMessages({
      expect_no_error(collections <- get_collections())
      expect_null(collections)
      expect_no_error(corpora <- get_corpora())
      expect_null(corpora)
      expect_no_error(transcripts <- get_transcripts(corpus = "Brown"))
      expect_null(transcripts)
      expect_no_error(participants <- get_participants(corpus = "Brown"))
      expect_null(participants)
      expect_no_error(stats <- get_speaker_statistics(corpus = "Brown"))
      expect_null(stats)
      expect_no_error(tokens <- get_tokens(corpus = "Brown", token = "dog"))
      expect_null(tokens)
      expect_no_error(types <- get_types(corpus = "Brown", type = "dog"))
      expect_null(types)
      expect_no_error(utterances <- get_utterances(corpus = "Brown"))
      expect_null(utterances)
      expect_no_error(contexts <- get_contexts(corpus = "Brown",
                                               token = "dog"))
      expect_null(contexts)
      expect_no_error(result <- get_sql_query("SELECT 1 AS x"))
      expect_null(result)
    })
    expect_message(get_collections(), "Could not retrieve data from Redivis")
  })
})

test_that("unknown db_version errors informatively (argument validation)", {
  testthat::local_mocked_bindings(
    get_db_info = function() childes_redivis_fallback,
    .package = "childesr")
  expect_error(get_collections(db_version = "1999.1"), "not found")
})

test_that("missing token argument errors informatively", {
  expect_error(get_tokens(corpus = "Brown"), "'token' is missing")
})

test_that("deprecated connection interface warns but still works", {
  expect_warning(con <- connect_to_childes(), "deprecated")
  expect_null(con)
  expect_warning(clear_connections(), "deprecated")
  skip_if_not_installed("redivis")
  with_simulated_outage({
    # connection = NULL (the old idiom) is accepted silently
    suppressMessages(expect_no_warning(get_collections(connection = NULL)))
    # a non-NULL connection is ignored with a deprecation warning
    suppressMessages(expect_warning(get_collections(connection = "con"),
                                    "deprecated"))
    # a deprecated db_args is ignored with a deprecation warning
    suppressMessages(expect_warning(get_collections(db_args = list(a = 1)),
                                    "deprecated"))
  })
})
