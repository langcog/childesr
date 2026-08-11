# Smoke tests against childes_db v1.0 (db_version "2018.1"), the first
# Redivis release, plus API-compatibility checks for the deprecated
# connection interface.

test_that("get_collections returns the 2018.1 collections", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.0")
  collections <- get_collections(db_version = "2018.1")
  expect_equal(nrow(collections), 20)
  expect_identical(names(collections)[1:2],
                   c("collection_id", "collection_name"))
})

test_that("get_transcripts returns the 2018.1 Brown transcripts", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.0")
  transcripts <- get_transcripts(corpus = "Brown", db_version = "2018.1")
  expect_equal(nrow(transcripts), 214)
  expect_identical(
    names(transcripts),
    c("transcript_id", "corpus_name", "language", "date", "filename",
      "target_child_name", "target_child_age", "target_child_sex",
      "collection_name", "pid", "collection_id", "corpus_id",
      "target_child_id"))
  expect_type(transcripts$date, "character")
  # ages are converted from days to months
  expect_true(all(transcripts$target_child_age < 100, na.rm = TRUE))
})

test_that("get_tokens filters server-side and applies replacements", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.0")
  tokens <- get_tokens(corpus = "Brown", target_child = "Adam",
                       token = c("dog", "ball"), db_version = "2018.1")
  expect_gt(nrow(tokens), 100)
  expect_true(all(tolower(tokens$gloss) %in% c("dog", "ball")))
  expect_true(all(tokens$target_child_name == "Adam"))
  # replace = TRUE drops the replacement column (fixed in 0.3.0)
  expect_false("replacement" %in% names(tokens))
})

test_that("get_participants errors on a missing child", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.0")
  expect_error(
    get_participants(corpus = "Brown", target_child = "NoSuchChild",
                     db_version = "2018.1"),
    "Duplicate or missing child")
})

test_that("deprecated connection interface still works against Redivis", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.0")
  expect_warning(con <- connect_to_childes(), "deprecated")
  expect_null(con)
  collections <- get_collections(connection = con, db_version = "2018.1")
  expect_equal(nrow(collections), 20)
})
