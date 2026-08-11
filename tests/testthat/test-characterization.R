# Characterization tests: every get_* call must reproduce the output of the
# MySQL-backed childesr 0.2.3.9000 run against db_version "2021.1", which is
# released on Redivis as childes_db v1.3. Row order is not guaranteed by
# either backend, so comparisons sort rows (see helper-fixtures.R).

DB <- "2021.1"
DB_TAG <- "v1.3"

test_that("get_collections matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(get_collections(db_version = DB), "collections")
})

test_that("get_corpora matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(get_corpora(db_version = DB), "corpora")
})

test_that("get_transcripts matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(get_transcripts(corpus = "Brown", db_version = DB),
                         "transcripts_brown")
  expect_matches_fixture(
    get_transcripts(collection = "Eng-NA", corpus = c("Brown", "Clark"),
                    db_version = DB),
    "transcripts_brown_clark")
  expect_matches_fixture(get_transcripts(target_child = "Shem",
                                         db_version = DB),
                         "transcripts_shem")
})

test_that("get_transcripts full pull has legacy shape", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_shape(get_transcripts(db_version = DB),
                       "transcripts_all_shape")
})

test_that("get_participants matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(get_participants(corpus = "Brown", db_version = DB),
                         "participants_brown")
  expect_matches_fixture(
    get_participants(corpus = "Brown", role = "Target_Child",
                     db_version = DB),
    "participants_brown_tc")
  expect_matches_fixture(
    get_participants(corpus = "Clark", role_exclude = "Target_Child",
                     db_version = DB),
    "participants_clark_nontc")
  expect_matches_fixture(
    get_participants(corpus = "Brown", age = c(24, 36), db_version = DB),
    "participants_brown_age")
  expect_matches_fixture(
    get_participants(corpus = "Brown", sex = "female", db_version = DB),
    "participants_brown_female")
  expect_matches_fixture(
    get_participants(corpus = "Brown", target_child = "Adam",
                     db_version = DB),
    "participants_adam")
})

test_that("get_participants full pull has legacy shape", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_shape(get_participants(db_version = DB),
                       "participants_all_shape")
})

test_that("get_speaker_statistics matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(
    get_speaker_statistics(corpus = "Brown", db_version = DB),
    "speaker_stats_brown")
  expect_matches_fixture(
    get_speaker_statistics(corpus = "Brown", role = "Target_Child",
                           age = c(24, 48), db_version = DB),
    "speaker_stats_brown_tc_age")
  expect_matches_shape(
    get_speaker_statistics(collection = "Eng-NA", db_version = DB),
    "speaker_stats_engna_shape")
})

test_that("get_tokens matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  # DELIBERATE FIXTURE DEVIATION in 0.3: get_tokens(replace = TRUE) now
  # correctly drops the `replacement` column. The MySQL-backed childesr
  # 0.2.3.9000 that generated these fixtures intended the same, but a
  # %<>%/|> operator-precedence bug discarded its select(-"replacement"),
  # so replace = TRUE fixtures retain the column; compare minus it.
  expect_matches_fixture(
    get_tokens(corpus = "Brown", target_child = "Adam",
               role = "Target_Child", token = c("dog", "ball"),
               db_version = DB),
    "tokens_adam_dogball", minus_cols = "replacement")
  expect_matches_fixture(
    get_tokens(corpus = "Brown", target_child = "Adam",
               token = c("dog", "ball"), replace = FALSE, db_version = DB),
    "tokens_adam_dogball_noreplace")
  expect_matches_fixture(
    get_tokens(corpus = "Brown", role = "Target_Child", stem = "run",
               token = "*", db_version = DB),
    "tokens_brown_stem_run", minus_cols = "replacement")
  expect_matches_shape(
    get_tokens(corpus = "Brown", target_child = "Adam", token = "*",
               part_of_speech = "n", db_version = DB),
    "tokens_adam_nouns_shape", minus_cols = "replacement")
})

test_that("get_types matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(
    get_types(corpus = "Brown", target_child = "Adam",
              type = c("dog", "ball"), db_version = DB),
    "types_adam_dogball")
  expect_matches_fixture(get_types(language = "spa", type = "perro",
                                   db_version = DB),
                         "types_spa_perro")
})

test_that("get_utterances matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(get_utterances(corpus = "Bohannon",
                                        db_version = DB),
                         "utterances_bohannon")
  expect_matches_shape(
    get_utterances(corpus = "Brown", target_child = "Adam",
                   db_version = DB),
    "utterances_adam_shape")
})

test_that("get_contexts matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  expect_matches_fixture(
    get_contexts(corpus = "Brown", target_child = "Adam", token = "lion",
                 window = c(1, 1), db_version = DB),
    "contexts_adam_lion")
})

test_that("get_sql_query matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased(DB_TAG)
  # DELIBERATE DIALECT CHANGE in 0.3: get_sql_query passes queries through
  # to BigQuery Standard SQL, where string comparison is case-sensitive
  # (MySQL's default collation was case-insensitive). The legacy fixture
  # query `gloss = 'dog'` is therefore translated to `LOWER(gloss) = 'dog'`;
  # the returned data must still match the legacy output exactly.
  expect_matches_fixture(
    get_sql_query(paste(
      "SELECT corpus_name, COUNT(id) AS count FROM token",
      "WHERE collection_name = 'Eng-NA' AND LOWER(gloss) = 'dog'",
      "GROUP BY corpus_name"), db_version = DB),
    "sql_dog_counts")
})

test_that("get_collections on a pinned older version matches legacy output", {
  skip_if_no_redivis()
  skip_if_version_unreleased("v1.2")
  expect_matches_fixture(get_collections(db_version = "2020.1"),
                         "collections_2020")
})
