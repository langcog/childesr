#!/usr/bin/env Rscript
# Generate characterization fixtures by running the CURRENT (MySQL-backed)
# childesr against the live hosted database, pinned to db_version "2021.1"
# (the same database state staged to Redivis as the childes-db dataset).
# The redivis backend is then developed against these golden outputs:
# same call -> same tibble.
#
# Run from the package root with the MySQL-backed childesr (0.2.3.9000)
# installed:
#   Rscript data-raw/make_fixtures.R
#
# Small results are stored in full; large pulls are stored as "shape"
# fixtures (dims, names, classes, distinct counts) to keep the repo light.

suppressMessages({
  library(childesr)
  library(dplyr)
  library(purrr)
})

DB <- "2021.1"
dir.create("tests/testthat/fixtures", recursive = TRUE, showWarnings = FALSE)

save_fixture <- function(x, name) {
  saveRDS(x, file.path("tests/testthat/fixtures", paste0(name, ".rds")),
          version = 2)
  message("wrote ", name, " (", paste(dim(x), collapse = " x "), ")")
}

shape <- function(x) {
  list(nrow = nrow(x), names = names(x),
       classes = map_chr(x, ~ paste(class(.x), collapse = "/")),
       n_distinct = map_int(x, dplyr::n_distinct))
}
save_shape <- function(x, name) {
  saveRDS(shape(x), file.path("tests/testthat/fixtures", paste0(name, ".rds")),
          version = 2)
  message("wrote shape ", name, " (", nrow(x), " rows)")
}

# ---- collections / corpora ---------------------------------------------------

save_fixture(get_collections(db_version = DB), "collections")
save_fixture(get_corpora(db_version = DB), "corpora")

# ---- transcripts -------------------------------------------------------------

save_fixture(get_transcripts(corpus = "Brown", db_version = DB),
             "transcripts_brown")
save_fixture(get_transcripts(collection = "Eng-NA",
                             corpus = c("Brown", "Clark"), db_version = DB),
             "transcripts_brown_clark")
save_fixture(get_transcripts(target_child = "Shem", db_version = DB),
             "transcripts_shem")
save_shape(get_transcripts(db_version = DB), "transcripts_all_shape")

# ---- participants ------------------------------------------------------------

save_fixture(get_participants(corpus = "Brown", db_version = DB),
             "participants_brown")
save_fixture(get_participants(corpus = "Brown", role = "Target_Child",
                              db_version = DB),
             "participants_brown_tc")
save_fixture(get_participants(corpus = "Clark",
                              role_exclude = "Target_Child", db_version = DB),
             "participants_clark_nontc")
save_fixture(get_participants(corpus = "Brown", age = c(24, 36),
                              db_version = DB),
             "participants_brown_age")
save_fixture(get_participants(corpus = "Brown", sex = "female",
                              db_version = DB),
             "participants_brown_female")
save_fixture(get_participants(corpus = "Brown", target_child = "Adam",
                              db_version = DB),
             "participants_adam")
save_shape(get_participants(db_version = DB), "participants_all_shape")

# ---- speaker statistics ------------------------------------------------------

save_fixture(get_speaker_statistics(corpus = "Brown", db_version = DB),
             "speaker_stats_brown")
save_fixture(get_speaker_statistics(corpus = "Brown", role = "Target_Child",
                                    age = c(24, 48), db_version = DB),
             "speaker_stats_brown_tc_age")
save_shape(get_speaker_statistics(collection = "Eng-NA", db_version = DB),
           "speaker_stats_engna_shape")

# ---- tokens ------------------------------------------------------------------

save_fixture(get_tokens(corpus = "Brown", target_child = "Adam",
                        role = "Target_Child", token = c("dog", "ball"),
                        db_version = DB),
             "tokens_adam_dogball")
save_fixture(get_tokens(corpus = "Brown", target_child = "Adam",
                        token = c("dog", "ball"), replace = FALSE,
                        db_version = DB),
             "tokens_adam_dogball_noreplace")
save_fixture(get_tokens(corpus = "Brown", role = "Target_Child",
                        stem = "run", token = "*", db_version = DB),
             "tokens_brown_stem_run")
save_shape(get_tokens(corpus = "Brown", target_child = "Adam", token = "*",
                      part_of_speech = "n", db_version = DB),
           "tokens_adam_nouns_shape")

# ---- types -------------------------------------------------------------------

save_fixture(get_types(corpus = "Brown", target_child = "Adam",
                       type = c("dog", "ball"), db_version = DB),
             "types_adam_dogball")
save_fixture(get_types(language = "spa", type = "perro", db_version = DB),
             "types_spa_perro")

# ---- utterances --------------------------------------------------------------

save_fixture(get_utterances(corpus = "Bohannon", db_version = DB),
             "utterances_bohannon")
save_shape(get_utterances(corpus = "Brown", target_child = "Adam",
                          db_version = DB),
           "utterances_adam_shape")

# ---- contexts ----------------------------------------------------------------

save_fixture(get_contexts(corpus = "Brown", target_child = "Adam",
                          token = "lion", window = c(1, 1), db_version = DB),
             "contexts_adam_lion")

# ---- raw SQL (vignette example) ---------------------------------------------

save_fixture(
  get_sql_query(paste(
    "SELECT corpus_name, COUNT(id) AS count FROM token",
    "WHERE collection_name = 'Eng-NA' AND gloss = 'dog'",
    "GROUP BY corpus_name"), db_version = DB),
  "sql_dog_counts")

# ---- version pinning ---------------------------------------------------------

save_fixture(get_collections(db_version = "2020.1"), "collections_2020")

message("done; childesr ", as.character(packageVersion("childesr")),
        ", db_version ", DB)
