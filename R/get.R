avg_month <- 365.2425 / 12

# canonical column orders, matching the MySQL childes-db table layouts that
# previous versions of childesr returned (Redivis does not guarantee column
# order); columns not present in a given dataset version are skipped, columns
# not listed are kept at the end
column_orders <- list(
  collection = c(
    "id", "name", "data_source"),
  corpus = c(
    "id", "name", "collection_name", "data_source", "collection_id"),
  transcript = c(
    "id", "corpus_name", "language", "date", "filename", "target_child_name",
    "target_child_age", "target_child_sex", "collection_name", "pid",
    "collection_id", "corpus_id", "target_child_id"),
  participant = c(
    "id", "code", "name", "role", "corpus_name", "min_age", "max_age",
    "language", "group", "sex", "ses", "education", "custom",
    "collection_name", "collection_id", "corpus_id", "target_child_id"),
  transcript_by_speaker = c(
    "id", "speaker_role", "language", "target_child_name", "target_child_age",
    "target_child_sex", "num_utterances", "mlu_w", "mlu_m", "mtld", "hdd",
    "num_types", "num_tokens", "num_morphemes", "collection_name",
    "collection_id", "corpus_id", "speaker_id", "target_child_id",
    "transcript_id"),
  token = c(
    "id", "gloss", "language", "token_order", "replacement", "prefix",
    "part_of_speech", "stem", "actual_phonology", "model_phonology", "suffix",
    "num_morphemes", "english", "clitic", "utterance_type", "corpus_name",
    "speaker_code", "speaker_name", "speaker_role", "target_child_name",
    "target_child_age", "target_child_sex", "collection_name",
    "collection_id", "corpus_id", "speaker_id", "target_child_id",
    "transcript_id", "utterance_id"),
  token_frequency = c(
    "id", "gloss", "count", "speaker_role", "language", "target_child_name",
    "target_child_age", "target_child_sex", "collection_name",
    "collection_id", "corpus_id", "speaker_id", "target_child_id",
    "transcript_id"),
  utterance = c(
    "id", "gloss", "stem", "actual_phonology", "model_phonology", "type",
    "language", "num_morphemes", "num_tokens", "utterance_order",
    "corpus_name", "part_of_speech", "speaker_code", "speaker_name",
    "speaker_role", "target_child_name", "target_child_age",
    "target_child_sex", "media_start", "media_end", "media_unit",
    "collection_name", "collection_id", "corpus_id", "speaker_id",
    "target_child_id", "transcript_id")
)

order_columns <- function(tbl, name) {
  dplyr::select(tbl, dplyr::any_of(column_orders[[name]]), dplyr::everything())
}

# internal engine for get_transcripts, shared by the other getters
get_transcripts_table <- function(collection = NULL, corpus = NULL,
                                  target_child = NULL, tag) {

  transcripts <- childes_table("transcript", tag)
  if (is.null(transcripts)) return(NULL)

  transcripts <- transcripts |>
    order_columns("transcript") |>
    dplyr::mutate(date = as.character(.data$date)) |>
    dplyr::rename(transcript_id = "id")

  if (!is.null(collection)) {
    transcripts %<>% dplyr::filter(.data$collection_name %in% collection)
  }
  if (!is.null(corpus)) {
    transcripts %<>% dplyr::filter(.data$corpus_name %in% corpus)
  }
  if (!is.null(target_child)) {
    transcripts %<>% dplyr::filter(.data$target_child_name %in% target_child)
  }

  transcripts %<>%
    dplyr::mutate(target_child_age = .data$target_child_age / avg_month)

  transcripts
}

#' Get collections
#'
#' @inheritParams connect_to_childes
#' @param connection Deprecated, ignored (childesr now reads from the
#'   childes-db dataset on Redivis)
#'
#' @return A `tbl` of Collection data
#' @export
#'
#' @examples
#' \dontrun{
#' get_collections()
#' }
get_collections <- function(connection = NULL, db_version = "current",
                            db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  collections <- childes_table("collection", ver$tag)
  if (is.null(collections)) return(invisible(NULL))

  collections |>
    order_columns("collection") |>
    dplyr::rename(collection_id = "id") |>
    dplyr::rename(collection_name = "name")
}

#' Get corpora
#'
#' @inheritParams get_collections
#'
#' @return A `tbl` of Corpus data
#' @export
#'
#' @examples
#' \dontrun{
#' get_corpora()
#' }
get_corpora <- function(connection = NULL, db_version = "current",
                        db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  corpora <- childes_table("corpus", ver$tag)
  if (is.null(corpora)) return(invisible(NULL))

  corpora |>
    order_columns("corpus") |>
    dplyr::rename(corpus_id = "id") |>
    dplyr::rename(corpus_name = "name")
}

#' Get transcripts
#'
#' @param collection A character vector of one or more names of collections
#' @param corpus A character vector of one or more names of corpora
#' @param target_child A character vector of one or more names of children
#' @inheritParams get_collections
#'
#' @section Identifiers:
#' Numeric ids in childes-db (`transcript_id`, `utterance_id`, token `id`,
#' and so on) are internal to a database release: they are not stable across
#' versions of childes-db and should never be used to link data across
#' releases. The TalkBank persistent identifier (the `pid` column returned
#' by `get_transcripts()`) is the stable, externally-facing identifier for a
#' transcript; use it to match transcripts across database versions or with
#' other TalkBank tools. For reproducible analyses, pin the database version
#' with the `db_version` argument.
#'
#' @return A `tbl` of Transcript data, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_transcripts()
#' }
get_transcripts <- function(collection = NULL, corpus = NULL,
                            target_child = NULL, connection = NULL,
                            db_version = "current", db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  transcripts <- get_transcripts_table(collection, corpus, target_child,
                                       ver$tag)
  if (is.null(transcripts)) return(invisible(NULL))
  transcripts
}

#' Get participants
#'
#' @inheritParams get_transcripts
#' @param role A character vector of one or more roles to include
#' @param role_exclude A character vector of one or more roles to exclude
#' @param age A numeric vector of an single age value or a min age value and max
#'   age value (inclusive) in months. For a single age value, participants are
#'   returned for which that age is within their age range; for two ages,
#'   participants are returned for whose age overlaps with the interval between
#'   those two ages.
#' @param sex A character vector of values "male" and/or "female"
#'
#' @inheritSection get_transcripts Identifiers
#' @return A `tbl` of Participant data, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_participants()
#' }
get_participants <- function(collection = NULL, corpus = NULL,
                             target_child = NULL, role = NULL,
                             role_exclude = NULL, age = NULL, sex = NULL,
                             connection = NULL, db_version = "current",
                             db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  participants <- childes_table("participant", ver$tag)
  if (is.null(participants)) return(invisible(NULL))
  participants %<>% order_columns("participant")

  if (!is.null(collection)) {
    participants %<>% dplyr::filter(.data$collection_name %in% collection)
  }

  if (!is.null(corpus)) {
    participants %<>% dplyr::filter(.data$corpus_name %in% corpus)
  }

  if (!is.null(age)) {
    days <- age * avg_month
    if (length(age) == 1) {
      participants %<>% dplyr::filter(.data$max_age >= days & .data$min_age <= days)
    } else if (length(age) == 2) {
      days_1 <- days[1]
      days_2 <- days[2]
      participants %<>% dplyr::filter((.data$max_age >= days_1 & .data$min_age <= days_2) |
                                        (.data$min_age <= days_2 & .data$max_age >= days_1))
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    participants %<>% dplyr::filter(.data$sex %in% sex_filter)
  }

  if (!is.null(target_child)) {
    child_id <- participants |>
      dplyr::filter(.data$name == target_child) |>
      dplyr::pull(.data$target_child_id) |>
      unique()
    if (length(child_id) != 1) {
      stop("Duplicate or missing child")
    } else {
      participants %<>% dplyr::filter(.data$target_child_id == child_id)
    }
  }

  if (!is.null(role)) {
    role_filter <- role
    participants %<>% dplyr::filter(.data$role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    participants %<>% dplyr::filter(!(.data$role %in% role_exclude))
  }

  transcripts <- get_transcripts_table(collection, corpus, target_child,
                                       ver$tag)
  if (is.null(transcripts)) return(invisible(NULL))

  target_children <- transcripts |>
    dplyr::select("target_child_id", "target_child_name") |>
    dplyr::distinct()

  # TODO remove after https://github.com/langcog/childes-db/issues/30 resolved
  participants %<>%
    dplyr::left_join(target_children, by = "target_child_id")

  participants %<>% dplyr::mutate(max_age = .data$max_age / avg_month)
  participants %<>% dplyr::mutate(min_age = .data$min_age / avg_month)

  participants
}

#' Get speaker statistics
#'
#' @inheritParams get_participants
#' @inheritSection get_transcripts Identifiers
#' @return A `tbl` of Speaker statistics, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_speaker_statistics()
#' }
get_speaker_statistics <- function(collection = NULL, corpus = NULL,
                                   target_child = NULL, role = NULL,
                                   role_exclude = NULL, age = NULL, sex = NULL,
                                   connection = NULL, db_version = "current",
                                   db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  transcripts <- get_transcripts_table(collection, corpus, target_child,
                                       ver$tag)
  if (is.null(transcripts)) return(invisible(NULL))

  speaker_statistics <- childes_table("transcript_by_speaker", ver$tag)
  if (is.null(speaker_statistics)) return(invisible(NULL))
  speaker_statistics %<>% order_columns("transcript_by_speaker")

  # NB: NAs are dropped from the id filters to mirror the SQL semantics of
  # the retired MySQL backend, where `IN (..., NULL)` never matches NULL
  if (!is.null(collection)) {
    collection_filter <- transcripts |>
      dplyr::select("collection_id", "target_child_id") |>
      dplyr::distinct() |>
      dplyr::pull(.data$target_child_id)
    collection_filter <- collection_filter[!is.na(collection_filter)]

    speaker_statistics %<>%
      dplyr::filter(.data$target_child_id %in% collection_filter)
  }

  if (!is.null(corpus)) {
    corpus_filter <- transcripts |>
      dplyr::select("corpus_id", "target_child_id") |>
      dplyr::distinct() |>
      dplyr::pull(.data$target_child_id)
    corpus_filter <- corpus_filter[!is.na(corpus_filter)]

    speaker_statistics %<>% dplyr::filter(.data$target_child_id %in% corpus_filter)
  }

  if (!is.null(age)) {
    if (!(length(age) %in% 1:2)) stop("`age` argument must be of length 1 or 2")
    days <- age * avg_month
    if (length(age) == 1) days <- c(days, days + avg_month)
    days_1 <- days[1]
    days_2 <- days[2]
    speaker_statistics %<>% dplyr::filter(.data$target_child_age >= days_1,
                                          .data$target_child_age <= days_2)
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    speaker_statistics %<>% dplyr::filter(.data$sex %in% sex_filter)
  }

  if (!is.null(target_child)) {
    speaker_statistics %<>% dplyr::filter(.data$target_child_name %in% target_child)
  }

  if (!is.null(role)) {
    role_filter <- role
    speaker_statistics %<>% dplyr::filter(.data$speaker_role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    speaker_statistics %<>% dplyr::filter(!(.data$speaker_role %in% role_exclude))
  }

  speaker_statistics %<>%
    dplyr::mutate(target_child_age = .data$target_child_age / avg_month)

  speaker_statistics
}

#' Get content
#'
#' Internal engine for the content getters (`get_tokens`, `get_types`,
#' `get_utterances`). Filters are translated into a BigQuery Standard SQL
#' query that runs server-side on Redivis, so that only the matching rows of
#' the (very large) content tables are transferred. String comparisons are
#' case-insensitive, mirroring the collation of the retired MySQL server.
#'
#' @inheritParams get_participants
#' @param content_type One of "token", "utterance" or "token_frequency"
#' @param token A character vector of one or more token patterns (`\%` matches
#'   any number of wildcard characters, `_` matches exactly one wildcard
#'   character)
#' @param stem A character vector of one or more stems
#' @param part_of_speech A character vector of one or more parts of speech
#' @param language A character vector of one or more languages
#' @param tag Redivis dataset version tag (e.g. "v1.3")
#' @keywords internal
get_content <- function(content_type, collection = NULL, language = NULL,
                        corpus = NULL, role = NULL, role_exclude = NULL,
                        age = NULL, sex = NULL, target_child = NULL,
                        token = NULL, stem = NULL, part_of_speech = NULL,
                        tag) {

  transcripts <- get_transcripts_table(collection, corpus, target_child, tag)
  if (is.null(transcripts)) return(NULL)

  corpora <- transcripts |>
    dplyr::select("corpus_id") |>
    dplyr::distinct()
  child_id <- transcripts |>
    dplyr::select("target_child_id") |>
    dplyr::distinct() |>
    dplyr::pull(.data$target_child_id)

  num_children <- length(child_id)
  num_corpora <- nrow(corpora)

  message("Getting data from ", num_children,
          ifelse(num_children == 1, " child", " children"), " in ",
          num_corpora, ifelse(num_corpora == 1, " corpus ", " corpora"), "...")

  # build up WHERE clauses corresponding to the supplied filters
  wheres <- character()

  # case-insensitive IN (...) condition, like MySQL's default collation
  sql_in <- function(column, values, negate = FALSE) {
    sprintf("LOWER(%s) %sIN (%s)", column, if (negate) "NOT " else "",
            paste(quote_sql(tolower(values)), collapse = ", "))
  }
  sql_id_in <- function(column, ids) {
    ids <- ifelse(is.na(ids), "NULL", as.character(ids))
    sprintf("%s IN (%s)", column, paste(ids, collapse = ", "))
  }

  if (content_type %in% c("token", "token_frequency") && !is.null(token) &&
      !identical("*", token)) {
    wheres <- c(wheres, paste0(
      "(", paste(sprintf("LOWER(gloss) LIKE %s", quote_sql(tolower(token))),
                 collapse = " OR "), ")"))
  }

  if (!is.null(stem)) {
    wheres <- c(wheres, sql_in("stem", stem))
  }

  if (!is.null(part_of_speech)) {
    wheres <- c(wheres, sql_in("part_of_speech", part_of_speech))
  }

  if (!num_corpora) {
    corpus_filter <- -1
    child_id <- -1
  } else {
    corpus_filter <- corpora$corpus_id
  }

  if (!is.null(collection) | !is.null(corpus)) {
    wheres <- c(wheres, sql_id_in("corpus_id", corpus_filter))
  }

  if (!is.null(target_child)) {
    wheres <- c(wheres, sql_id_in("target_child_id", child_id))
  }

  if (!is.null(age)) {
    if (!(length(age) %in% 1:2)) stop("`age` argument must be of length 1 or 2")
    days <- age * avg_month
    if (length(age) == 1) days <- c(days, days + avg_month)
    wheres <- c(wheres, sprintf(
      "target_child_age >= %.10f AND target_child_age <= %.10f",
      days[1], days[2]))
  }

  if (!is.null(sex)) {
    wheres <- c(wheres, sql_in("sex", sex))
  }

  if (!is.null(role)) {
    wheres <- c(wheres, sql_in("speaker_role", role))
  }

  if (!is.null(role_exclude)) {
    wheres <- c(wheres, sql_in("speaker_role", role_exclude, negate = TRUE))
  }

  if (!is.null(language)) {
    wheres <- c(wheres, sql_in("language", language))
  }

  sql <- paste0("SELECT * FROM ", content_type,
                if (length(wheres) > 0) {
                  paste0(" WHERE ", paste(wheres, collapse = " AND "))
                })

  content <- childes_query(sql, tag)
  if (is.null(content)) return(NULL)

  content |>
    order_columns(content_type) |>
    dplyr::mutate(target_child_age = .data$target_child_age / avg_month)
}


#' Get tokens
#'
#' @inheritParams get_collections
#' @inheritParams get_content
#' @param replace A boolean indicating whether to replace "gloss" with
#'   "replacement" (i.e. phonologically assimilated form), when available
#'   (defaults to \code{TRUE})
#'
#' @inheritSection get_transcripts Identifiers
#' @return A `tbl` of Token data, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_tokens(token = "dog")
#' }
get_tokens <- function(collection = NULL, language = NULL, corpus = NULL,
                       target_child = NULL, role = NULL, role_exclude = NULL,
                       age = NULL, sex = NULL, token, stem = NULL,
                       part_of_speech = NULL, replace = TRUE, connection = NULL,
                       db_version = "current", db_args = NULL) {

  if (missing(token))
    stop("Argument 'token' is missing. To fetch all tokens, supply '*' for ",
         "argument 'token'. Caution: this may result in a long-running query.")

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  tokens <- get_content(content_type = "token",
                        collection = collection,
                        language = language,
                        corpus = corpus,
                        role = role,
                        role_exclude = role_exclude,
                        age = age,
                        sex = sex,
                        target_child = target_child,
                        token = token,
                        stem = stem,
                        part_of_speech = part_of_speech,
                        tag = ver$tag)
  if (is.null(tokens)) return(invisible(NULL))

  if (replace) {
    # gloss is swapped for replacement whenever a replacement is present,
    # and the replacement column is dropped. (The MySQL-backed childesr
    # 0.2.3.9000 intended the same but retained the column: its
    # select(-"replacement") result was discarded due to %<>%/|> operator
    # precedence. Fixed deliberately in 0.3.0.)
    tokens %<>%
      dplyr::mutate(gloss = dplyr::if_else(
        !is.na(.data$replacement) & .data$replacement == "",
        .data$gloss, .data$replacement))
    tokens %<>% dplyr::select(-"replacement")
  }

  tokens
}


#' Get types
#'
#' @inheritParams get_collections
#' @inheritParams get_content
#' @param type A character vector of one or more type patterns (`%` matches any
#'   number of wildcard characters, `_` matches exactly one wildcard character)
#'
#' @inheritSection get_transcripts Identifiers
#' @return A `tbl` of Type data, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_types()
#' }
get_types <- function(collection = NULL, language = NULL, corpus = NULL,
                      role = NULL, role_exclude = NULL, age = NULL, sex = NULL,
                      target_child = NULL, type = NULL, connection = NULL,
                      db_version = "current", db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  types <- get_content(content_type = "token_frequency",
                       collection = collection,
                       language = language,
                       corpus = corpus,
                       role = role,
                       role_exclude = role_exclude,
                       age = age,
                       sex = sex,
                       target_child = target_child,
                       token = type,
                       tag = ver$tag)
  if (is.null(types)) return(invisible(NULL))
  types
}

#' Get utterances
#'
#' @inheritParams get_participants
#' @param language A character vector of one or more languages
#'
#' @inheritSection get_transcripts Identifiers
#' @return A `tbl` of Utterance data, filtered down by supplied arguments
#' @export
#'
#' @examples
#' \dontrun{
#' get_utterances(target_child = "Shem")
#' }
get_utterances <- function(collection = NULL, language = NULL, corpus = NULL,
                           role = NULL, role_exclude = NULL, age = NULL,
                           sex = NULL, target_child = NULL, connection = NULL,
                           db_version = "current", db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  utterances <- get_content(content_type = "utterance",
                            collection = collection,
                            language = language,
                            corpus = corpus,
                            role = role,
                            role_exclude = role_exclude,
                            age = age,
                            sex = sex,
                            target_child = target_child,
                            tag = ver$tag)
  if (is.null(utterances)) return(invisible(NULL))
  utterances
}

#' Get the utterances surrounding a token(s)
#'
#' @inheritParams get_utterances
#' @inheritParams get_tokens
#' @param window A length 2 numeric vector of how many utterances before and
#'   after each utterance containing the target token to retrieve
#' @param remove_duplicates A boolean indicating whether to remove duplicate
#'   utterances from the results
#'
#' @inheritSection get_transcripts Identifiers
#' @return A 'tbl' of Utterance data, filtered down by supplied arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' get_contexts(target_child = "Shem", token = "dog")
#' }
get_contexts <- function(collection = NULL, language = NULL, corpus = NULL,
                        role = NULL, role_exclude = NULL, age = NULL,
                        sex = NULL, target_child = NULL, token,
                        window = c(0, 0), remove_duplicates = TRUE,
                        connection = NULL, db_version = "current",
                        db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  tokens <- get_content(content_type = "token",
                        collection = collection,
                        language = language,
                        corpus = corpus,
                        role = role,
                        role_exclude = role_exclude,
                        age = age,
                        sex = sex,
                        target_child = target_child,
                        token = token,
                        tag = ver$tag)
  if (is.null(tokens)) return(invisible(NULL))
  token_utterances <- unique(tokens$utterance_id)

  suppressMessages(
    utterances <- get_content(content_type = "utterance",
                              collection = collection,
                              language = language,
                              corpus = corpus,
                              role = role,
                              role_exclude = role_exclude,
                              age = age,
                              sex = sex,
                              target_child = target_child,
                              tag = ver$tag)
  )
  if (is.null(utterances)) return(invisible(NULL))
  utterances %<>% dplyr::rename(utterance_id = "id")

  utterance_orders <- utterances |>
    dplyr::filter(.data$utterance_id %in% token_utterances) |>
    dplyr::select("transcript_id", "utterance_order")

  # each matched utterance contributes the utterances in its window
  # [utterance_order - window[1], utterance_order + window[2]]; since
  # utterance_order is an integer this is equivalent to joining on the
  # expanded set of (transcript_id, utterance_order) pairs, which needs only
  # the two content queries above rather than one query per matched token
  targets <- purrr::map2(
    utterance_orders$transcript_id, utterance_orders$utterance_order,
    function(tid, index) dplyr::tibble(
      transcript_id = tid,
      utterance_order = rlang::seq2(index - window[1], index + window[2]))) |>
    purrr::list_rbind()
  if (nrow(targets) == 0) {
    targets <- utterance_orders[0, c("transcript_id", "utterance_order")]
  }

  contexts <- dplyr::inner_join(utterances, targets,
                                by = c("transcript_id", "utterance_order"),
                                relationship = "many-to-many")

  if (remove_duplicates) {
    contexts %<>% dplyr::distinct(.data$transcript_id, .data$utterance_id,
                                  .keep_all = TRUE)
  }

  contexts
}


#' Run a SQL Query script on the CHILDES database
#'
#' As of childesr 0.3, queries run against the childes-db dataset on Redivis,
#' whose query engine uses BigQuery Standard SQL rather than MySQL SQL.
#' Standard SQL queries against the childes-db tables (`collection`,
#' `corpus`, `transcript`, `participant`, `transcript_by_speaker`,
#' `utterance`, `token`, `token_frequency`) work unchanged; queries using
#' MySQL-specific syntax may need to be updated (see
#' \url{https://cloud.google.com/bigquery/docs/reference/standard-sql/}).
#'
#' @inheritParams get_collections
#' @param sql_query_string A valid BigQuery Standard SQL query string
#'
#' @return The result of running the supplied SQL query on the childes-db
#'   dataset
#' @export
#'
#' @examples
#' \dontrun{
#' get_sql_query("SELECT * FROM collection")
#' }
get_sql_query <- function(sql_query_string, connection = NULL,
                          db_version = "current", db_args = NULL) {

  check_connection(connection)
  ver <- resolve_version(db_version, db_args)

  result <- childes_query(sql_query_string, ver$tag)
  if (is.null(result)) return(invisible(NULL))
  result
}
