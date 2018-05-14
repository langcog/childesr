#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
NULL

utils::globalVariables(c("collection_id", "collection_name", "corpus_id",
                         "corpus_name", "gloss", "id", "max_age", "min_age",
                         "name", "speaker_role", "target_child_id",
                         "target_child_name", "target_child_age",
                         "utterance_id", "transcript_id", "utterance_order",
                         "replacement"))

avg_month <- 365.2425 / 12

translate_version <- function(db_version, db_args, db_info) {

  # using the childes-db hosted server
  if (db_args$host == db_info$host) {

    # current version
    if (db_version == "current") {
      db_to_use <- db_info[["current"]]
      message("Using current database version: '", db_to_use, "'.")
      return("childesdb")

    # supported version
    } else if (db_version %in% db_info[["supported"]]) {
      db_to_use <- db_version
      message("Using supported database version: '", db_to_use, "'.")
      return("childesdb")

    # historical version
    } else if (db_version %in% db_info[["historical"]]) {
      stop("Version '", db_version, "' is no longer hosted by ",
           "childes-db.stanford.edu; either specify a more recent version or ",
           "install MySQL Server locally and update db_args.")

    # version not recognized
    } else {
      stop("Version '", db_version, "' not found. Specify one of: 'current', ",
           paste(sprintf("'%s'", db_info$supported), collapse = ", "), ".")
    }

  # using a different server than the childes-db hosted one
  } else {
    message("Not using hosted database version; no checks will be applied to ",
            "version specification.")
    return(db_args$db_name)
  }
}

resolve_connection <- function(connection, db_version = NULL, db_args = NULL) {
  if (is.null(connection)) connect_to_childes(db_version, db_args)
  else connection
}

#' Connect to CHILDES
#'
#' @param db_version String of the name of database version to use
#' @param db_args List with host, user, and password defined
#' @return con A DBIConnection object for the CHILDES database
#' @export
#'
#' @examples
#' \donttest{
#' con <- connect_to_childes(db_version = "current", db_args = NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_childes <- function(db_version = "current", db_args = NULL) {

  db_info <- jsonlite::fromJSON(
    "https://childes-db.stanford.edu/childes-db.json"
  )

  if (is.null(db_args)) db_args <- db_info

  con <- DBI::dbConnect(
    RMySQL::MySQL(),
    host = db_args$host,
    dbname = translate_version(db_version, db_args, db_info),
    user = db_args$user,
    password = db_args$password
  )
  DBI::dbGetQuery(con, "SET NAMES utf8")
  return(con)
}

#' Get table
#'
#' @param connection A connection to the CHILDES database
#' @param name String of a table name
#'
#' @return A `tbl`
get_table <- function(connection, name) {
  dplyr::tbl(connection, name)
}

#' Get collections
#'
#' @inheritParams connect_to_childes
#' @param connection A connection to the CHILDES database
#'
#' @return A `tbl` of Collection data. If `connection` is supplied, the result
#'   remains a remote query, otherwise it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_collections()
#' }
get_collections <- function(connection = NULL, db_version = "current",
                            db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  collections <- dplyr::tbl(con, "collection") %>%
    dplyr::rename(collection_id = id) %>%
    dplyr::rename(collection_name = name)

  if (is.null(connection)) {
    collections %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(collections)
}

#' Get corpora
#'
#' @inheritParams get_collections
#'
#' @return A `tbl` of Corpus data. If `connection` is supplied, the result
#'   remains a remote query, otherwise it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_corpora()
#' }
get_corpora <- function(connection = NULL, db_version = "current",
                        db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  corpora <- dplyr::tbl(con, "corpus") %>%
    dplyr::rename(corpus_id = id) %>%
    dplyr::rename(corpus_name = name)

  if (is.null(connection)) {
    corpora %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(corpora)
}

#' Get transcripts
#'
#' @param collection A character vector of one or more names of collections
#' @param corpus A character vector of one or more names of corpora
#' @param target_child A character vector of one or more names of children
#' @inheritParams get_collections
#'
#' @return A `tbl` of Transcript data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_transcripts()
#' }
get_transcripts <- function(collection = NULL, corpus = NULL,
                            target_child = NULL, connection = NULL,
                            db_version = "current", db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)

  transcripts <- get_table(con, "transcript") %>%
    dplyr::rename(transcript_id = id)

  if (!is.null(collection)) {
    transcripts %<>% dplyr::filter(collection_name %in% collection)
  }
  if (!is.null(corpus)) {
    transcripts %<>% dplyr::filter(corpus_name %in% corpus)
  }
  if (!is.null(target_child)) {
    transcripts %<>% dplyr::filter(target_child_name %in% target_child)
  }

  transcripts %<>%
    dplyr::mutate(target_child_age = target_child_age / avg_month)

  if (is.null(connection)) {
    transcripts %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(transcripts)

}

#' Get participants
#'
#' @inheritParams get_transcripts
#' @param role A character vector of one or more roles to include
#' @param role_exclude A character vector of one or more roles to exclude
#' @param age A numeric vector of an age or a min age (inclusive) and max age
#'   (exclusive) in months
#' @param sex A character vector of values "male" and/or "female"
#'
#' @return A `tbl` of Participant data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_participants()
#' }
get_participants <- function(collection = NULL, corpus = NULL,
                             target_child = NULL, role = NULL,
                             role_exclude = NULL, age = NULL, sex = NULL,
                             connection = NULL, db_version = "current",
                             db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  participants <- get_table(con, "participant")

  if (!is.null(collection)) {
    participants %<>% dplyr::filter(collection_name %in% collection)
  }

  if (!is.null(corpus)) {
    participants %<>% dplyr::filter(corpus_name %in% corpus)
  }

  if (!is.null(age)) {
    days <- age * avg_month
    if (length(age) == 1) {
      participants %<>% dplyr::filter(max_age >= days & min_age <= days)
    } else if (length(age) == 2) {
      participants %<>% dplyr::filter(max_age >= days[1] | min_age <= days[2])
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    participants %<>% dplyr::filter(sex %in% sex_filter)
  }

  if (!is.null(target_child)) {
    child_id <- participants %>%
      dplyr::filter(name == target_child) %>%
      dplyr::pull(target_child_id) %>%
      unique()
    if (length(child_id) != 1) {
      stop("Duplicate or missing child")
    } else {
      participants %<>% dplyr::filter(target_child_id == child_id)
    }
  }

  if (!is.null(role)) {
    role_filter <- role
    participants %<>% dplyr::filter(role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    participants %<>% dplyr::filter(!(role %in% role_exclude))
  }

  target_children <- get_transcripts(collection, corpus, target_child, con) %>%
    dplyr::distinct(target_child_id, target_child_name) %>%
    dplyr::select(target_child_id, target_child_name)

  # TODO remove after https://github.com/langcog/childes-db/issues/30 resolved
  participants %<>%
    dplyr::left_join(target_children, by = "target_child_id")

  participants %<>% dplyr::mutate(max_age = max_age / avg_month)
  participants %<>% dplyr::mutate(min_age = min_age / avg_month)

  if (is.null(connection)) {
    participants %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(participants)

}
#' Get speaker statistics
#'
#' @inheritParams get_participants
#' @return A `tbl` of Speaker statistics, filtered down by supplied arguments.
#'   If `connection` is supplied, the result remains a remote query, otherwise
#'   it is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_speaker_statistics()
#' }
get_speaker_statistics <- function(collection = NULL, corpus = NULL,
                                   target_child = NULL, role = NULL,
                                   role_exclude = NULL, age = NULL, sex = NULL,
                                   connection = NULL, db_version = "current",
                                   db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  transcripts <- get_transcripts(collection, corpus, target_child, con)
  speaker_statistics <- get_table(con, "transcript_by_speaker")

  if (!is.null(collection)) {
    collection_filter <- transcripts %>%
      dplyr::distinct(collection_id, target_child_id) %>%
      dplyr::pull(target_child_id)

    speaker_statistics %<>%
      dplyr::filter(target_child_id %in% collection_filter)
  }

  if (!is.null(corpus)) {
    corpus_filter <- transcripts %>%
      dplyr::distinct(corpus_id, target_child_id) %>%
      dplyr::pull(target_child_id)

    speaker_statistics %<>% dplyr::filter(target_child_id %in% corpus_filter)
  }


  if (!is.null(age)) {
    if (!(length(age) %in% 1:2)) stop("`age` argument must be of length 1 or 2")
    days <- age * avg_month
    if (length(age) == 1) days <- c(days, days + avg_month)
    speaker_statistics %<>% dplyr::filter(target_child_age >= days[1],
                                          target_child_age <= days[2])
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    speaker_statistics %<>% dplyr::filter(sex %in% sex_filter)
  }

  if (!is.null(target_child)) {
    speaker_statistics %<>% dplyr::filter(target_child_name %in% target_child)
  }

  if (!is.null(role)) {
    role_filter <- role
    speaker_statistics %<>% dplyr::filter(speaker_role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    speaker_statistics %<>% dplyr::filter(!(speaker_role %in% role_exclude))
  }

  speaker_statistics %<>%
    dplyr::mutate(target_child_age = target_child_age / avg_month)

  if (is.null(connection)) {
    suppressWarnings(speaker_statistics %<>% dplyr::collect())
    DBI::dbDisconnect(con)
  }

  return(speaker_statistics)
}

#' Get content
#'
#' @inheritParams get_participants
#' @param content_type One of "token" or "utterance"
#' @param token A character vector of one or more token patterns (`\%` matches
#'   any number of wildcard characters, `_` matches exactly one wildcard
#'   character)
#' @param stem A character vector of one or more stems
#' @param part_of_speech A character vector of one or more parts of speech
#' @param language A character vector of one or more languages
get_content <- function(content_type, collection = NULL, language = NULL,
                        corpus = NULL, role = NULL, role_exclude = NULL,
                        age = NULL, sex = NULL, target_child = NULL,
                        token = NULL, stem = NULL, part_of_speech = NULL,
                        connection) {

  transcripts <- get_transcripts(collection, corpus, target_child, connection)

  corpora <- transcripts %>%
    dplyr::distinct(corpus_id) %>%
    dplyr::collect()
  child_id <- transcripts %>%
    dplyr::distinct(target_child_id) %>%
    dplyr::pull(target_child_id)

  num_children <- length(child_id)
  num_corpora <- nrow(corpora)

  message("Getting data from ", num_children,
          ifelse(num_children == 1, " child", " children"), " in ",
          num_corpora, ifelse(num_corpora == 1, " corpus ", " corpora"), "...")

  content <- dplyr::tbl(connection, content_type)

  if (content_type %in% c("token", "token_frequency") & !is.null(token) &
      !identical("*", token)) {
    token_filter <- sprintf("gloss %%like%% '%s'", token) %>%
      paste(collapse = " | ")
    content %<>% dplyr::filter_(token_filter)
  }

  if (!is.null(stem)) {
    stem_filter <- stem
    content %<>% dplyr::filter(stem %in% stem_filter)
  }

  if (!is.null(part_of_speech)) {
    part_of_speech_filter <- part_of_speech
    content %<>% dplyr::filter(part_of_speech %in% part_of_speech_filter)
  }

  if (!num_corpora) {
    corpus_filter <- -1
    child_id <- -1
  } else {
    corpus_filter <- corpora$corpus_id
  }

  if (!is.null(collection) | !is.null(corpus)) {
    content %<>% dplyr::filter(corpus_id %in% corpus_filter)
  }

  if (!is.null(target_child)) {
    content %<>% dplyr::filter(target_child_id %in% child_id)
  }

  if (!is.null(age)) {
    if (!(length(age) %in% 1:2)) stop("`age` argument must be of length 1 or 2")
    days <- age * avg_month
    if (length(age) == 1) days <- c(days, days + avg_month)
    content %<>% dplyr::filter(target_child_age >= days[1],
                               target_child_age <= days[2])
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    content %<>% dplyr::filter(sex %in% sex_filter)
  }

  if (!is.null(role)) {
    content %<>% dplyr::filter(speaker_role %in% role)
  }

  if (!is.null(role_exclude)) {
    content %<>% dplyr::filter(!(speaker_role %in% role_exclude))
  }

  if (!is.null(language)) {
    language_filter <- language
    content %<>% dplyr::filter(language %in% language_filter)
  }

  content %<>%
    dplyr::mutate(target_child_age = target_child_age / avg_month)

  return(content)
}


#' Get tokens
#'
#' @inheritParams connect_to_childes
#' @inheritParams get_content
#' @param replace A boolean indicating whether to replace "gloss" with
#'   "replacement" (i.e. phonologically assimilated form), when available (defaults to \code{TRUE})
#'
#' @return A `tbl` of Token data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
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

  con <- resolve_connection(connection, db_version, db_args)
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
                        connection = con)

  if (replace) {
    tokens %<>%
      dplyr::mutate(gloss = if (replacement == "") gloss else replacement) %>%
      dplyr::select(-replacement)
  }

  if (is.null(connection)) {
    tokens %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(tokens)
}


#' Get types
#'
#' @inheritParams connect_to_childes
#' @inheritParams get_content
#' @param type A character vector of one or more type patterns (`%` matches any
#'   number of wildcard characters, `_` matches exactly one wildcard character)
#'
#' @return A `tbl` of Type data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_types()
#' }
get_types <- function(collection = NULL, language = NULL, corpus = NULL,
                      role = NULL, role_exclude = NULL, age = NULL, sex = NULL,
                      target_child = NULL, type = NULL, connection = NULL,
                      db_version = "current", db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
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
                       connection = con)

  if (is.null(connection)) {
    types %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(types)
}

#' Get utterances
#'
#' @inheritParams get_participants
#' @param language A character vector of one or more languages
#'
#' @return A `tbl` of Utterance data, filtered down by supplied arguments. If
#'   `connection` is supplied, the result remains a remote query, otherwise it
#'   is retrieved into a local tibble.
#' @export
#'
#' @examples
#' \donttest{
#' get_utterances(target_child = "Shem")
#' }
get_utterances <- function(collection = NULL, language = NULL, corpus = NULL,
                           role = NULL, role_exclude = NULL, age = NULL,
                           sex = NULL, target_child = NULL, connection = NULL,
                           db_version = "current", db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  utterances <- get_content(content_type = "utterance",
                            collection = collection,
                            language = language,
                            corpus = corpus,
                            role = role,
                            role_exclude = role_exclude,
                            age = age,
                            sex = sex,
                            target_child = target_child,
                            connection = con)

  if (is.null(connection)) {
    utterances %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(utterances)
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
#' @return A 'tbl' of Utterance data, filtered down by supplied arguments.
#' @export
#'
#' @examples
#' \donttest{
#' get_contexts(target_child = "Shem", token = "dog")
#' }
get_contexts <- function(collection = NULL, language = NULL, corpus = NULL,
                        role = NULL, role_exclude = NULL, age = NULL,
                        sex = NULL, target_child = NULL, token,
                        window = c(0, 0), remove_duplicates = TRUE,
                        connection = NULL, db_version = "current",
                        db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)

  token_utterances <- get_tokens(collection = collection,
                                 language = language,
                                 corpus = corpus,
                                 role = role,
                                 role_exclude = role_exclude,
                                 age = age,
                                 sex = sex,
                                 target_child = target_child,
                                 token = token,
                                 connection = con) %>%
    dplyr::pull(utterance_id)

  suppressMessages(
    utterances <- get_utterances(collection = collection,
                                 language = language,
                                 corpus = corpus,
                                 role = role,
                                 role_exclude = role_exclude,
                                 age = age,
                                 sex = sex,
                                 target_child = target_child,
                                 connection = con) %>%
      dplyr::rename(utterance_id = id)
  )

  utterance_orders <- utterances %>%
    dplyr::filter(utterance_id %in% token_utterances) %>%
    dplyr::select(transcript_id, utterance_order) %>%
    dplyr::collect()

  contexts <- purrr::map2_df(utterance_orders$transcript_id,
                             utterance_orders$utterance_order,
                             function(tid, index) {
    utterances %>%
      dplyr::filter(transcript_id == tid,
                    utterance_order >= (index - window[1]),
                    utterance_order <= (index + window[2])) %>%
      dplyr::collect()
  })

  if (remove_duplicates) {
    contexts %<>% dplyr::distinct(transcript_id, utterance_id, .keep_all = TRUE)
  }

  if (is.null(connection)) {
    DBI::dbDisconnect(con)
  }
  return(contexts)
}

#' Get database version
#'
#' @inheritParams connect_to_childes
#' @inheritParams get_table
#'
#' @return The database version as a string
#' @export
#'
#' @examples
#' \donttest{
#' get_database_version()
#' }
get_database_version <- function(connection = NULL, db_version = "current",
                                 db_args = NULL) {

  con <- resolve_connection(connection, db_version, db_args)
  admin <- dplyr::tbl(con, "admin") %>% dplyr::collect()

  if (is.null(connection)) {
    DBI::dbDisconnect(con)
  }

  return(admin$version[1])
}
