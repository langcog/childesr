#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
NULL

utils::globalVariables(c("collection_id", "collection_name", "corpus_id",
                         "corpus_name", "gloss", "id", "max_age", "min_age",
                         "name", "speaker_role", "target_child_id",
                         "target_child_name"))

#' Connect to CHILDES
#'
#' @return A DBIConnection object for the CHILDES database
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_childes()
#' DBI::dbDisconnect(con)
#' }
connect_to_childes <- function() {
  DBI::dbConnect(RMySQL::MySQL(),
                 host = "ec2-54-68-171-132.us-west-2.compute.amazonaws.com",
                 dbname = "childesdb",
                 user = "childesdb",
                 password = "uy5z4hf7ihBjf")
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
#' @param connection A connection to the CHILDES database
#'
#' @return A `tbl` of Collection data. If `connection` is supplied, the result
#'   is collected locally, otherwise it remains remote.
#' @export
#'
#' @examples
#' \dontrun{
#' get_collections()
#' }
get_collections <- function(connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

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
#' @return A `tbl` of Corpus data. If `connection` is supplied, the result is
#'   collected locally, otherwise it remains remote.
#' @export
#'
#' @examples
#' \dontrun{
#' get_corpora()
#' }
get_corpora <- function(connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

  corpora <- dplyr::tbl(con, "corpus") %>%
    dplyr::rename(corpus_id = id) %>%
    dplyr::rename(corpus_name = name) %>%
    dplyr::left_join(get_collections(con), by = "collection_id")

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
#' @param child A character vector of one or more names of children
#' @inheritParams get_collections
#'
#' @return A `tbl` of Transcript data, filtered down by collection, corpus, and
#'   child supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_transcripts()
#' }
get_transcripts <- function(collection = NULL, corpus = NULL, child = NULL,
                            connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

  transcripts <- get_table(con, "transcript") %>%
    dplyr::rename(transcript_id = id) %>%
    dplyr::left_join(get_corpora(con), by = "corpus_id")

  if (!is.null(collection)) {
    transcripts %<>% dplyr::filter(collection_name %in% collection)
  }
  if (!is.null(corpus)) {
    transcripts %<>% dplyr::filter(corpus_name %in% corpus)
  }
  if (!is.null(child)) {
    transcripts %<>% dplyr::filter(target_child_name %in% child)
  }

  if (is.null(connection)) {
    transcripts %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(transcripts)

}


#' Get participants
#'
#' @inheritParams get_transcripts
#' @param role A character vector of one or more roles
#' @param age A numeric vector of an age or a min age and max age (in months)
#' @param sex A character vector of values "male" and/or "female"
#'
#' @return A `tbl` of Participant data, filtered down by collection, corpus,
#'   child, role, age, and sex supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_participants()
#' }
get_participants <- function(collection = NULL, corpus = NULL, child = NULL,
                             role = NULL, age = NULL, sex = NULL,
                             connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

  participants <- get_table(con, "participant") %>%
    dplyr::left_join(get_corpora(con), by = "corpus_id")

  if (!is.null(collection)) {
    participants %<>% dplyr::filter(collection_name %in% collection)
  }

  if (!is.null(corpus)) {
    participants %<>% dplyr::filter(corpus_name %in% corpus)
  }

  if (!is.null(role)) {
    role_filter <- role
    participants %<>% dplyr::filter(role %in% role_filter)
  }

  if (!is.null(age)) {
    days <- age * 30.5
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

  if (!is.null(child)) {
    child_id <- participants %>%
      dplyr::filter(name == child) %>%
      dplyr::pull(target_child_id) %>%
      unique()
    if (length(child_id) != 1) {
      stop("Duplicate or missing child")
    } else {
      participants %<>% dplyr::filter(target_child_id == child_id)
    }
  }

  if (is.null(connection)) {
    participants %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(participants)

}

#' Get content
#'
#' @param content_type One of "token" or "utterance"
#' @inheritParams get_participants
get_content <- function(content_type, collection = NULL, corpus = NULL,
                        role = NULL, age = NULL, sex = NULL, child = NULL,
                        connection) {

  transcripts <- get_transcripts(collection, corpus, child, connection)
  corpora <- transcripts %>%
    dplyr::distinct(corpus_id) %>%
    dplyr::collect()
  child_id <- transcripts %>%
    dplyr::distinct(target_child_id) %>%
    dplyr::pull(target_child_id)

  content <- dplyr::tbl(connection, content_type)

  if (!is.null(corpus)) {
    content %<>% dplyr::filter(corpus_id %in% corpora$corpus_id)
  }

  if (!is.null(role)) {
    content %<>% dplyr::filter(speaker_role %in% role)
  }

  if (!is.null(age)) {
    days <- age * 30.5
    if (length(age) == 1) {
      content %<>% dplyr::filter(max_age >= days & min_age <= days)
    } else if (length(age) == 2) {
      content %<>% dplyr::filter(max_age >= days[1] | min_age <= days[2])
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    content %<>% dplyr::filter(sex %in% sex_filter)
  }

  if (!is.null(child)) {
    content %<>% dplyr::filter(target_child_id %in% child_id)
  }

  return(content)
}


#' Get tokens
#'
#' @inheritParams get_participants
#' @param token A character vector of tokens
#'
#' @return A `tbl` of Token data, filtered down by collection, corpus,
#'   child, role, age, sex, and token supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_tokens(token = "dog")
#' }
get_tokens <- function(collection = NULL, corpus = NULL, child = NULL,
                       role = NULL, age = NULL, sex = NULL, token = NULL,
                       connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

  tokens <- get_content(content_type = "token",
                        collection = collection,
                        corpus = corpus,
                        role = role,
                        age = age,
                        sex = sex,
                        child = child,
                        connection = con)

  if (!is.null(token)) {
    tokens %<>% dplyr::filter(gloss %in% token)
  }

  if (is.null(connection)) {
    tokens %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(tokens)
}

#' Get utterances
#'
#' @inheritParams get_participants
#'
#' @return A `tbl` of Utterance data, filtered down by collection, corpus,
#'   child, role, age, and sex supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_utterances(child = "Shem")
#' }
get_utterances <- function(collection = NULL, corpus = NULL, role = NULL,
                           age = NULL, sex = NULL, child = NULL,
                           connection = NULL) {
  if (is.null(connection)) con <- connect_to_childes() else con <- connection

  utterances <- get_content(content_type = "utterance",
                            collection = collection,
                            corpus = corpus,
                            role = role,
                            age = age,
                            sex = sex,
                            child = child,
                            connection = con)

  if (is.null(connection)) {
    utterances %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(utterances)
}
