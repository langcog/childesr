#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom RJSONIO "fromJSON"
NULL

utils::globalVariables(c("collection_id", "collection_name", "corpus_id",
                         "corpus_name", "gloss", "id", "max_age", "min_age",
                         "name", "speaker_role", "target_child_id",
                         "target_child_name", "target_child_age", "%like%"))

translateVersion <- function(version, connectionDetails, db_info){
  if (connectionDetails$host == db_info$host){
    # using the childes-db hosted server 
    if (version == 'current'){

      # CURRENT VERSION
      db_to_use = db_info[['current']]
      print(paste0('Using current version, ',db_to_use))
      return(db_to_use)
    } else if (version %in% db_info[['supported']]){
      
      # SUPPORTED VERSION
      db_to_use = version
      print(paste0('Using supported version, ',db_to_use))
      return(db_to_use)
    } else if ((version %in% db_info[['historical']])  ){

      # HISTORICAL VERSION
      stop(paste0(version, ' is no longer hosted by childes-db.stanford.edu; either specify a more recent version or install MySQL Server locally and update connectionDetails'))
    } else {

      # NOT RECOGNIZED
      stop(paste0(version, ' not found. Specify one of: current, ', paste(db_info$supported, collapse=', ')))
    }
  } else {
    # using a different server than the childes-db hosted one
    print(paste0('Not using hosted version; no checks will be applied to version specification'))
  }   
}

getExistingOrMakeNewConnection <- function(connection, version, connectionDetails){
  if (is.null(connection)){
    con <- connect_to_childes(version, connectionDetails)
  }  else {
    # print version even when we aren't connecting to a new database
    con <- connection
    print(paste0('Using version: ', version))
  } 
  return(con)
}  

#' Connect to CHILDES
#'
#' @param version String name of the version to use
#' @param version connectionDetails NULL or list with host, user, and password defined
#' @return A DBIConnection object for the CHILDES database
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_childes(version="current", connectionDetails=NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_childes <- function(version="current", connectionDetails=NULL) {  

  db_info = fromJSON("https://childes-db.stanford.edu/childes-db.json")
  
  if (is.null(connectionDetails)){
    connectionDetails = db_info
  } 

  DBI::dbConnect(RMySQL::MySQL(),
                 host = connectionDetails$host,
                 dbname = translateVersion(version, connectionDetails, db_info),
                 user = connectionDetails$user,
                 password = connectionDetails$password)
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
#' @param version String with the version name (used as the database name)
#' @param connectionDetails List with with host, user, and password
#'
#' @return A `tbl` of Collection data. If `connection` is supplied, the result
#'   is collected locally, otherwise it remains remote.
#' @export
#'
#' @examples
#' \dontrun{
#' get_collections()
#' }
get_collections <- function(connection = NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
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
get_corpora <- function(connection = NULL, version='current', connectionDetails=NULL ) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
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
                            connection = NULL, version='current' , connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)

  transcripts <- get_table(con, "transcript") %>%
    dplyr::rename(transcript_id = id)

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
#' @param role A character vector of one or more roles to include
#' @param role_exclude A character vector of one or more roles to exclude
#' @param age A numeric vector of an age or a min age (inclusive) and max age
#'   (exclusive) in months
#' @param sex A character vector of values "male" and/or "female"
#'
#' @return A `tbl` of Participant data, filtered down by collection, corpus,
#'   child, role, role_exclude, age, and sex supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_participants()
#' }
get_participants <- function(collection = NULL, corpus = NULL, child = NULL,
                             role = NULL, role_exclude = NULL, age = NULL,
                             sex = NULL, connection = NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  participants <- get_table(con, "participant")

  if (!is.null(collection)) {
    participants %<>% dplyr::filter(collection_name %in% collection)
  }

  if (!is.null(corpus)) {
    participants %<>% dplyr::filter(corpus_name %in% corpus)
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

  if (!is.null(role)) {
    role_filter <- role
    participants %<>% dplyr::filter(role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    participants %<>% dplyr::filter(!(role %in% role_exclude))
  }

  target_children <- get_transcripts(collection, corpus, child, con) %>%
    dplyr::distinct(target_child_id, target_child_name) %>%
    dplyr::select(target_child_id, target_child_name)

  # TODO remove after https://github.com/langcog/childes-db/issues/30 is resolved
  participants %<>%
    dplyr::left_join(target_children, by = "target_child_id")

  if (is.null(connection)) {
    participants %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }

  return(participants)

}
#' Get speaker statistics
#'
#' @inheritParams get_participants
#' @return A `tbl` of Speaker statistics, filtered down by collection, corpus,
#'   child, role, role_exclude, age, and sex supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_speaker_statistics()
#' }
get_speaker_statistics <- function(collection = NULL, corpus = NULL, child = NULL,
                                   role = NULL, role_exclude = NULL, age = NULL,
                                   sex = NULL, connection = NULL, version='current', connectionDetails = NULL) {

  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  transcripts <- get_transcripts(collection, corpus, child, connection)

  suppressWarnings(speaker_statistics <- get_table(con, "transcript_by_speaker"))

  if (!is.null(collection)) {
    collection_filter <- transcripts %>%
      dplyr::distinct(collection_id, target_child_id) %>%
      dplyr::pull(target_child_id)

    speaker_statistics %<>% dplyr::filter(target_child_id %in% collection_filter)
  }

  if (!is.null(corpus)) {
    corpus_filter <- transcripts %>%
      dplyr::distinct(corpus_id, target_child_id) %>%
      dplyr::pull(target_child_id)

    speaker_statistics %<>% dplyr::filter(target_child_id %in% corpus_filter)
  }

  if (!is.null(age)) {
    days <- age * 30.5
    if (length(age) == 1) {
      speaker_statistics %<>% dplyr::filter(max_age >= days & min_age <= days)
    } else if (length(age) == 2) {
      speaker_statistics %<>% dplyr::filter(max_age >= days[1] | min_age <= days[2])
    } else {
      stop("`age` argument must be of length 1 or 2")
    }
  }

  if (!is.null(sex)) {
    sex_filter <- sex
    speaker_statistics %<>% dplyr::filter(sex %in% sex_filter)
  }

  if (!is.null(child)) {
    child_filter <- child
    speaker_statistics %<>% dplyr::filter(target_child_name %in% child)
  }

  if (!is.null(role)) {
    role_filter <- role
    speaker_statistics %<>% dplyr::filter(speaker_role %in% role_filter)
  }

  if (!is.null(role_exclude)) {
    speaker_statistics %<>% dplyr::filter(!(speaker_role %in% role_exclude))
  }

  if (is.null(connection)) {
    suppressWarnings(speaker_statistics %<>% dplyr::collect())
    DBI::dbDisconnect(con)
  }

  return(speaker_statistics)
}

#' Get content
#'
#' @param content_type One of "token" or "utterance"
#' @param token A character vector of one or more token patterns (`\%` matches
#'   any number of wildcard characters, `_` matches exactly one wildcard
#'   character)
#' @param language A character vector of one or more languages
#' @inheritParams get_participants
get_content <- function(content_type, collection = NULL, language = NULL, corpus = NULL,
                        role = NULL, role_exclude = NULL, age = NULL,
                        sex = NULL, child = NULL, token = NULL, connection =NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  transcripts <- get_transcripts(collection, corpus, child, connection)
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

  if (!num_corpora) {
    corpus_filter <- -1
    child_id <- -1
  } else {
    corpus_filter <- corpora$corpus_id
  }

  if (!is.null(collection) | !is.null(corpus)) {
    content %<>% dplyr::filter(corpus_id %in% corpus_filter)
  }

  if (!is.null(child)) {
    content %<>% dplyr::filter(target_child_id %in% child_id)
  }

  if (!is.null(age)) {
    if (!(length(age) %in% 1:2)) {
      stop("`age` argument must be of length 1 or 2")
    }

    days <- age * 30.5
    if (length(age) == 1) days <- c(days, days + 30.49)

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

  return(content)
}


#' Get tokens
#'
#' @inheritParams get_content
#'
#' @return A `tbl` of Token data, filtered down by collection, corpus, child,
#'   role, age, sex, and token supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_tokens(token = "dog")
#' }
get_tokens <- function(collection = NULL, language = NULL, corpus = NULL, child = NULL,
                       role = NULL, role_exclude = NULL, age = NULL, sex = NULL,
                       token, connection = NULL, version='current', connectionDetails=NULL) {

  if(missing(token))
    stop("Argument \"token\" is missing. To fetch all tokens, supply \"*\"
         for argument \"token\". Caution: this may result in a long-running query.")

  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  tokens <- get_content(content_type = "token",
                        collection = collection,
                        language = language,
                        corpus = corpus,
                        role = role,
                        role_exclude = role_exclude,
                        age = age,
                        sex = sex,
                        child = child,
                        token = token,
                        connection = con)

  if (is.null(connection)) {
    tokens %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(tokens)
}


#' Get types
#'
#' @inheritParams get_content
#' @param type A character vector of one or more type patterns (`%` matches
#'   any number of wildcard characters, `_` matches exactly one wildcard
#'   character)
#'
#' @return A `tbl` of Type data, filtered down by collection, corpus, child,
#'   role, age, sex, and token supplied, if any.
#' @export
#'
#' @examples
#' \dontrun{
#' get_types()
#' }
get_types <- function(collection = NULL, language = NULL, corpus = NULL, child = NULL,
                       role = NULL, role_exclude = NULL, age = NULL, sex = NULL,
                       type = NULL, connection = NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  types <- get_content(content_type = "token_frequency",
                        collection = collection,
                        language = language,
                        corpus = corpus,
                        role = role,
                        role_exclude = role_exclude,
                        age = age,
                        sex = sex,
                        child = child,
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
#' @param language A character vector of one or more languages
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
get_utterances <- function(collection = NULL, language = NULL, corpus = NULL, role = NULL,
                           role_exclude = NULL, age = NULL, sex = NULL,
                           child = NULL, connection = NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  utterances <- get_content(content_type = "utterance",
                            collection = collection,
                            language = language,
                            corpus = corpus,
                            role = role,
                            role_exclude = role_exclude,
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

#' Get database version
#'
#' @param connection A connection to the CHILDES database
#' @return The database version as a string
#' @export
#'
#' @examples
#' \dontrun{
#' get_database_version()
#' }
get_database_version <- function(connection = NULL, version='current', connectionDetails=NULL) {
  con = getExistingOrMakeNewConnection(connection, version, connectionDetails)
  admin <- dplyr::tbl(con, "admin")

  if (is.null(connection)) {
    admin %<>% dplyr::collect()
    DBI::dbDisconnect(con)
  }
  return(admin$version[1])
}
