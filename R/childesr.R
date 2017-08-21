#' @importFrom magrittr "%>%"
NULL

#' @importFrom magrittr "%<>%"
NULL

connect_to_childes <- function() {
  DBI::dbConnect(RMySQL::MySQL(),
                 host = "ec2-54-68-171-132.us-west-2.compute.amazonaws.com",
                 dbname = "childesdb",
                 user = "childesdb",
                 password = "uy5z4hf7ihBjf")
}

get_table <- function(con, name) {
  dplyr::tbl(con, name)
}

get_collections <- function() {
  con <- connect_to_childes()

  collections <- dplyr::tbl(con, "collection") %>%
    dplyr::rename(collection_id = id,
                  collection_name = name)

  DBI::dbDisconnect(con)
  return(collections)
}

get_corpora <- function() {
  con <- connect_to_childes()

  corpora <- dplyr::tbl(con, "corpus") %>%
    dplyr::rename(corpus_id = id,
                  corpus_name = name)

  DBI::dbDisconnect(con)

  return(corpora)
}

get_transcripts <- function(collection = NULL, corpus = NULL, child = NULL) {
  con <- connect_to_childes()

  transcripts <- get_table(con, "transcript") %>%
    dplyr::left_join(get_corpora(), by = "corpus_id") %>%
    dplyr::left_join(get_collections(), by = "collection_id")

  if (!is.null(collection)) {
    transcripts %<>% dplyr::filter(collection_name %in% collection)
  }
  if (!is.null(corpus)) {
    transcripts %<>% dplyr::filter(corpus_name %in% corpus)
  }
  if (!is.null(child)) {
    transcripts %<>% dplyr::filter(target_child_name %in% child)
  }

  try(DBI::dbDisconnect(con), silent = TRUE)

  return(transcripts)

}
