translate_version <- function(db_version, db_args, db_info) {

  # using the childes-db hosted server
  if (db_args$host == db_info$host) {

    # current version
    if (db_version == "current") {
      db_to_use <- db_info[["current"]]
      message("Using current database version: '", db_to_use, "'.")
      return(db_to_use)

      # supported version
    } else if (db_version %in% db_info[["supported"]]) {
      db_to_use <- db_version
      message("Using supported database version: '", db_to_use, "'.")
      return(db_to_use)

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

#' Get information on database connection options
#'
#' @return List of database info: host name, current version, supported
#'   versions, historical versions, username, password
#' @export
#'
#' @examples
#' \dontrun{
#' get_db_info()
#' }
get_db_info <- function() {
  tryCatch(jsonlite::fromJSON("https://langcog.github.io/childes-db-website/childes-db.json"),
           error = function(e) message(strwrap(
             prefix = " ", initial = "",
             "Could not retrieve childes-db connection information. Please check your
             internet connection. If this error persists please contact
             childes-db-contact@stanford.edu"
           )))
}

#' Connect to CHILDES
#'
#' @param db_version String of the name of database version to use
#' @param db_args List with host, user, and password defined
#' @return con A DBIConnection object for the CHILDES database
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_childes(db_version = "current", db_args = NULL)
#' DBI::dbDisconnect(con)
#' }
connect_to_childes <- function(db_version = "current", db_args = NULL) {

  # get info from hosted json, NULL if connection error
  db_info <- get_db_info()

  # if db_info was not found, return NULL
  if (is.null(db_info)) return()

  # if db_args is unspecified, use db_info
  if (is.null(db_args)) db_args <- db_info

  tryCatch(
    expr = {
      con <- DBI::dbConnect(
        RMySQL::MySQL(),
        host = db_args$host,
        dbname = translate_version(db_version, db_args, db_info),
        user = db_args$user,
        password = db_args$password
      )
      DBI::dbGetQuery(con, "SET NAMES utf8")
      return(con)
    },
    error = function(e) message("Could not connect to childes-db"))
}

#' Clear all MySQL connections
#'
#' @export
clear_connections <- function() {
  cons <- DBI::dbListConnections(RMySQL::MySQL())
  purrr::walk(cons, DBI::dbDisconnect)
  message(sprintf("Cleared %s connections", length(cons)))
}
