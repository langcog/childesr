# childesr reads data from the versioned childes-db dataset hosted on Redivis
# (https://redivis.com/datapages/datasets/childes_db). This file contains the
# machinery for resolving database versions to Redivis dataset versions and
# for running queries against them.

# session-level cache for small whole-table fetches
.childesr_env <- new.env(parent = emptyenv())

# qualified reference of the childes-db dataset on Redivis
childes_dataset_reference <- "childes_db:b6q6"
childes_organization <- "datapages"

# fallback mapping from childes-db versions to Redivis dataset versions, used
# when the hosted childes-db.json does not (yet) contain redivis_* fields
childes_redivis_fallback <- list(
  redivis_current = "2026.1",
  redivis_versions = c("2018.1" = "v1.0", "2019.1" = "v1.1",
                       "2020.1" = "v1.2", "2021.1" = "v1.3",
                       "2026.1" = "v1.4")
)

#' Get information on database connection options
#'
#' Retrieves hosted information about available versions of childes-db and
#' their corresponding versions of the childes-db dataset on Redivis. If the
#' hosted information cannot be retrieved or does not specify the Redivis
#' version mapping, a mapping built into the package is used instead.
#'
#' @return List of database info, including `redivis_current` (the childes-db
#'   version that is the current Redivis release) and `redivis_versions` (a
#'   named vector mapping childes-db versions to Redivis dataset versions).
#' @export
#'
#' @examples
#' \dontrun{
#' get_db_info()
#' }
get_db_info <- function() {
  info <- tryCatch(
    jsonlite::fromJSON("https://langcog.github.io/childes-db-website/childes-db.json"),
    error = function(e) {
      message(strwrap(
        prefix = " ", initial = "",
        "Could not retrieve hosted childes-db version information; using the
        version mapping built into childesr. If this message persists please
        check your internet connection or contact
        childes-db-contact@stanford.edu"
      ))
      NULL
    })
  if (is.null(info)) info <- list()
  if (is.null(info$redivis_current)) {
    info$redivis_current <- childes_redivis_fallback$redivis_current
  }
  if (is.null(info$redivis_versions)) {
    info$redivis_versions <- childes_redivis_fallback$redivis_versions
  } else {
    info$redivis_versions <- unlist(info$redivis_versions)
  }
  info
}

# resolve a db_version argument ("current", "2021.1", ...) to a childes-db
# version and its Redivis dataset version tag
resolve_version <- function(db_version = "current", db_args = NULL) {
  if (!is.null(db_args)) {
    warning("childesr now reads from the childes-db dataset on Redivis; ",
            "`db_args` is deprecated and ignored.", call. = FALSE)
  }

  info <- get_db_info()
  versions <- info$redivis_versions

  if (db_version == "current") {
    db_to_use <- info$redivis_current
    message("Using current database version: '", db_to_use, "'.")
  } else if (db_version %in% names(versions)) {
    db_to_use <- db_version
    message("Using supported database version: '", db_to_use, "'.")
  } else {
    stop("Version '", db_version, "' not found. Specify one of: 'current', ",
         paste(sprintf("'%s'", names(versions)), collapse = ", "), ".",
         call. = FALSE)
  }

  list(db_version = db_to_use, tag = unname(versions[[db_to_use]]))
}

# check that the redivis package is available; if not, message and FALSE
redivis_available <- function() {
  if (!requireNamespace("redivis", quietly = TRUE)) {
    message(
      "childesr needs the `redivis` package to access CHILDES data.\n",
      "Install it with:\n",
      '  install.packages("redivis", repos = c("https://langcog.r-universe.dev", "https://cloud.r-project.org"))')
    return(FALSE)
  }
  TRUE
}

# reference to the childes-db Redivis dataset at a given version tag
childes_dataset <- function(tag) {
  redivis::redivis$organization(childes_organization)$dataset(
    childes_dataset_reference, version = tag)
}

# CRAN policy requires graceful failure on unavailable internet resources:
# transient errors are retried with backoff, then produce a message and
# NULL -- never an error (the retry count is an option so tests can
# simulate an outage without waiting out the backoff)
childes_try <- function(expr,
                        tries = getOption("childesr.request_tries", 3)) {
  expr <- substitute(expr)
  env <- parent.frame()
  for (i in seq_len(tries)) {
    result <- tryCatch(eval(expr, env), error = function(e) {
      if (i < tries) {
        message("Redivis request failed (attempt ", i, "/", tries,
                "), retrying...")
        Sys.sleep(2 ^ i)
      } else {
        message("Could not retrieve data from Redivis. Please check your ",
                "internet connection. If this error persists please contact ",
                "childes-db-contact@stanford.edu.\n(", conditionMessage(e), ")")
      }
      NULL
    })
    if (!is.null(result)) return(result)
  }
  NULL
}

# muffle the redivis client's advisory warning about unqualified table
# references (table names are resolved within the pinned dataset version)
quiet_redivis <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("No reference id was provided", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# fetch a whole table as a tibble, cached per session + version
childes_table <- function(name, tag) {
  if (!redivis_available()) return(NULL)
  key <- paste(tag, name)
  if (is.null(.childesr_env[[key]])) {
    .childesr_env[[key]] <-
      childes_try(quiet_redivis(childes_dataset(tag)$table(name)$to_tibble()))
  }
  .childesr_env[[key]]
}

# run a SQL query against the dataset (server-side filtering for big tables)
childes_query <- function(sql, tag) {
  if (!redivis_available()) return(NULL)
  childes_try(quiet_redivis(childes_dataset(tag)$query(sql)$to_tibble()))
}

# escape a string for use as a BigQuery Standard SQL string literal
quote_sql <- function(x) {
  paste0("'", gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", x)), "'")
}

# warn (once per call) if a deprecated connection argument is supplied
check_connection <- function(connection) {
  if (!is.null(connection)) {
    warning("childesr now reads from the childes-db dataset on Redivis; ",
            "the `connection` argument is deprecated and ignored.",
            call. = FALSE)
  }
}

#' Connect to CHILDES
#'
#' As of childesr 0.3, data are retrieved
#' from the versioned childes-db dataset on Redivis rather than a MySQL
#' database, so no connection object is needed. `connect_to_childes()` is
#' deprecated: it warns and returns `NULL`, which can still be passed as
#' the `connection` argument of the `get_` functions (where it is ignored).
#'
#' @param db_version String of the name of database version to use
#' @param db_args Deprecated, ignored
#' @return NULL
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_childes(db_version = "current")
#' }
connect_to_childes <- function(db_version = "current", db_args = NULL) {
  .Deprecated(msg = paste(
    "connect_to_childes() is deprecated: as of childesr 0.3, data come from",
    "the childes-db dataset on Redivis and no connection object is needed;",
    "call the `get_` functions directly."))
  invisible(NULL)
}

#' Clear all connections
#'
#' As of childesr 0.3, data are retrieved from the versioned childes-db
#' dataset on Redivis rather than a MySQL database, so there are no
#' connections to clear. This function only clears the session's table cache.
#'
#' @keywords internal
#' @export
clear_connections <- function() {
  .Deprecated(msg = paste(
    "clear_connections() is deprecated: childesr now reads from Redivis and",
    "there are no database connections to clear (the session's table cache",
    "was cleared)."))
  rm(list = ls(.childesr_env), envir = .childesr_env)
  invisible(NULL)
}
