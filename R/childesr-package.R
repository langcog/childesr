#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom magrittr "%<>%"
## usethis namespace: end
NULL

# TRUE if a redivis credential-cache dir looks baked in from a foreign build
# machine: a non-NULL path outside the current HOME's ~/.redivis whose parent
# directory does not exist. (r-universe binary macOS builds of redivis made
# before 2026-07-31 evaluated their auth paths at build time, freezing the CI
# runner's HOME -- e.g. /Users/runner/.redivis -- so OAuth tokens could never
# be cached and every request re-prompted for authentication. Fixed upstream
# on 2026-07-31; current redivis versions compute auth paths via accessors
# and no longer expose auth_vars, in which case this check stays silent.)
is_foreign_baked_dir <- function(dir, home = Sys.getenv("HOME")) {
  is.character(dir) && length(dir) == 1 && !is.na(dir) &&
    !identical(normalizePath(dir, mustWork = FALSE),
               normalizePath(file.path(home, ".redivis"), mustWork = FALSE)) &&
    !dir.exists(dirname(dir))
}

# returns a warning message if the installed redivis looks like a stale
# broken binary, NULL otherwise; any error (redivis not installed, or a
# fixed/refactored redivis without auth_vars) degrades to silent NULL.
# `ns` is injectable for testing.
redivis_binary_warning <- function(ns = NULL) {
  tryCatch({
    if (is.null(ns)) {
      if (!requireNamespace("redivis", quietly = TRUE)) return(NULL)
      ns <- asNamespace("redivis")
    }
    auth_vars <- get("auth_vars", envir = ns, inherits = FALSE)
    if (is_foreign_baked_dir(auth_vars$redivis_dir)) {
      paste0(
        "The installed `redivis` package appears to be a stale binary ",
        "built on another machine: its credential cache path (",
        auth_vars$redivis_dir, ") points outside your home directory, so ",
        "OAuth tokens cannot be saved and every request may re-prompt for ",
        "browser authentication (bug in binaries built before 2026-07-31, ",
        "since fixed upstream). Either update the redivis package:\n",
        '  install.packages("redivis", repos = "https://langcog.r-universe.dev")\n',
        "or authenticate with an API token (created under your workspace ",
        "settings at redivis.com), which bypasses the OAuth cache:\n",
        '  Sys.setenv(REDIVIS_API_TOKEN = "...")')
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# warn (never error) at attach time if the installed redivis client is a
# stale broken binary
.onAttach <- function(lib, pkg) {
  msg <- redivis_binary_warning()
  if (!is.null(msg)) packageStartupMessage(msg)
  invisible(NULL)
}
