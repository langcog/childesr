#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom magrittr "%<>%"
## usethis namespace: end
NULL

# TRUE if a redivis credential-cache dir looks baked in from a foreign build
# machine: a non-NULL path outside the current HOME's ~/.redivis whose parent
# directory does not exist. (The r-universe binary macOS builds of redivis
# evaluate their auth paths at build time, freezing the CI runner's HOME --
# e.g. /Users/runner/.redivis -- so OAuth tokens can never be cached and
# every request re-prompts for authentication.)
is_foreign_baked_dir <- function(dir, home = Sys.getenv("HOME")) {
  is.character(dir) && length(dir) == 1 && !is.na(dir) &&
    !identical(normalizePath(dir, mustWork = FALSE),
               normalizePath(file.path(home, ".redivis"), mustWork = FALSE)) &&
    !dir.exists(dirname(dir))
}

# warn (never error) at attach time if the installed redivis client is such a
# broken binary; tryCatch degrades to silence if redivis's internals change
.onAttach <- function(lib, pkg) {
  tryCatch({
    if (requireNamespace("redivis", quietly = TRUE)) {
      auth_vars <- get("auth_vars", envir = asNamespace("redivis"))
      if (is_foreign_baked_dir(auth_vars$redivis_dir)) {
        packageStartupMessage(
          "The installed `redivis` package appears to be a binary built on ",
          "another machine: its credential cache path (",
          auth_vars$redivis_dir, ") points outside your home directory, so ",
          "OAuth tokens cannot be saved and every request may re-prompt for ",
          "browser authentication. Either reinstall redivis from source:\n",
          '  install.packages("redivis", repos = "https://langcog.r-universe.dev", type = "source")\n',
          "or authenticate with an API token (created under your workspace ",
          "settings at redivis.com), which bypasses the OAuth cache:\n",
          '  Sys.setenv(REDIVIS_API_TOKEN = "...")')
      }
    }
  }, error = function(e) invisible(NULL))
  invisible(NULL)
}
