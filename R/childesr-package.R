#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom magrittr "%<>%"
## usethis namespace: end
NULL

# enable rlang::on_load
.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

# config file path
cnf_path <- NULL
# on package load, update config file path to package install location
rlang::on_load({
  cnf_path <- system.file("childesr.cnf", package = "childesr")
})
