# childesr 0.3.0

## Release summary

This release migrates the package's backend from a self-hosted MySQL server
(being sunset) to the versioned childes-db dataset hosted on Redivis. The
user-facing API (function names, arguments, and returned data) is unchanged
and verified against golden outputs of the previous version by a
characterization test suite. `connect_to_childes()`, `clear_connections()`,
and the `connection`/`db_args` arguments are soft-deprecated (they warn and
are ignored). Dependencies on DBI, dbplyr, and RMySQL are dropped.

## Internet resources (CRAN policy)

The package accesses a remote data service and is engineered so that CRAN
checks involve no network access:

- All functions fail gracefully when the resource is unavailable: network
  errors are retried with backoff, then produce an informative message and
  a NULL return, never an error or warning. This behavior is covered by
  hermetic (mocked, network-free) tests that do run on CRAN.
- All examples are wrapped in \dontrun{} because they query a remote
  database (potentially long-running downloads).
- Vignette chunks that access the database are only evaluated when
  NOT_CRAN=true.
- All networked tests are gated behind skip_on_cran() and a credential
  check.

## Suggested package not on CRAN

The `redivis` client (the interface to the data host) is in Suggests with
`requireNamespace()` guards at every use, and `Additional_repositories:`
points to https://langcog.r-universe.dev which serves it. Without it
installed, functions produce an informative message with installation
instructions and return NULL.

## Test environments

- local macOS install, R 4.5 (R CMD check --as-cran on the built tarball,
  with no credentials and NOT_CRAN unset, simulating a CRAN machine)
- GitHub Actions ubuntu-latest (CRAN simulation without credentials, and
  full network test suite with credentials)

## R CMD check results

0 errors | 0 warnings | 0 notes

(The local macOS run shows one NOTE, "Skipping checking HTML validation:
'tidy' doesn't look like recent enough HTML Tidy" -- an artifact of the
local environment's outdated HTML Tidy, not a package issue.)
