# childesr 0.3.0

Submitted 2026-09-15.

## Release summary

This release migrates the package's backend from a self-hosted MySQL server
(now sunset) to the versioned childes-db dataset hosted on Redivis. The
user-facing API (function names, arguments, and returned data) is unchanged
and verified against golden outputs of the previous version by a
characterization test suite. `connect_to_childes()`, `clear_connections()`,
and the `connection`/`db_args` arguments are soft-deprecated (they warn and
are ignored). Dependencies on DBI, dbplyr, and RMySQL are dropped.

## Maintainer change

The package maintainer has changed from Mika Braginsky
(mika.br@gmail.com) to Michael C. Frank (mcfrank@stanford.edu), with the
previous maintainer's agreement. This produces the expected
"New maintainer" NOTE in the CRAN incoming feasibility check.

## Internet resources (CRAN policy)

The package accesses a remote data service and is engineered so that CRAN
checks involve no network access:

- All functions fail gracefully when the resource is unavailable: network
  errors are retried with backoff, then produce an informative message and
  a NULL return, never an error or warning. This behavior is covered by
  hermetic (mocked, network-free) tests that do run on CRAN, and was
  additionally verified by rendering the vignette with invalid
  credentials (every chunk completes; all getters return NULL).
- All examples are wrapped in \dontrun{} because they query a remote
  database (potentially long-running downloads).
- Vignette chunks that access the database are only evaluated when
  NOT_CRAN=true.
- All networked tests are gated behind skip_on_cran() and a credential
  check.

## Suggested package not on CRAN

The `redivis` client (the interface to the data host) is in Suggests with
`requireNamespace()` guards at every use, and `Additional_repositories:`
points to https://langcog.r-universe.dev, which serves it (the incoming
feasibility check reports "Availability using Additional_repositories
specification: redivis yes https://langcog.r-universe.dev"). Without it
installed, functions produce an informative message with installation
instructions and return NULL.

## Reverse dependencies

One reverse dependency on CRAN: childeswordfreq 0.2.0 (Imports). It calls
`get_tokens()`, `get_types()`, `get_utterances()` and `get_contexts()`
with the `db_version` argument and uses the `gloss` column; none of these
signatures or columns changed. It does not use the deprecated
`connection` argument or the `replacement` column that
`get_tokens(replace = TRUE)` now drops. (It was non-functional against
the retired MySQL server; this release restores it.)

## Test environments

- local macOS (aarch64), R 4.5: R CMD check --as-cran on the built tarball
  with no credentials and NOT_CRAN=false (simulating a CRAN machine),
  including the remote CRAN incoming feasibility check and the PDF manual
- GitHub Actions ubuntu-latest: CRAN simulation without credentials
  (NOT_CRAN=false) on every push, and the full network test suite with
  credentials nightly (green through 2026-09-15)

## R CMD check results

0 errors | 0 warnings | 2 notes

- "checking CRAN incoming feasibility ... NOTE": New maintainer (see
  above); "Suggests or Enhances not in mainstream repositories: redivis"
  with availability confirmed via Additional_repositories.
- "checking HTML version of manual ... NOTE": "Skipping checking HTML
  validation: 'tidy' doesn't look like recent enough HTML Tidy" -- an
  artifact of the local environment's outdated HTML Tidy, not a package
  issue.
