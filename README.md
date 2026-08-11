<!-- badges: start -->
[![R-CMD-check](https://github.com/langcog/childesr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/langcog/childesr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# An R interface to childes-db

The `childesr` package allows you to access data in the childes-db from R. This removes the need to write complex SQL queries in order to get the information you want from the database. This vignette shows some examples of how to use the data loading functions and what the resulting data look like.

### Install `childesr` from this GitHub repository:

```
# install.packages("devtools")
devtools::install_github("langcog/childesr")
```

`childesr` reads data from the childes-db dataset on Redivis via the
[`redivis`](https://apidocs.redivis.com/client-libraries/redivis-r) R client,
which is not on CRAN. Install it with:

```
install.packages("redivis", repos = "https://langcog.r-universe.dev")
```

(A bug in redivis macOS binaries built before 2026-07-31 broke OAuth token
caching, causing repeated browser authentication prompts; current r-universe
binaries are fine. If you see repeated auth prompts, update the redivis
package with the command above.) For headless or scripted use, you can
instead authenticate with an API token, which bypasses the OAuth cache
entirely: create a token under your workspace settings at
[redivis.com](https://redivis.com) and set
`Sys.setenv(REDIVIS_API_TOKEN = "...")` before use.

### Tutorial

To get a hands on walk-through on how to use `childesr` to access `childes-db`, check out this [tutorial](https://langcog.github.io/childes-db-website/api.html).

### Other relevant GitHub repositories

- Website frontend: http://github.com/langcog/childes-db-website
- Interactive data visualizations: https://github.com/langcog/childes-db-shiny
