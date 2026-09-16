# Get corpora

Get corpora

## Usage

``` r
get_corpora(connection = NULL, db_version = "current", db_args = NULL)
```

## Arguments

- connection:

  Deprecated, ignored (childesr now reads from the childes-db dataset on
  Redivis)

- db_version:

  String of the name of database version to use

- db_args:

  Deprecated, ignored

## Value

A \`tbl\` of Corpus data

## Examples

``` r
if (FALSE) { # \dontrun{
get_corpora()
} # }
```
