# Get corpora

Get corpora

## Usage

``` r
get_corpora(connection = NULL, db_version = "current", db_args = NULL)
```

## Arguments

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

A \`tbl\` of Corpus data. If \`connection\` is supplied, the result
remains a remote query, otherwise it is retrieved into a local tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
get_corpora()
} # }
```
