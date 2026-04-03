# Get transcripts

Get transcripts

## Usage

``` r
get_transcripts(
  collection = NULL,
  corpus = NULL,
  target_child = NULL,
  connection = NULL,
  db_version = "current",
  db_args = NULL
)
```

## Arguments

- collection:

  A character vector of one or more names of collections

- corpus:

  A character vector of one or more names of corpora

- target_child:

  A character vector of one or more names of children

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

A \`tbl\` of Transcript data, filtered down by supplied arguments. If
\`connection\` is supplied, the result remains a remote query, otherwise
it is retrieved into a local tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
get_transcripts()
} # }
```
