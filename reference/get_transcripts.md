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

  Deprecated, ignored (childesr now reads from the childes-db dataset on
  Redivis)

- db_version:

  String of the name of database version to use

- db_args:

  Deprecated, ignored

## Value

A \`tbl\` of Transcript data, filtered down by supplied arguments

## Identifiers

Numeric ids in childes-db (\`transcript_id\`, \`utterance_id\`, token
\`id\`, and so on) are internal to a database release: they are not
stable across versions of childes-db and should never be used to link
data across releases. The TalkBank persistent identifier (the \`pid\`
column returned by \`get_transcripts()\`) is the stable,
externally-facing identifier for a transcript; use it to match
transcripts across database versions or with other TalkBank tools. For
reproducible analyses, pin the database version with the \`db_version\`
argument.

## Examples

``` r
if (FALSE) { # \dontrun{
get_transcripts()
} # }
```
