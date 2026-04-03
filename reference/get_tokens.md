# Get tokens

Get tokens

## Usage

``` r
get_tokens(
  collection = NULL,
  language = NULL,
  corpus = NULL,
  target_child = NULL,
  role = NULL,
  role_exclude = NULL,
  age = NULL,
  sex = NULL,
  token,
  stem = NULL,
  part_of_speech = NULL,
  replace = TRUE,
  connection = NULL,
  db_version = "current",
  db_args = NULL
)
```

## Arguments

- collection:

  A character vector of one or more names of collections

- language:

  A character vector of one or more languages

- corpus:

  A character vector of one or more names of corpora

- target_child:

  A character vector of one or more names of children

- role:

  A character vector of one or more roles to include

- role_exclude:

  A character vector of one or more roles to exclude

- age:

  A numeric vector of an single age value or a min age value and max age
  value (inclusive) in months. For a single age value, participants are
  returned for which that age is within their age range; for two ages,
  participants are returned for whose age overlaps with the interval
  between those two ages.

- sex:

  A character vector of values "male" and/or "female"

- token:

  A character vector of one or more token patterns (\`%\` matches any
  number of wildcard characters, \`\_\` matches exactly one wildcard
  character)

- stem:

  A character vector of one or more stems

- part_of_speech:

  A character vector of one or more parts of speech

- replace:

  A boolean indicating whether to replace "gloss" with "replacement"
  (i.e. phonologically assimilated form), when available (defaults to
  `TRUE`)

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

A \`tbl\` of Token data, filtered down by supplied arguments. If
\`connection\` is supplied, the result remains a remote query, otherwise
it is retrieved into a local tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
get_tokens(token = "dog")
} # }
```
