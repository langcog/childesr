# Get types

Get types

## Usage

``` r
get_types(
  collection = NULL,
  language = NULL,
  corpus = NULL,
  role = NULL,
  role_exclude = NULL,
  age = NULL,
  sex = NULL,
  target_child = NULL,
  type = NULL,
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

- target_child:

  A character vector of one or more names of children

- type:

  A character vector of one or more type patterns (\` number of wildcard
  characters, \`\_\` matches exactly one wildcard character)

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

A \`tbl\` of Type data, filtered down by supplied arguments. If
\`connection\` is supplied, the result remains a remote query, otherwise
it is retrieved into a local tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
get_types()
} # }
```
