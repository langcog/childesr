# Get speaker statistics

Get speaker statistics

## Usage

``` r
get_speaker_statistics(
  collection = NULL,
  corpus = NULL,
  target_child = NULL,
  role = NULL,
  role_exclude = NULL,
  age = NULL,
  sex = NULL,
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

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

A \`tbl\` of Speaker statistics, filtered down by supplied arguments. If
\`connection\` is supplied, the result remains a remote query, otherwise
it is retrieved into a local tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
get_speaker_statistics()
} # }
```
