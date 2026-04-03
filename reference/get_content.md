# Get content

Get content

## Usage

``` r
get_content(
  content_type,
  collection = NULL,
  language = NULL,
  corpus = NULL,
  role = NULL,
  role_exclude = NULL,
  age = NULL,
  sex = NULL,
  target_child = NULL,
  token = NULL,
  stem = NULL,
  part_of_speech = NULL,
  connection
)
```

## Arguments

- content_type:

  One of "token" or "utterance"

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

- token:

  A character vector of one or more token patterns (\`%\` matches any
  number of wildcard characters, \`\_\` matches exactly one wildcard
  character)

- stem:

  A character vector of one or more stems

- part_of_speech:

  A character vector of one or more parts of speech

- connection:

  A connection to the CHILDES database
