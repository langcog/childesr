# Run a SQL Query script on the CHILDES database

As of childesr 0.3, queries run against the childes-db dataset on
Redivis, whose query engine uses BigQuery Standard SQL rather than MySQL
SQL. Standard SQL queries against the childes-db tables (\`collection\`,
\`corpus\`, \`transcript\`, \`participant\`, \`transcript_by_speaker\`,
\`utterance\`, \`token\`, \`token_frequency\`) work unchanged; queries
using MySQL-specific syntax may need to be updated (see
<https://cloud.google.com/bigquery/docs/reference/standard-sql/>).

## Usage

``` r
get_sql_query(
  sql_query_string,
  connection = NULL,
  db_version = "current",
  db_args = NULL
)
```

## Arguments

- sql_query_string:

  A valid BigQuery Standard SQL query string

- connection:

  Deprecated, ignored (childesr now reads from the childes-db dataset on
  Redivis)

- db_version:

  String of the name of database version to use

- db_args:

  Deprecated, ignored

## Value

The result of running the supplied SQL query on the childes-db dataset

## Examples

``` r
if (FALSE) { # \dontrun{
get_sql_query("SELECT * FROM collection")
} # }
```
