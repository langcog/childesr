# Run a SQL Query script on the CHILDES database

Run a SQL Query script on the CHILDES database

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

  A valid sql query string character

- connection:

  A connection to the CHILDES database

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

The database after calling the supplied SQL query

## Examples

``` r
if (FALSE) { # \dontrun{
get_sql_query("SELECT * FROM collection")
} # }
```
