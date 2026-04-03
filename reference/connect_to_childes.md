# Connect to CHILDES

Connect to CHILDES

## Usage

``` r
connect_to_childes(db_version = "current", db_args = NULL)
```

## Arguments

- db_version:

  String of the name of database version to use

- db_args:

  List with host, user, and password defined

## Value

con A DBIConnection object for the CHILDES database

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_childes(db_version = "current", db_args = NULL)
DBI::dbDisconnect(con)
} # }
```
