# Connect to CHILDES

As of childesr 0.3, data are retrieved from the versioned childes-db
dataset on Redivis rather than a MySQL database, so no connection object
is needed. \`connect_to_childes()\` is deprecated: it warns and returns
\`NULL\`, which can still be passed as the \`connection\` argument of
the \`get\_\` functions (where it is ignored).

## Usage

``` r
connect_to_childes(db_version = "current", db_args = NULL)
```

## Arguments

- db_version:

  String of the name of database version to use

- db_args:

  Deprecated, ignored

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_childes(db_version = "current")
} # }
```
