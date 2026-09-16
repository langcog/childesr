# Get information on database connection options

Retrieves hosted information about available versions of childes-db and
their corresponding versions of the childes-db dataset on Redivis. If
the hosted information cannot be retrieved or does not specify the
Redivis version mapping, a mapping built into the package is used
instead.

## Usage

``` r
get_db_info()
```

## Value

List of database info, including \`redivis_current\` (the childes-db
version that is the current Redivis release) and \`redivis_versions\` (a
named vector mapping childes-db versions to Redivis dataset versions).

## Examples

``` r
if (FALSE) { # \dontrun{
get_db_info()
} # }
```
