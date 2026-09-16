# Clear all connections

As of childesr 0.3, data are retrieved from the versioned childes-db
dataset on Redivis rather than a MySQL database, so there are no
connections to clear. This function only clears the session's table
cache.

## Usage

``` r
clear_connections()
```
