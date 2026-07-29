# childesr 0.3.0
- data are now retrieved from the versioned childes-db dataset on Redivis
  (https://redivis.com/datapages/datasets/childes_db) instead of a MySQL
  server; the `get_` function interface and outputs are unchanged
- filtering of the large content tables (tokens, utterances, types) happens
  server-side on Redivis, so only matching rows are transferred
- `connect_to_childes()`, `clear_connections()`, and the `connection` and
  `db_args` arguments are deprecated and ignored (no connection is needed)
- `get_sql_query()` now runs BigQuery Standard SQL (Redivis's query engine)
  instead of MySQL SQL
- `get_tokens(replace = TRUE)` now correctly drops the `replacement` column;
  previously a bug kept it
- dropped dependencies on DBI, dbplyr, and RMySQL; added a Suggests
  dependency on the redivis client (installable from
  https://langcog.r-universe.dev)
- the current database version is now childes-db 2026.1 (Redivis v1.4). It
  adds a `token_morpheme` table (accessible via `get_sql_query()`; dedicated
  getters may follow) and reworks morphological parsing, so morpheme-based
  measures (e.g. `mlu_m`, `num_morphemes`) are not comparable with earlier
  versions. As always, numeric ids are internal to a release -- pin
  `db_version` for reproducible analyses and use `pid` to link transcripts
  across versions

# childesr 0.2.3
- update url for database connection

# childesr 0.2.2
- add checks for database connection errors

# childesr 0.2.1
- fix age filtering in get_participants()
- add support for SQL Query strings
- update for compatibility with dplyr 2.0.0

# childesr 0.1.2
- enable connections to previous versions of childes-db
- add function for clearing all database connections
- don't run code in vignette if unable to connect to childes-db
