# supabaseR 0.1.0 (development)

## Breaking Changes

* Complete API redesign: all functions renamed from `get_*`/`put_*` to `sb_db_*` prefix
* Removed deprecated functions: `get_table_data`, `put_table_row`, `delete_table_row`, etc.
* Removed renv dependency management

## New Features
* `sb_db_connect()` / `sb_db_disconnect()`: connection management with cli feedback
* `sb_db_status()`: check DBI/API backend availability
* `sb_db_tables()`, `sb_db_table_exists()`, `sb_db_schema()`: table metadata
* `sb_db_read()`: read table data with optional limit
* `sb_db_query()`: query with `where` operators (gt, lt, like, in, etc.) or raw SQL
* `sb_db_insert()`, `sb_db_update()`, `sb_db_delete()`, `sb_db_truncate()`: CRUD operations
* `sb_db_upsert()`: insert or update with conflict handling
* Backend detection on package load with status display
* cli-powered success messages for all write operations

## Internal

* Use `glue::glue_sql()` for all SQL construction
* Add `build_where()` helper for operator support
* Add comprehensive test coverage with testthat 3.0 snapshots

# supabaseR 0.0.1

* Initial release with basic CRUD functions
* Environment variable configuration
