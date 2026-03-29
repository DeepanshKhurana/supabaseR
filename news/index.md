# Changelog

## supabaseR 0.1.0 (development)

### Breaking Changes

- Complete API redesign: all functions renamed from `get_*`/`put_*` to
  `sb_db_*` prefix
- Removed deprecated functions: `get_table_data`, `put_table_row`,
  `delete_table_row`, etc.
- Removed renv dependency management

### New Features

- [`sb_db_connect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_connect.md)
  /
  [`sb_db_disconnect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_disconnect.md):
  connection management with cli feedback
- [`sb_db_status()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_status.md):
  check DBI/API backend availability
- [`sb_db_tables()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_tables.md),
  [`sb_db_table_exists()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_table_exists.md),
  [`sb_db_schema()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_schema.md):
  table metadata
- [`sb_db_read()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_read.md):
  read table data with optional limit
- [`sb_db_query()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_query.md):
  query with `where` operators (gt, lt, like, in, etc.) or raw SQL
- [`sb_db_insert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_insert.md),
  [`sb_db_update()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_update.md),
  [`sb_db_delete()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_delete.md),
  [`sb_db_truncate()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_truncate.md):
  CRUD operations
- [`sb_db_upsert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_upsert.md):
  insert or update with conflict handling
- Backend detection on package load with status display
- cli-powered success messages for all write operations

### Internal

- Use
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  for all SQL construction
- Add
  [`build_where()`](https://deepanshkhurana.github.io/supabaseR/reference/build_where.md)
  helper for operator support
- Add comprehensive test coverage with testthat 3.0 snapshots

## supabaseR 0.0.1

- Initial release with basic CRUD functions
- Environment variable configuration
