# Package index

## Connection

Connect and manage Supabase connections

### Unified API

Auto-dispatches to DBI or REST based on available credentials

- [`sb_connect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_connect.md)
  : Connect to Supabase
- [`sb_disconnect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_disconnect.md)
  : Disconnect from Supabase
- [`sb_status()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_status.md)
  : Get connection status

### DBI Backend

Direct PostgreSQL connection via DBI

- [`sb_db_connect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_connect.md)
  : Connect to Supabase
- [`sb_db_disconnect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_disconnect.md)
  : Disconnect from Supabase
- [`sb_db_status()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_status.md)
  : Check DBI backend status
- [`sb_db_creds()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_creds.md)
  : Read Supabase credentials from environment variables

### REST API Backend

Supabase REST API via PostgREST

- [`sb_api_connect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_connect.md)
  : Connect to Supabase via REST API
- [`sb_api_disconnect()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_disconnect.md)
  : Disconnect from Supabase API
- [`sb_api_status()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_status.md)
  : Check API backend status

## Read Operations

Query and read data from tables

### Unified API

- [`sb_read()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_read.md)
  : Read table data
- [`sb_query()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_query.md)
  : Query a table
- [`sb_tables()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_tables.md)
  : List tables
- [`sb_schema()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_schema.md)
  : Get table schema
- [`sb_table_exists()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_table_exists.md)
  : Check if table exists

### DBI Backend

- [`sb_db_read()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_read.md)
  : Read table data
- [`sb_db_query()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_query.md)
  : Query a table
- [`sb_db_tables()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_tables.md)
  : List tables in schema
- [`sb_db_schema()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_schema.md)
  : Get table schema
- [`sb_db_table_exists()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_table_exists.md)
  : Check if table exists

### REST API Backend

- [`sb_api_read()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_read.md)
  : Read table data via API
- [`sb_api_query()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_query.md)
  : Query a table via API
- [`sb_api_tables()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_tables.md)
  : List tables via API
- [`sb_api_schema()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_schema.md)
  : Get table schema via API
- [`sb_api_table_exists()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_table_exists.md)
  : Check if a table exists via API

## Write Operations

Insert, update, delete, and upsert data

### Unified API

- [`sb_insert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_insert.md)
  : Insert rows into a table
- [`sb_update()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_update.md)
  : Update rows in a table
- [`sb_delete()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_delete.md)
  : Delete rows from a table
- [`sb_upsert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_upsert.md)
  : Upsert rows into a table
- [`sb_truncate()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_truncate.md)
  : Truncate a table

### DBI Backend

- [`sb_db_insert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_insert.md)
  : Insert rows into a table
- [`sb_db_update()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_update.md)
  : Update rows in a table
- [`sb_db_delete()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_delete.md)
  : Delete rows from a table
- [`sb_db_upsert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_upsert.md)
  : Upsert rows into a table
- [`sb_db_truncate()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_db_truncate.md)
  : Truncate a table

### REST API Backend

- [`sb_api_insert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_insert.md)
  : Insert rows into a table via API
- [`sb_api_update()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_update.md)
  : Update rows in a table via API
- [`sb_api_delete()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_delete.md)
  : Delete rows from a table via API
- [`sb_api_upsert()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_upsert.md)
  : Upsert rows into a table via API
- [`sb_api_truncate()`](https://deepanshkhurana.github.io/supabaseR/reference/sb_api_truncate.md)
  : Truncate a table via API
