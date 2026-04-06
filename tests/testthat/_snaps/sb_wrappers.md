# get_backend() / should error when not connected

    Code
      get_backend()
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_connect() / should error when no credentials available in auto mode

    Code
      sb_connect()
    Condition
      Error in `sb_connect()`:
      ! No credentials found. Set DBI or API environment variables.

# sb_disconnect() / should error when not connected

    Code
      sb_disconnect()
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_read() / should error when not connected

    Code
      sb_read("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_insert() / should error when not connected

    Code
      sb_insert("t", data.frame())
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_update() / should error when not connected

    Code
      sb_update("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_delete() / should error when not connected

    Code
      sb_delete("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_upsert() / should error when not connected

    Code
      sb_upsert("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_query() / should error when not connected

    Code
      sb_query("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_query() / should error when sql provided with API backend

    Code
      sb_query(table = "t", sql = "SELECT 1")
    Condition
      Error in `sb_query()`:
      x Raw SQL is not supported with the API backend.

# sb_tables() / should error when not connected

    Code
      sb_tables()
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_schema() / should error when not connected

    Code
      sb_schema("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_table_exists() / should error when not connected

    Code
      sb_table_exists("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

# sb_truncate() / should error when not connected

    Code
      sb_truncate("t")
    Condition
      Error in `get_backend()`:
      ! Not connected. Call sb_connect() first.

