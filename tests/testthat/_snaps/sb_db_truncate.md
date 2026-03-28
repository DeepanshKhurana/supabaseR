# sb_db_truncate() / should error on invalid arguments

    Code
      sb_db_truncate(table = NULL)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_truncate(table = 123)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_truncate() / should error when not connected

    Code
      sb_db_truncate(table = "temp_data")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

