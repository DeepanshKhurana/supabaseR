# sb_db_schema() / should error on invalid table argument

    Code
      sb_db_schema(table = NULL)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_schema(table = 123)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_schema() / should error when not connected

    Code
      sb_db_schema(table = "users")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

