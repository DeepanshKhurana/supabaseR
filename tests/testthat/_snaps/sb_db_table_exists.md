# sb_db_table_exists() / should error on invalid table argument

    Code
      sb_db_table_exists(table = NULL)
    Condition
      Error in `sb_db_table_exists()`:
      ! Assertion on 'table' failed: Must be of type 'string', not 'NULL'.

---

    Code
      sb_db_table_exists(table = 123)
    Condition
      Error in `sb_db_table_exists()`:
      ! Assertion on 'table' failed: Must be of type 'string', not 'double'.

# sb_db_table_exists() / should error when not connected

    Code
      sb_db_table_exists(table = "users")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

