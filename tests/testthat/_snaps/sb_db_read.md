# sb_db_read() / should error on invalid arguments

    Code
      sb_db_read(table = NULL)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_read(table = 123)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_read(table = "users", limit = "ten")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_read() / should error when not connected

    Code
      sb_db_read(table = "users")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

