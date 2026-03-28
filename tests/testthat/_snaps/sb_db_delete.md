# sb_db_delete() / should error on invalid arguments

    Code
      sb_db_delete(table = NULL, where = list(id = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_delete(table = "users", where = list())
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_delete(table = "users", where = NULL)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_delete() / should error when not connected

    Code
      sb_db_delete(table = "users", where = list(id = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

