# sb_db_update() / should error on invalid arguments

    Code
      sb_db_update(table = NULL, data = list(name = "Bob"), where = list(id = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_update(table = "users", data = list(), where = list(id = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_update(table = "users", data = list(name = "Bob"), where = list())
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_update() / should error when not connected

    Code
      sb_db_update(table = "users", data = list(name = "Bob"), where = list(id = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

