# sb_db_insert() / should error on invalid arguments

    Code
      sb_db_insert(table = NULL, data = data.frame(a = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_insert(table = 123, data = data.frame(a = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_insert(table = "users", data = list(a = 1))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

---

    Code
      sb_db_insert(table = "users", data = NULL)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_insert() / should error when not connected

    Code
      sb_db_insert(table = "users", data = data.frame(name = "Alice"))
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

