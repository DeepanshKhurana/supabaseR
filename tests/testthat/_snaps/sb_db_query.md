# sb_db_query() / should error when not connected with raw SQL

    Code
      sb_db_query(sql = "SELECT * FROM users")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_query() / should error when not connected with structured query

    Code
      sb_db_query(table = "users")
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

# sb_db_query() / should accept columns, where, and limit parameters

    Code
      sb_db_query(table = "orders", columns = c("id", "total"), where = list(user_id = 42),
      limit = 5)
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

