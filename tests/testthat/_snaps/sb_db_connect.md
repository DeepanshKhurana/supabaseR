# sb_db_connect() / should error when DBI credentials are not available

    Code
      sb_db_connect()
    Condition
      Error in `sb_db_connect()`:
      ! DBI credentials not found.
            Set SUPABASE_HOST, SUPABASE_DBNAME, SUPABASE_USER, SUPABASE_PASSWORD.

# get_connection() / should error when not connected

    Code
      get_connection()
    Condition
      Error in `get_connection()`:
      ! Not connected. Call sb_db_connect() first.

