# sb_api_delete() / should error when API connection is not available

    Code
      sb_api_delete("users", where = list(id = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

# sb_api_delete() / should error on invalid arguments

    Code
      sb_api_delete(table = NULL, where = list(id = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_delete(table = "users", where = list())
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_delete(table = "users", where = NULL)
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

