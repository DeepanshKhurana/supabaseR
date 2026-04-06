# sb_api_schema() / should error when API connection is not available

    Code
      sb_api_schema("users")
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

# sb_api_schema() / should error on invalid arguments

    Code
      sb_api_schema(table = NULL)
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_schema(table = 123)
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

