# sb_api_insert() / should error when API connection is not available

    Code
      sb_api_insert("users", data.frame(name = "Alice"))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

# sb_api_insert() / should error on invalid arguments

    Code
      sb_api_insert(table = NULL, data = data.frame(a = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_insert(table = 123, data = data.frame(a = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_insert(table = "users", data = list(a = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_insert(table = "users", data = NULL)
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

