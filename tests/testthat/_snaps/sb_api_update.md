# sb_api_update() / should error when API connection is not available

    Code
      sb_api_update("users", data = list(name = "Bob"), where = list(id = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

# sb_api_update() / should error on invalid arguments

    Code
      sb_api_update(table = NULL, data = list(a = 1), where = list(id = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_update(table = "users", data = list(), where = list(id = 1))
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_update(table = "users", data = list(a = 1), where = list())
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

---

    Code
      sb_api_update(table = "users", data = list(a = 1), where = NULL)
    Condition
      Error in `.check_api_available()`:
      x API credentials not available.
      i Call `sb_api_connect()` or set `SUPABASE_URL` and at least one of `SUPABASE_PUBLISHABLE_KEY`, `SUPABASE_ANON_KEY`, `SUPABASE_SECRET_KEY`, or `SUPABASE_ROLE_KEY`.

