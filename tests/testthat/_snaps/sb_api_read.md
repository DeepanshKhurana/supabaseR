# sb_api_read() / should error when API connection is not available

    Code
      sb_api_read("test_endpoint")
    Condition
      Error in `sb_api_read()`:
      x API credentials not available.
      i Set SUPABASE_URL, SUPABASE_ANON_KEY, and SUPABASE_ROLE_KEY.

