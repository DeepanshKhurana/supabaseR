# sb_api_read(): should error when API connection is not available

    Code
      sb_api_read("test_endpoint")
    Error <rlang_error>
      [38;5;252m[31m✖[38;5;252m API credentials not available.
      [36mℹ[38;5;252m Please set environment variables: SUPABASE_URL, SUPABASE_ANON_KEY, SUPABASE_ROLE_KEY.[39m

