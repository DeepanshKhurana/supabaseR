# sb_db_creds() / should return credentials from envvars

    Code
      sb_db_creds()
    Output
      $host
      [1] "localhost"
      
      $port
      [1] 6543
      
      $dbname
      [1] "test_db"
      
      $user
      [1] "user"
      
      $password
      [1] "password"
      
      $schema
      [1] "public"
      

# sb_db_creds() / should error when credentials are missing

    Code
      sb_db_creds()
    Condition
      Error in `sb_db_creds()`:
      ! Missing Supabase credentials: host. Set environment variables: SUPABASE_HOST, SUPABASE_DBNAME, SUPABASE_USER, SUPABASE_PASSWORD

