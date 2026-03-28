describe("read_supabase_creds()", {
  it("should return a Supabase credentials from the envvars", {
    Sys.setenv(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password",
      SUPABASE_SCHEMA = "public"
  )
    
    creds <- read_supabase_creds()
    
    expect_identical(creds$host, "localhost")
    expect_identical(creds$dbname, "test_db")
    expect_identical(creds$user, "user")
    expect_identical(creds$password, "password")
    expect_identical(creds$schema, "public")
  })

  it("should throw an error if any credential is missing", {
    Sys.setenv(SUPABASE_HOST = "")
    
    expect_error(
      read_supabase_creds(), 
      "Cannot connect to Supabase. Are environment variables set?"
    )
  })
})