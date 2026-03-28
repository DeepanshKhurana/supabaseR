describe("sb_db_creds()", {
  it("should return credentials from envvars", {
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password",
      SUPABASE_SCHEMA = "public"
    )
    expect_snapshot(sb_db_creds())
  })

  it("should error when credentials are missing", {
    withr::local_envvar(
      SUPABASE_HOST = "",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password"
    )
    expect_snapshot(sb_db_creds(), error = TRUE)
  })

  it("should not require schema", {
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password",
      SUPABASE_SCHEMA = ""
    )
    expect_equal(sb_db_creds()$host, "localhost")
  })

  it("should include port 6543", {
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password"
    )
    expect_equal(sb_db_creds()$port, 6543)
  })
})
