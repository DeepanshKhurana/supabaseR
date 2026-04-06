describe("sb_db_creds()", {
  it("should return credentials from envvars", {
    # Arrange
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password",
      SUPABASE_SCHEMA = "public"
    )
    # Act and Assert
    expect_snapshot(sb_db_creds())
  })

  it("should error when credentials are missing", {
    # Arrange
    withr::local_envvar(
      SUPABASE_HOST = "",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password"
    )
    # Act and Assert
    expect_snapshot(sb_db_creds(), error = TRUE)
  })

  it("should not require schema", {
    # Arrange
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password",
      SUPABASE_SCHEMA = ""
    )
    # Act and Assert
    expect_equal(sb_db_creds()$host, "localhost")
  })

  it("should include port 6543", {
    # Arrange
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "test_db",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "password"
    )
    # Act and Assert
    expect_equal(sb_db_creds()$port, 6543)
  })
})
