describe("sb_api_connect()", {
  it("should error when API credentials are not available", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ROLE_KEY = ""
    )
    # Act and Assert
    expect_snapshot(sb_api_connect(), error = TRUE)
  })

  it("should set credentials successfully with new-format publishable key", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "sb_publishable_testkey",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ROLE_KEY = "",
      SUPABASE_ANON_KEY = ""
    )
    # Act
    sb_api_connect()
    # Assert
    expect_true(isTRUE(.sb_env$api_available))
    expect_equal(.sb_env$api_url, "https://test.supabase.co")
    expect_equal(.sb_env$api_key, "sb_publishable_testkey")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should set secret_key when SUPABASE_SECRET_KEY is provided", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_SECRET_KEY = "sb_secret_testkey",
      SUPABASE_ROLE_KEY = ""
    )
    # Act
    sb_api_connect()
    # Assert
    expect_equal(.sb_env$api_secret_key, "sb_secret_testkey")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_secret_key <- NULL
  })

  it("should warn on legacy JWT anon key", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_ANON_KEY = "eyJlegacyanonkey",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ROLE_KEY = ""
    )
    # Act and Assert
    expect_warning(sb_api_connect())
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should warn on legacy JWT secret key", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_SECRET_KEY = "eyJlegacysecretkey",
      SUPABASE_ROLE_KEY = ""
    )
    # Act and Assert
    expect_warning(sb_api_connect())
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_secret_key <- NULL
  })
})
