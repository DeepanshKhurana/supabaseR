describe("startup_message()", {
  it("should include package name and version", {
    # Act
    msg <- startup_message()
    # Assert
    expect_match(msg, "supabaseR")
  })

  it("should show green tick when DBI available", {
    # Arrange
    .sb_env$dbi_available <- TRUE
    # Act
    msg <- startup_message()
    # Assert
    expect_match(msg, "DBI Backend")
    .sb_env$dbi_available <- FALSE
  })

  it("should show red cross when DBI not available", {
    # Arrange
    .sb_env$dbi_available <- FALSE
    # Act
    msg <- startup_message()
    # Assert
    expect_match(msg, "DBI Backend")
  })

  it("should show green tick when API available", {
    # Arrange
    .sb_env$api_available <- TRUE
    # Act
    msg <- startup_message()
    # Assert
    expect_match(msg, "API Backend")
    .sb_env$api_available <- FALSE
  })

  it("should show red cross when API not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act
    msg <- startup_message()
    # Assert
    expect_match(msg, "API Backend")
  })
})

describe(".onLoad()", {
  it("should detect DBI credentials when all vars are set", {
    # Arrange
    withr::local_envvar(
      SUPABASE_HOST = "localhost",
      SUPABASE_DBNAME = "testdb",
      SUPABASE_USER = "user",
      SUPABASE_PASSWORD = "pass"
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_true(.sb_env$dbi_available)
    .sb_env$dbi_available <- FALSE
  })

  it("should set api_available when SUPABASE_URL and key are set", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "sb_publishable_test",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_ROLE_KEY = ""
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_true(.sb_env$api_available)
    expect_equal(.sb_env$api_url, "https://test.supabase.co")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should set legacy_key_warning when anon key starts with eyJ", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_ANON_KEY = "eyJlegacyanonkey",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ROLE_KEY = ""
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_true(.sb_env$legacy_key_warning)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
    .sb_env$legacy_key_warning <- FALSE
  })

  it("should set legacy_secret_warning when secret key starts with eyJ", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_SECRET_KEY = "eyJlegacysecretkey",
      SUPABASE_ROLE_KEY = ""
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_true(.sb_env$legacy_secret_warning)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_secret_key <- NULL
    .sb_env$legacy_secret_warning <- FALSE
  })

  it("should use SUPABASE_ROLE_KEY as fallback when SUPABASE_SECRET_KEY is unset", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "https://test.supabase.co",
      SUPABASE_PUBLISHABLE_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ROLE_KEY = "sb_secret_rolekey"
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_true(.sb_env$api_available)
    expect_equal(.sb_env$api_secret_key, "sb_secret_rolekey")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_secret_key <- NULL
  })

  it("should not set api_available when URL is missing", {
    # Arrange
    withr::local_envvar(
      SUPABASE_URL = "",
      SUPABASE_PUBLISHABLE_KEY = "sb_publishable_test",
      SUPABASE_SECRET_KEY = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_ROLE_KEY = ""
    )
    # Act
    .onLoad(NULL, NULL)
    # Assert
    expect_false(.sb_env$api_available)
  })
})

describe(".onAttach()", {
  it("should warn when legacy_key_warning is TRUE", {
    # Arrange
    .sb_env$legacy_key_warning <- TRUE
    .sb_env$legacy_secret_warning <- FALSE
    # Act and Assert
    expect_warning(.onAttach(NULL, NULL))
    .sb_env$legacy_key_warning <- FALSE
  })

  it("should warn when legacy_secret_warning is TRUE", {
    # Arrange
    .sb_env$legacy_key_warning <- FALSE
    .sb_env$legacy_secret_warning <- TRUE
    # Act and Assert
    expect_warning(.onAttach(NULL, NULL))
    .sb_env$legacy_secret_warning <- FALSE
  })

  it("should not warn when no legacy warnings are set", {
    # Arrange
    .sb_env$legacy_key_warning <- FALSE
    .sb_env$legacy_secret_warning <- FALSE
    # Act and Assert
    expect_no_warning(.onAttach(NULL, NULL))
  })
})

describe(".onUnload()", {
  it("should disconnect when conn is not null on unload", {
    # Arrange
    mock_conn <- DBI::ANSI()
    .sb_env$conn <- mock_conn
    mockery::stub(.onUnload, "DBI::dbDisconnect", function(...) invisible(NULL))
    # Act and Assert
    expect_no_error(.onUnload(NULL))
    .sb_env$conn <- NULL
  })

  it("should not error when conn is null on unload", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_no_error(.onUnload(NULL))
  })
})
