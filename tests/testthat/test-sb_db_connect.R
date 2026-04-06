describe("sb_db_connect()", {
  it("should error when DBI credentials are not available", {
    # Arrange
    .sb_env$dbi_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_db_connect(), error = TRUE)
  })

  it("should connect and store connection when credentials available", {
    # Arrange
    .sb_env$dbi_available <- TRUE
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_connect, "sb_db_creds", function() {
      list(host = "localhost", port = 5432L, dbname = "test",
           user = "user", password = "pass")
    })
    mockery::stub(sb_db_connect, "DBI::dbConnect", function(...) mock_conn)
    # Act
    sb_db_connect()
    # Assert
    expect_identical(.sb_env$conn, mock_conn)
    .sb_env$conn <- NULL
    .sb_env$dbi_available <- FALSE
  })

  it("should set schema from argument", {
    # Arrange
    .sb_env$dbi_available <- TRUE
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_connect, "sb_db_creds", function() {
      list(host = "localhost", port = 5432L, dbname = "test",
           user = "user", password = "pass")
    })
    mockery::stub(sb_db_connect, "DBI::dbConnect", function(...) mock_conn)
    # Act
    sb_db_connect(schema = "myschema")
    # Assert
    expect_equal(.sb_env$schema, "myschema")
    .sb_env$conn <- NULL
    .sb_env$schema <- NULL
    .sb_env$dbi_available <- FALSE
  })
})

describe("sb_db_disconnect()", {
  it("should handle disconnect when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_no_error(sb_db_disconnect())
  })

  it("should disconnect and clear conn when connected", {
    # Arrange
    mock_conn <- DBI::ANSI()
    .sb_env$conn <- mock_conn
    mockery::stub(sb_db_disconnect, "DBI::dbDisconnect", function(...) invisible(NULL))
    # Act
    sb_db_disconnect()
    # Assert
    expect_null(.sb_env$conn)
  })
})

describe("get_connection()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(get_connection(), error = TRUE)
  })

  it("should return connection when connected", {
    # Arrange
    mock_conn <- DBI::ANSI()
    .sb_env$conn <- mock_conn
    # Act
    result <- get_connection()
    # Assert
    expect_identical(result, mock_conn)
    .sb_env$conn <- NULL
  })
})

describe("get_schema()", {
  it("should return schema from environment or stored value", {
    # Arrange
    .sb_env$schema <- NULL
    withr::local_envvar(SUPABASE_SCHEMA = "public")
    # Act and Assert
    expect_equal(get_schema(), "public")

    .sb_env$schema <- "custom_schema"
    expect_equal(get_schema(), "custom_schema")
    .sb_env$schema <- NULL
  })

  it("should fall back to SUPABASE_SCHEMA env var when schema is empty string", {
    # Arrange
    .sb_env$schema <- ""
    withr::local_envvar(SUPABASE_SCHEMA = "envschema")
    # Act and Assert
    expect_equal(get_schema(), "envschema")
    .sb_env$schema <- NULL
  })
})
