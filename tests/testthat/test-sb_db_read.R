describe("sb_db_read()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_snapshot(sb_db_read(table = NULL), error = TRUE)
    expect_snapshot(sb_db_read(table = 123), error = TRUE)
    expect_snapshot(sb_db_read(table = "users", limit = "ten"), error = TRUE)
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_read(table = "users"), error = TRUE)
  })

  it("should return data frame on successful query", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_read, "get_connection", function() mock_conn)
    mockery::stub(sb_db_read, "sb_db_table_exists", function(...) TRUE)
    expected <- data.frame(id = 1L, name = "Alice")
    mockery::stub(sb_db_read, "DBI::dbGetQuery", function(...) expected)
    # Act
    result <- sb_db_read("users")
    # Assert
    expect_equal(result, expected)
  })

  it("should pass LIMIT clause when limit > 0", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_read, "get_connection", function() mock_conn)
    mockery::stub(sb_db_read, "sb_db_table_exists", function(...) TRUE)
    captured_query <- NULL
    mockery::stub(sb_db_read, "DBI::dbGetQuery", function(conn, q, ...) {
      captured_query <<- as.character(q)
      data.frame()
    })
    # Act
    sb_db_read("users", limit = 10)
    # Assert
    expect_match(captured_query, "LIMIT")
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_read, "get_connection", function() mock_conn)
    mockery::stub(sb_db_read, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_read("no_such_table"))
  })
})
