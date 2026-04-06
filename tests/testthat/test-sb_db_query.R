describe("sb_db_query()", {
  it("should error when not connected with raw SQL", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_query(sql = "SELECT * FROM users"), error = TRUE)
  })

  it("should error when not connected with structured query", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_query(table = "users"), error = TRUE)
  })

  it("should accept columns, where, and limit parameters", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(
      sb_db_query(
        table = "orders",
        columns = c("id", "total"),
        where = list(user_id = 42),
        limit = 5
      ),
      error = TRUE
    )
  })

  it("should execute raw SQL directly when sql is provided", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_query, "get_connection", function() mock_conn)
    expected <- data.frame(count = 1L)
    mockery::stub(sb_db_query, "DBI::dbGetQuery", function(conn, q, ...) expected)
    # Act
    result <- sb_db_query(sql = "SELECT COUNT(*) FROM users")
    # Assert
    expect_equal(result, expected)
  })

  it("should run structured query with table and columns", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_query, "get_connection", function() mock_conn)
    mockery::stub(sb_db_query, "sb_db_table_exists", function(...) TRUE)
    expected <- data.frame(id = 1L, name = "Alice")
    mockery::stub(sb_db_query, "DBI::dbGetQuery", function(...) expected)
    # Act
    result <- sb_db_query("users", columns = c("id", "name"))
    # Assert
    expect_equal(result, expected)
  })

  it("should apply where clause and limit in structured query", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_query, "get_connection", function() mock_conn)
    mockery::stub(sb_db_query, "sb_db_table_exists", function(...) TRUE)
    captured_query <- NULL
    mockery::stub(sb_db_query, "DBI::dbGetQuery", function(conn, q, ...) {
      captured_query <<- as.character(q)
      data.frame()
    })
    # Act
    sb_db_query("orders",
                where = list(user_id = 42),
                limit = 5)
    # Assert
    expect_match(captured_query, "WHERE")
    expect_match(captured_query, "LIMIT")
  })

  it("should error when table does not exist in structured query", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_query, "get_connection", function() mock_conn)
    mockery::stub(sb_db_query, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_query("no_such"))
  })
})
