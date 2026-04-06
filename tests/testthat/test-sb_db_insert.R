describe("sb_db_insert()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_snapshot(sb_db_insert(table = NULL, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = 123, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = "users", data = list(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = "users", data = NULL), error = TRUE)
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_insert(table = "users", data = data.frame(name = "Alice")), error = TRUE)
  })

  it("should return row count on successful insert", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_insert, "get_connection", function() mock_conn)
    mockery::stub(sb_db_insert, "sb_db_table_exists", function(...) TRUE)
    mockery::stub(sb_db_insert, "DBI::dbAppendTable", function(...) 2L)
    # Act
    n <- sb_db_insert("users", data.frame(name = c("Alice", "Bob")))
    # Assert
    expect_equal(n, 2L)
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_insert, "get_connection", function() mock_conn)
    mockery::stub(sb_db_insert, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_insert("no_such", data.frame(name = "Alice")))
  })
})
