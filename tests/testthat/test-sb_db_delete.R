describe("sb_db_delete()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_snapshot(sb_db_delete(table = NULL, where = list(id = 1)), error = TRUE)
    expect_snapshot(sb_db_delete(table = "users", where = list()), error = TRUE)
    expect_snapshot(sb_db_delete(table = "users", where = NULL), error = TRUE)
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_delete(table = "users", where = list(id = 1)), error = TRUE)
  })

  it("should return row count on successful delete", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_delete, "get_connection", function() mock_conn)
    mockery::stub(sb_db_delete, "sb_db_table_exists", function(...) TRUE)
    mockery::stub(sb_db_delete, "DBI::dbExecute", function(...) 3L)
    # Act
    n <- sb_db_delete("users", where = list(id = 1))
    # Assert
    expect_equal(n, 3L)
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_delete, "get_connection", function() mock_conn)
    mockery::stub(sb_db_delete, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_delete("no_such", where = list(id = 1)))
  })
})
