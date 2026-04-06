describe("sb_db_truncate()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_snapshot(sb_db_truncate(table = NULL), error = TRUE)
    expect_snapshot(sb_db_truncate(table = 123), error = TRUE)
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_truncate(table = "temp_data"), error = TRUE)
  })

  it("should return invisibly on success", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_truncate, "get_connection", function() mock_conn)
    mockery::stub(sb_db_truncate, "sb_db_table_exists", function(...) TRUE)
    mockery::stub(sb_db_truncate, "DBI::dbExecute", function(...) 0L)
    # Act
    result <- sb_db_truncate("users")
    # Assert
    expect_null(result)
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_truncate, "get_connection", function() mock_conn)
    mockery::stub(sb_db_truncate, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_truncate("no_such"))
  })
})
