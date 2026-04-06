describe("sb_db_schema()", {
  it("should error on invalid table argument", {
    # Arrange, Act and Assert
    expect_snapshot(sb_db_schema(table = NULL), error = TRUE)
    expect_snapshot(sb_db_schema(table = 123), error = TRUE)
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_schema(table = "users"), error = TRUE)
  })

  it("should return data frame with column info", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_schema, "get_connection", function() mock_conn)
    mockery::stub(sb_db_schema, "sb_db_table_exists", function(...) TRUE)
    expected <- data.frame(
      column_name = c("id", "name"),
      data_type = c("integer", "character varying"),
      stringsAsFactors = FALSE
    )
    mockery::stub(sb_db_schema, "DBI::dbGetQuery", function(...) expected)
    # Act
    result <- sb_db_schema("users")
    # Assert
    expect_equal(result, expected)
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_schema, "get_connection", function() mock_conn)
    mockery::stub(sb_db_schema, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_schema("no_such"))
  })
})
