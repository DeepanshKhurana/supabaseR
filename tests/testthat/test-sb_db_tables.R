describe("sb_db_tables()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(sb_db_tables(), error = TRUE)
  })

  it("should return character vector of table names", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_tables, "get_connection", function() mock_conn)
    expected_df <- data.frame(table_name = c("users", "posts"),
                              stringsAsFactors = FALSE)
    mockery::stub(sb_db_tables, "DBI::dbGetQuery", function(...) expected_df)
    # Act
    result <- sb_db_tables()
    # Assert
    expect_equal(result, c("users", "posts"))
  })
})
