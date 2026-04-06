describe("sb_db_update()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_snapshot(
      sb_db_update(
        table = NULL,
        data = list(name = "Bob"),
        where = list(id = 1)
      ),
      error = TRUE
    )
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(),
        where = list(id = 1)
      ),
      error = TRUE
    )
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(name = "Bob"),
        where = list()
      ),
      error = TRUE
    )
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(name = "Bob"),
        where = list(id = 1)
      ),
      error = TRUE
    )
  })

  it("should return row count on successful update", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_update, "get_connection", function() mock_conn)
    mockery::stub(sb_db_update, "sb_db_table_exists", function(...) TRUE)
    mockery::stub(sb_db_update, "DBI::dbExecute", function(...) 1L)
    # Act
    n <- sb_db_update("users",
                      data = list(name = "Bob"),
                      where = list(id = 1))
    # Assert
    expect_equal(n, 1L)
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_update, "get_connection", function() mock_conn)
    mockery::stub(sb_db_update, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_update("no_such",
                              data = list(name = "X"),
                              where = list(id = 1)))
  })
})
