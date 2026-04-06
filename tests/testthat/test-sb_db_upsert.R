describe("sb_db_upsert()", {
  it("should error on invalid arguments", {
    # Arrange, Act and Assert
    expect_error(
      sb_db_upsert(table = NULL, data = data.frame(a = 1), conflict_columns = "id")
    )
    expect_error(
      sb_db_upsert(table = "users", data = NULL, conflict_columns = "id")
    )
    expect_error(
      sb_db_upsert(table = "users", data = data.frame(a = 1), conflict_columns = NULL)
    )
  })

  it("should error when not connected", {
    # Arrange
    .sb_env$conn <- NULL
    # Act and Assert
    expect_error(
      sb_db_upsert(
        table = "users",
        data = data.frame(id = 1, name = "Alice"),
        conflict_columns = "id"
      )
    )
  })

  it("should return row count on successful upsert", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_upsert, "get_connection", function() mock_conn)
    mockery::stub(sb_db_upsert, "sb_db_table_exists", function(...) TRUE)
    mockery::stub(sb_db_upsert, "DBI::dbExecute", function(...) 1L)
    # Act
    n <- sb_db_upsert("users",
                      data = data.frame(id = 1L, name = "Alice"),
                      conflict_columns = "id")
    # Assert
    expect_equal(n, 1L)
  })

  it("should handle NA values in data by emitting NULL in SQL", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_upsert, "get_connection", function() mock_conn)
    mockery::stub(sb_db_upsert, "sb_db_table_exists", function(...) TRUE)
    captured_query <- NULL
    mockery::stub(sb_db_upsert, "DBI::dbExecute", function(conn, q, ...) {
      captured_query <<- as.character(q)
      1L
    })
    # Act
    sb_db_upsert("users",
                 data = data.frame(id = 1L, name = NA_character_),
                 conflict_columns = "id")
    # Assert
    expect_match(captured_query, "NULL")
  })

  it("should error when table does not exist", {
    # Arrange
    mock_conn <- DBI::ANSI()
    mockery::stub(sb_db_upsert, "get_connection", function() mock_conn)
    mockery::stub(sb_db_upsert, "sb_db_table_exists", function(...) FALSE)
    # Act and Assert
    expect_error(sb_db_upsert("no_such",
                              data = data.frame(id = 1L),
                              conflict_columns = "id"))
  })
})
