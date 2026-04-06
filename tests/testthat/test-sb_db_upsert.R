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
    local_mocked_bindings(
      get_connection = function() mock_conn,
      sb_db_table_exists = function(...) TRUE,
      .package = "supabaseR"
    )
    local_mocked_bindings(
      dbExecute = function(...) 1L,
      .package = "DBI"
    )
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
    captured_query <- NULL
    local_mocked_bindings(
      get_connection = function() mock_conn,
      sb_db_table_exists = function(...) TRUE,
      .package = "supabaseR"
    )
    local_mocked_bindings(
      dbExecute = function(conn, statement, ...) {
        captured_query <<- as.character(statement)
        1L
      },
      .package = "DBI"
    )
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
    local_mocked_bindings(
      get_connection = function() mock_conn,
      sb_db_table_exists = function(...) FALSE,
      .package = "supabaseR"
    )
    # Act and Assert
    expect_error(sb_db_upsert("no_such",
                              data = data.frame(id = 1L),
                              conflict_columns = "id"))
  })
})
