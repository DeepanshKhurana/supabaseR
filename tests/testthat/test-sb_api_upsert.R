describe("sb_api_upsert()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(
      sb_api_upsert("users", data = data.frame(id = 1), conflict_columns = "id"),
      error = TRUE
    )
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(
      sb_api_upsert(table = NULL, data = data.frame(id = 1), conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_api_upsert(table = "users", data = list(id = 1), conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_api_upsert(table = "users", data = data.frame(id = 1), conflict_columns = NULL),
      error = TRUE
    )
  })

  it("should return row count on successful upsert", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(
      status_code = 201L,
      content_range = "*/1"
    ))
    mockery::stub(sb_api_upsert, ".sb_api_request", m)
    # Act
    n <- sb_api_upsert("users",
                       data = data.frame(id = 1L, name = "Alice"),
                       conflict_columns = "id")
    # Assert
    expect_equal(n, 1L)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "POST")
    expect_match(args[["params"]]$on_conflict, "id")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on error response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_upsert, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 400L,
        body = '{"message":"Bad Request"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_upsert("users",
                               data = data.frame(id = 1L),
                               conflict_columns = "id"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
