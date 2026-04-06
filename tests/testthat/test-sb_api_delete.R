describe("sb_api_delete()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_delete("users", where = list(id = 1)), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_delete(table = NULL, where = list(id = 1)), error = TRUE)
    expect_snapshot(sb_api_delete(table = "users", where = list()), error = TRUE)
    expect_snapshot(sb_api_delete(table = "users", where = NULL), error = TRUE)
  })

  it("should return row count on successful DELETE", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(
      status_code = 204L,
      content_range = "*/3"
    ))
    mockery::stub(sb_api_delete, ".sb_api_request", m)
    # Act
    n <- sb_api_delete("users", where = list(id = 1))
    # Assert
    expect_equal(n, 3L)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "DELETE")
    expect_equal(args[["path"]], "rest/v1/users")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on error response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_delete, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 403L,
        body = '{"message":"Forbidden"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_delete("users", where = list(id = 1)))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
