describe("sb_api_truncate()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_truncate("users"), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_truncate(table = NULL), error = TRUE)
    expect_snapshot(sb_api_truncate(table = 123), error = TRUE)
  })

  it("should return invisibly on success", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(status_code = 204L))
    mockery::stub(sb_api_truncate, ".sb_api_request", m)
    # Act
    result <- sb_api_truncate("users")
    # Assert
    expect_null(result)
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
    mockery::stub(sb_api_truncate, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 403L,
        body = '{"message":"Forbidden"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_truncate("users"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
