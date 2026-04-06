describe("sb_api_read()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_read("test_endpoint"), error = TRUE)
  })

  it("should return a tibble on successful GET", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(body = '[{"id":1,"name":"Alice"}]'))
    mockery::stub(sb_api_read, ".sb_api_request", m)
    # Act
    result <- sb_api_read("users")
    # Assert
    mockery::expect_called(m, 1)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "GET")
    expect_equal(args[["path"]], "rest/v1/users")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1L)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should pass limit param when limit > 0", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(body = "[]"))
    mockery::stub(sb_api_read, ".sb_api_request", m)
    # Act
    sb_api_read("users", limit = 5)
    # Assert
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["params"]]$limit, 5)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on non-200 response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_read, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 404L,
        body = '{"message":"Not Found"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_read("users"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
