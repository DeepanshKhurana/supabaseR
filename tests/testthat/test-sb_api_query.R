describe("sb_api_query()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_query("test_table"), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_query(table = NULL), error = TRUE)
    expect_snapshot(sb_api_query(table = 123), error = TRUE)
    expect_snapshot(sb_api_query(table = "users", limit = "ten"), error = TRUE)
  })

  it("should return a tibble on successful GET", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(body = '[{"id":1}]'))
    mockery::stub(sb_api_query, ".sb_api_request", m)
    # Act
    result <- sb_api_query("users")
    # Assert
    mockery::expect_called(m, 1)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "GET")
    expect_s3_class(result, "tbl_df")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should pass where and columns params", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(body = "[]"))
    mockery::stub(sb_api_query, ".sb_api_request", m)
    # Act
    sb_api_query("users",
                 columns = c("id", "name"),
                 where = list(id = 1),
                 limit = 10)
    # Assert
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["params"]]$select, "id,name")
    expect_equal(args[["params"]]$limit, 10)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on error response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_query, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 400L,
        body = '{"message":"Bad Request"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_query("users"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
