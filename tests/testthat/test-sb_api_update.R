describe("sb_api_update()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(
      sb_api_update("users", data = list(name = "Bob"), where = list(id = 1)),
      error = TRUE
    )
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(
      sb_api_update(table = NULL, data = list(a = 1), where = list(id = 1)),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(), where = list(id = 1)),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(a = 1), where = list()),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(a = 1), where = NULL),
      error = TRUE
    )
  })

  it("should return row count on successful PATCH", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(
      status_code = 204L,
      content_range = "*/1"
    ))
    mockery::stub(sb_api_update, ".sb_api_request", m)
    # Act
    n <- sb_api_update("users", data = list(name = "Bob"), where = list(id = 1))
    # Assert
    expect_equal(n, 1L)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "PATCH")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on error response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_update, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 400L,
        body = '{"message":"Bad filter"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_update("users",
                               data = list(name = "X"),
                               where = list(id = 1)))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
