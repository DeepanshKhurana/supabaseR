describe("sb_api_insert()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_insert("users", data.frame(name = "Alice")), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_insert(table = NULL, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = 123, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = "users", data = list(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = "users", data = NULL), error = TRUE)
  })

  it("should return row count on successful POST", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    m <- mockery::mock(make_mock_response(
      status_code = 201L,
      content_range = "*/2"
    ))
    mockery::stub(sb_api_insert, ".sb_api_request", m)
    # Act
    n <- sb_api_insert("users", data.frame(name = c("Alice", "Bob")))
    # Assert
    expect_equal(n, 2L)
    args <- mockery::mock_args(m)[[1]]
    expect_equal(args[["method"]], "POST")
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
    mockery::stub(sb_api_insert, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 409L,
        body = '{"message":"Conflict"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_insert("users", data.frame(id = 1L)))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
