describe("sb_api_tables()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_tables(), error = TRUE)
  })

  it("should return character vector of table names from OpenAPI spec", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    spec_body <- jsonlite::toJSON(list(
      paths = list(
        `/users` = list(),
        `/posts` = list(),
        `/rpc/my_func` = list()
      )
    ), auto_unbox = TRUE)
    mockery::stub(sb_api_tables, ".sb_api_request",
                  function(...) make_mock_response(body = spec_body))
    # Act
    result <- sb_api_tables()
    # Assert
    expect_type(result, "character")
    expect_true("users" %in% result)
    expect_true("posts" %in% result)
    expect_false("rpc/my_func" %in% result)
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on non-200 response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_tables, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 503L,
        body = '{"message":"Service Unavailable"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_tables())
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
