describe("sb_api_schema()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_schema("users"), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_schema(table = NULL), error = TRUE)
    expect_snapshot(sb_api_schema(table = 123), error = TRUE)
  })

  it("should return tibble with column_name and data_type from OpenAPI spec", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    spec_body <- jsonlite::toJSON(list(
      components = list(
        schemas = list(
          users = list(
            properties = list(
              id = list(type = "integer", format = "int4"),
              name = list(type = "string")
            )
          )
        )
      ),
      definitions = list(),
      paths = list()
    ), auto_unbox = TRUE)
    mockery::stub(sb_api_schema, ".sb_api_request",
                  function(...) make_mock_response(body = spec_body))
    # Act
    result <- sb_api_schema("users")
    # Assert
    expect_s3_class(result, "tbl_df")
    expect_named(result, c("column_name", "data_type"))
    expect_equal(sort(result$column_name), c("id", "name"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should error when table not in API spec", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    spec_body <- jsonlite::toJSON(list(
      components = list(schemas = list()),
      definitions = list(),
      paths = list()
    ), auto_unbox = TRUE)
    mockery::stub(sb_api_schema, ".sb_api_request",
                  function(...) make_mock_response(body = spec_body))
    # Act and Assert
    expect_error(sb_api_schema("nonexistent"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should call .sb_api_abort on non-200 response", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_schema, ".sb_api_request", function(...) {
      make_mock_response(
        status_code = 503L,
        body = '{"message":"Service Unavailable"}'
      )
    })
    # Act and Assert
    expect_error(sb_api_schema("users"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
