describe("sb_api_status()", {
  it("should return connected=FALSE when API not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    # Act
    status <- sb_api_status()
    # Assert
    expect_false(status$connected)
    expect_null(status$url)
  })

  it("should return connected=TRUE with url when API available", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    # Act
    status <- sb_api_status()
    # Assert
    expect_true(status$connected)
    expect_equal(status$url, "https://test.supabase.co")
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
  })

  it("should return list with correct names including schema", {
    # Arrange
    withr::local_envvar(SUPABASE_SCHEMA = "myschema")
    .sb_env$schema <- NULL
    .sb_env$api_available <- FALSE
    # Act
    status <- sb_api_status()
    # Assert
    expect_named(status, c("connected", "url", "schema"))
    expect_equal(status$schema, "myschema")
  })
})
