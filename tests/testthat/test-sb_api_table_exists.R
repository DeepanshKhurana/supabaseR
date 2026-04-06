describe("sb_api_table_exists()", {
  it("should error when API connection is not available", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_table_exists("users"), error = TRUE)
  })

  it("should error on invalid arguments", {
    # Arrange
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_api_table_exists(table = NULL), error = TRUE)
    expect_snapshot(sb_api_table_exists(table = 123), error = TRUE)
  })

  it("should return TRUE when table is in sb_api_tables result", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_table_exists, "sb_api_tables",
                  function(...) c("users", "posts"))
    # Act and Assert
    expect_true(sb_api_table_exists("users"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })

  it("should return FALSE when table is not in sb_api_tables result", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    mockery::stub(sb_api_table_exists, "sb_api_tables",
                  function(...) c("users", "posts"))
    # Act and Assert
    expect_false(sb_api_table_exists("nonexistent"))
    .sb_env$api_available <- FALSE
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
  })
})
