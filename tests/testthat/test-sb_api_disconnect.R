describe("sb_api_disconnect()", {
  it("should clear all API credentials", {
    # Arrange
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test"
    .sb_env$api_secret_key <- "sb_secret_test"
    .sb_env$api_available <- TRUE
    # Act
    sb_api_disconnect()
    # Assert
    expect_null(.sb_env$api_url)
    expect_null(.sb_env$api_key)
    expect_null(.sb_env$api_secret_key)
    expect_false(isTRUE(.sb_env$api_available))
  })

  it("should return invisibly without error", {
    # Arrange
    .sb_env$api_url <- NULL
    .sb_env$api_key <- NULL
    .sb_env$api_secret_key <- NULL
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_no_error(sb_api_disconnect())
  })
})
