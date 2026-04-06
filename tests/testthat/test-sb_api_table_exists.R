describe("sb_api_table_exists()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_table_exists("users"), error = TRUE)
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_table_exists(table = NULL), error = TRUE)
    expect_snapshot(sb_api_table_exists(table = 123), error = TRUE)
  })
})
