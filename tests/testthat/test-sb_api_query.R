describe("sb_api_query()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_query("test_table"), error = TRUE)
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_query(table = NULL), error = TRUE)
    expect_snapshot(sb_api_query(table = 123), error = TRUE)
    expect_snapshot(sb_api_query(table = "users", limit = "ten"), error = TRUE)
  })
})
