describe("sb_api_tables()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_tables(), error = TRUE)
  })
})
