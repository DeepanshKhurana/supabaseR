describe("sb_api_connect()", {
  it("should error when API credentials are not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_connect(), error = TRUE)
  })
})
