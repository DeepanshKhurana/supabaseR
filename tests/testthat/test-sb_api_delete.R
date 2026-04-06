describe("sb_api_delete()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_delete("users", where = list(id = 1)), error = TRUE)
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_delete(table = NULL, where = list(id = 1)), error = TRUE)
    expect_snapshot(sb_api_delete(table = "users", where = list()), error = TRUE)
    expect_snapshot(sb_api_delete(table = "users", where = NULL), error = TRUE)
  })
})
