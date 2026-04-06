describe("sb_api_update()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(
      sb_api_update("users", data = list(name = "Bob"), where = list(id = 1)),
      error = TRUE
    )
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(
      sb_api_update(table = NULL, data = list(a = 1), where = list(id = 1)),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(), where = list(id = 1)),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(a = 1), where = list()),
      error = TRUE
    )
    expect_snapshot(
      sb_api_update(table = "users", data = list(a = 1), where = NULL),
      error = TRUE
    )
  })
})
