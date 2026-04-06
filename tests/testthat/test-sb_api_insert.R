describe("sb_api_insert()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_insert("users", data.frame(name = "Alice")), error = TRUE)
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(sb_api_insert(table = NULL, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = 123, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = "users", data = list(a = 1)), error = TRUE)
    expect_snapshot(sb_api_insert(table = "users", data = NULL), error = TRUE)
  })
})
