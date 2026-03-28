describe("sb_db_insert()", {
  it("should error on invalid arguments", {
    expect_snapshot(sb_db_insert(table = NULL, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = 123, data = data.frame(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = "users", data = list(a = 1)), error = TRUE)
    expect_snapshot(sb_db_insert(table = "users", data = NULL), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_insert(table = "users", data = data.frame(name = "Alice")), error = TRUE)
  })
})
