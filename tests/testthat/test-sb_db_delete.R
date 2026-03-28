describe("sb_db_delete()", {
  it("should error on invalid arguments", {
    expect_snapshot(sb_db_delete(table = NULL, where = list(id = 1)), error = TRUE)
    expect_snapshot(sb_db_delete(table = "users", where = list()), error = TRUE)
    expect_snapshot(sb_db_delete(table = "users", where = NULL), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_delete(table = "users", where = list(id = 1)), error = TRUE)
  })
})
