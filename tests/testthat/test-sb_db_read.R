describe("sb_db_read()", {
  it("should error on invalid arguments", {
    expect_snapshot(sb_db_read(table = NULL), error = TRUE)
    expect_snapshot(sb_db_read(table = 123), error = TRUE)
    expect_snapshot(sb_db_read(table = "users", limit = "ten"), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_read(table = "users"), error = TRUE)
  })
})
