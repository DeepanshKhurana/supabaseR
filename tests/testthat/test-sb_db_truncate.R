describe("sb_db_truncate()", {
  it("should error on invalid arguments", {
    expect_snapshot(sb_db_truncate(table = NULL), error = TRUE)
    expect_snapshot(sb_db_truncate(table = 123), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_truncate(table = "temp_data"), error = TRUE)
  })
})
