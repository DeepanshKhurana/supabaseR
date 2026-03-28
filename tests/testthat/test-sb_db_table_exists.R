describe("sb_db_table_exists()", {
  it("should error on invalid table argument", {
    expect_snapshot(sb_db_table_exists(table = NULL), error = TRUE)
    expect_snapshot(sb_db_table_exists(table = 123), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_table_exists(table = "users"), error = TRUE)
  })
})
