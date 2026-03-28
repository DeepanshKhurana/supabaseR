describe("sb_db_schema()", {
  it("should error on invalid table argument", {
    expect_snapshot(sb_db_schema(table = NULL), error = TRUE)
    expect_snapshot(sb_db_schema(table = 123), error = TRUE)
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_schema(table = "users"), error = TRUE)
  })
})
