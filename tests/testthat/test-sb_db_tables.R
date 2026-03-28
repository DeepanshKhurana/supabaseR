describe("sb_db_tables()", {
  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_tables(), error = TRUE)
  })
})
