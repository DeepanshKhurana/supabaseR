describe("sb_db_status()", {
  it("should return backend availability status", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_status())
  })
})
