describe("sb_db_status()", {
  it("should return backend availability status", {
    # Arrange
    withr::local_envvar(SUPABASE_SCHEMA = NA)
    .sb_env$conn <- NULL
    .sb_env$schema <- NULL
    # Act and Assert
    expect_snapshot(sb_db_status())
  })
})
