describe("sb_db_status()", {
  it("should return backend availability status", {
    withr::local_envvar(SUPABASE_SCHEMA = NA)
    .sb_env$conn <- NULL
    .sb_env$schema <- NULL
    expect_snapshot(sb_db_status())
  })
})
