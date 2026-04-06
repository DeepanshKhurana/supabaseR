describe("sb_api_connect()", {
  it("should error when API credentials are not available", {
    withr::local_envvar(
      SUPABASE_URL = "",
      SUPABASE_ANON_KEY = "",
      SUPABASE_PUBLISHABLE_KEY = ""
    )
    expect_snapshot(sb_api_connect(), error = TRUE)
  })
})
