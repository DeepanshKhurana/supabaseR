describe("sb_db_connect()", {
  it("should error when DBI credentials are not available", {
    .sb_env$dbi_available <- FALSE
    expect_snapshot(sb_db_connect(), error = TRUE)
  })
})

describe("sb_db_disconnect()", {
  it("should handle disconnect when not connected", {
    .sb_env$conn <- NULL
    expect_silent(sb_db_disconnect())
  })
})

describe("get_connection()", {
  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(get_connection(), error = TRUE)
  })
})

describe("get_schema()", {
  it("should return schema from environment or stored value", {
    .sb_env$schema <- NULL
    withr::local_envvar(SUPABASE_SCHEMA = "public")
    expect_equal(get_schema(), "public")

    .sb_env$schema <- "custom_schema"
    expect_equal(get_schema(), "custom_schema")
    .sb_env$schema <- NULL
  })
})
