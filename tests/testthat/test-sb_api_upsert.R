describe("sb_api_upsert()", {
  it("should error when API connection is not available", {
    .sb_env$api_available <- FALSE
    expect_snapshot(
      sb_api_upsert("users", data = data.frame(id = 1), conflict_columns = "id"),
      error = TRUE
    )
  })

  it("should error on invalid arguments", {
    .sb_env$api_available <- FALSE
    expect_snapshot(
      sb_api_upsert(table = NULL, data = data.frame(id = 1), conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_api_upsert(table = "users", data = list(id = 1), conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_api_upsert(table = "users", data = data.frame(id = 1), conflict_columns = NULL),
      error = TRUE
    )
  })
})
