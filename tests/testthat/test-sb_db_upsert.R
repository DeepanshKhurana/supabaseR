describe("sb_db_upsert()", {
  it("should error on invalid arguments", {
    expect_snapshot(
      sb_db_upsert(table = NULL, data = data.frame(a = 1), conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_db_upsert(table = "users", data = NULL, conflict_columns = "id"),
      error = TRUE
    )
    expect_snapshot(
      sb_db_upsert(table = "users", data = data.frame(a = 1), conflict_columns = NULL),
      error = TRUE
    )
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(
      sb_db_upsert(
        table = "users",
        data = data.frame(id = 1, name = "Alice"),
        conflict_columns = "id"
      ),
      error = TRUE
    )
  })
})
