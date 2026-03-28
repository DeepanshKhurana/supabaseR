describe("sb_db_query()", {
  it("should error when not connected with raw SQL", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_query(sql = "SELECT * FROM users"), error = TRUE)
  })

  it("should error when not connected with structured query", {
    .sb_env$conn <- NULL
    expect_snapshot(sb_db_query(table = "users"), error = TRUE)
  })

  it("should accept columns, where, and limit parameters", {
    .sb_env$conn <- NULL
    # All these should fail on connection, not validation
    expect_snapshot(
      sb_db_query(
        table = "orders",
        columns = c("id", "total"),
        where = list(user_id = 42),
        limit = 5
      ),
      error = TRUE
    )
  })
})
