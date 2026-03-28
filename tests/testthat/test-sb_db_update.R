describe("sb_db_update()", {
  it("should error on invalid arguments", {
    expect_snapshot(
      sb_db_update(
        table = NULL,
        data = list(name = "Bob"),
        where = list(id = 1)
      ),
      error = TRUE
    )
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(),
        where = list(id = 1)
      ),
      error = TRUE
    )
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(name = "Bob"),
        where = list()
      ),
      error = TRUE
    )
  })

  it("should error when not connected", {
    .sb_env$conn <- NULL
    expect_snapshot(
      sb_db_update(
        table = "users",
        data = list(name = "Bob"),
        where = list(id = 1)
      ),
      error = TRUE
    )
  })
})
