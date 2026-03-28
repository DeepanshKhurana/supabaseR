describe("build_where()", {
  mock_conn <- DBI::ANSI()

  it("should return empty SQL for NULL or empty where", {
    expect_equal(as.character(build_where(NULL, mock_conn)), "")
    expect_equal(as.character(build_where(list(), mock_conn)), "")
  })

  it("should build equality condition", {
    result <- as.character(build_where(list(id = 1), mock_conn))
    expect_match(result, "WHERE")
    expect_match(result, "id")
  })

  it("should build gt operator", {
    result <- as.character(build_where(list(age = list(gt = 18)), mock_conn))
    expect_match(result, ">")
    expect_match(result, "18")
  })

  it("should build lt operator", {
    result <- as.character(build_where(list(age = list(lt = 18)), mock_conn))
    expect_match(result, "<")
  })

  it("should build gte operator", {
    result <- as.character(build_where(list(age = list(gte = 18)), mock_conn))
    expect_match(result, ">=")
  })

  it("should build lte operator", {
    result <- as.character(build_where(list(age = list(lte = 18)), mock_conn))
    expect_match(result, "<=")
  })

  it("should build neq operator", {
    result <- as.character(build_where(list(status = list(neq = "inactive")), mock_conn))
    expect_match(result, "<>")
  })

  it("should build like operator", {
    result <- as.character(build_where(list(name = list(like = "A%")), mock_conn))
    expect_match(result, "LIKE")
  })

  it("should build ilike operator", {
    result <- as.character(build_where(list(name = list(ilike = "a%")), mock_conn))
    expect_match(result, "ILIKE")
  })

  it("should build IN operator", {
    result <- as.character(build_where(list(id = list("in" = c(1, 2, 3))), mock_conn))
    expect_match(result, "IN")
  })

  it("should build IS operator", {
    result <- as.character(build_where(list(deleted_at = list(is = "NULL")), mock_conn))
    expect_match(result, "IS")
    expect_match(result, "NULL")
  })

  it("should combine multiple conditions with AND", {
    result <- as.character(build_where(list(a = 1, b = 2), mock_conn))
    expect_match(result, "AND")
  })

  it("should error on unknown operator", {
    expect_snapshot(build_where(list(x = list(unknown = 1)), mock_conn), error = TRUE)
  })

  it("should omit WHERE keyword when include_keyword = FALSE", {
    result <- as.character(build_where(list(id = 1), mock_conn, include_keyword = FALSE))
    expect_false(grepl("WHERE", result))
  })
})
