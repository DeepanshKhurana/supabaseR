describe("build_where()", {
  mock_conn <- DBI::ANSI()

  it("should return empty SQL for NULL or empty where", {
    # Arrange, Act and Assert
    expect_equal(as.character(build_where(NULL, mock_conn)), "")
    expect_equal(as.character(build_where(list(), mock_conn)), "")
  })

  it("should build equality condition", {
    # Act
    result <- as.character(build_where(list(id = 1), mock_conn))
    # Assert
    expect_match(result, "WHERE")
    expect_match(result, "id")
  })

  it("should build gt operator", {
    # Act
    result <- as.character(build_where(list(age = list(gt = 18)), mock_conn))
    # Assert
    expect_match(result, ">")
    expect_match(result, "18")
  })

  it("should build lt operator", {
    # Act
    result <- as.character(build_where(list(age = list(lt = 18)), mock_conn))
    # Assert
    expect_match(result, "<")
  })

  it("should build gte operator", {
    # Act
    result <- as.character(build_where(list(age = list(gte = 18)), mock_conn))
    # Assert
    expect_match(result, ">=")
  })

  it("should build lte operator", {
    # Act
    result <- as.character(build_where(list(age = list(lte = 18)), mock_conn))
    # Assert
    expect_match(result, "<=")
  })

  it("should build neq operator", {
    # Act
    result <- as.character(build_where(list(status = list(neq = "inactive")), mock_conn))
    # Assert
    expect_match(result, "<>")
  })

  it("should build like operator", {
    # Act
    result <- as.character(build_where(list(name = list(like = "A%")), mock_conn))
    # Assert
    expect_match(result, "LIKE")
  })

  it("should build ilike operator", {
    # Act
    result <- as.character(build_where(list(name = list(ilike = "a%")), mock_conn))
    # Assert
    expect_match(result, "ILIKE")
  })

  it("should build IN operator", {
    # Act
    result <- as.character(build_where(list(id = list("in" = c(1, 2, 3))), mock_conn))
    # Assert
    expect_match(result, "IN")
  })

  it("should build IS operator", {
    # Act
    result <- as.character(build_where(list(deleted_at = list(is = "NULL")), mock_conn))
    # Assert
    expect_match(result, "IS")
    expect_match(result, "NULL")
  })

  it("should combine multiple conditions with AND", {
    # Act
    result <- as.character(build_where(list(a = 1, b = 2), mock_conn))
    # Assert
    expect_match(result, "AND")
  })

  it("should error on unknown operator", {
    # Act and Assert
    expect_snapshot(build_where(list(x = list(unknown = 1)), mock_conn), error = TRUE)
  })

  it("should omit WHERE keyword when include_keyword = FALSE", {
    # Act
    result <- as.character(build_where(list(id = 1), mock_conn, include_keyword = FALSE))
    # Assert
    expect_false(grepl("WHERE", result))
  })
})

describe(".build_api_params()", {
  it("should return select=* by default", {
    # Act
    result <- .build_api_params()
    # Assert
    expect_equal(result$select, "*")
    expect_null(result$limit)
  })

  it("should join column vector into comma-separated select", {
    # Act
    result <- .build_api_params(columns = c("id", "name", "email"))
    # Assert
    expect_equal(result$select, "id,name,email")
  })

  it("should produce eq. prefix for bare value", {
    # Act
    result <- .build_api_params(where = list(id = 1))
    # Assert
    expect_equal(result$id, "eq.1")
  })

  it("should produce op. prefix for operator", {
    # Act and Assert
    expect_equal(.build_api_params(where = list(age = list(gt = 25)))$age, "gt.25")
    expect_equal(.build_api_params(where = list(age = list(gte = 18)))$age, "gte.18")
    expect_equal(.build_api_params(where = list(age = list(lt = 65)))$age, "lt.65")
    expect_equal(.build_api_params(where = list(age = list(lte = 100)))$age, "lte.100")
    expect_equal(
      .build_api_params(where = list(status = list(neq = "inactive")))$status,
      "neq.inactive"
    )
    expect_equal(.build_api_params(where = list(name = list(like = "A%")))$name, "like.A%")
    expect_equal(.build_api_params(where = list(name = list(ilike = "a%")))$name, "ilike.a%")
    expect_equal(
      .build_api_params(where = list(deleted_at = list(is = "null")))$deleted_at,
      "is.null"
    )
  })

  it("should produce in.(v1,v2,v3) format for in operator", {
    # Act
    result <- .build_api_params(where = list(id = list("in" = c(1, 2, 3))))
    # Assert
    expect_equal(result$id, "in.(1,2,3)")
  })

  it("should add limit param when limit > 0", {
    # Act
    result <- .build_api_params(limit = 10)
    # Assert
    expect_equal(result$limit, 10)
  })

  it("should omit limit param when limit == 0", {
    # Act
    result <- .build_api_params(limit = 0)
    # Assert
    expect_null(result$limit)
  })

  it("should include multiple where conditions as separate params", {
    # Act
    result <- .build_api_params(where = list(a = 1, b = 2))
    # Assert
    expect_equal(result$a, "eq.1")
    expect_equal(result$b, "eq.2")
  })
})
