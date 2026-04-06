describe("get_backend()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(get_backend(), error = TRUE)
  })
})

describe("sb_connect()", {
  it("should error when no credentials available in auto mode", {
    # Arrange
    .sb_env$dbi_available <- FALSE
    .sb_env$api_available <- FALSE
    # Act and Assert
    expect_snapshot(sb_connect(), error = TRUE)
  })

  it("should dispatch to API when backend='api'", {
    # Arrange
    .sb_env$api_available <- TRUE
    mockery::stub(sb_connect, "sb_api_connect", function(...) invisible(NULL))
    # Act
    sb_connect(backend = "api")
    # Assert
    expect_equal(.sb_env$backend, "api")
    .sb_env$backend <- NULL
    .sb_env$api_available <- FALSE
  })

  it("should auto-detect and dispatch to db when dbi_available", {
    # Arrange
    .sb_env$dbi_available <- TRUE
    .sb_env$api_available <- FALSE
    mockery::stub(sb_connect, "sb_db_connect", function(...) invisible(NULL))
    # Act
    sb_connect()
    # Assert
    expect_equal(.sb_env$backend, "db")
    .sb_env$backend <- NULL
    .sb_env$dbi_available <- FALSE
  })

  it("should auto-detect and dispatch to api when only api_available", {
    # Arrange
    .sb_env$dbi_available <- FALSE
    .sb_env$api_available <- TRUE
    mockery::stub(sb_connect, "sb_api_connect", function(...) invisible(NULL))
    # Act
    sb_connect()
    # Assert
    expect_equal(.sb_env$backend, "api")
    .sb_env$backend <- NULL
    .sb_env$api_available <- FALSE
  })
})

describe("sb_disconnect()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_disconnect(), error = TRUE)
  })

  it("should clear backend after API disconnect", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_disconnect, "sb_api_disconnect", function() invisible(NULL))
    # Act
    sb_disconnect()
    # Assert
    expect_null(.sb_env$backend)
  })

  it("should clear backend after DB disconnect", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_disconnect, "sb_db_disconnect", function() invisible(NULL))
    # Act
    sb_disconnect()
    # Assert
    expect_null(.sb_env$backend)
  })
})

describe("sb_status()", {
  it("should return list with correct structure", {
    # Arrange
    .sb_env$dbi_available <- FALSE
    .sb_env$api_available <- FALSE
    .sb_env$conn <- NULL
    .sb_env$backend <- NULL
    .sb_env$schema <- NULL
    withr::local_envvar(SUPABASE_SCHEMA = "public")
    # Act
    status <- sb_status()
    # Assert
    expect_named(status, c("dbi", "api", "backend", "schema"))
    expect_named(status$dbi, c("available", "connected"))
    expect_named(status$api, c("available", "connected"))
    expect_false(status$dbi$available)
    expect_false(status$api$available)
    expect_null(status$backend)
  })

  it("should reflect connected API state", {
    # Arrange
    .sb_env$api_available <- TRUE
    .sb_env$backend <- "api"
    # Act
    status <- sb_status()
    # Assert
    expect_true(status$api$available)
    expect_equal(status$backend, "api")
    .sb_env$api_available <- FALSE
    .sb_env$backend <- NULL
  })
})

describe("sb_read()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_read("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_read, "sb_api_read", function(...) data.frame(id = 1L))
    # Act
    result <- sb_read("users")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_read, "sb_db_read", function(...) data.frame(id = 2L))
    # Act
    result <- sb_read("users")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })
})

describe("sb_insert()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_insert("t", data.frame()), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_insert, "sb_api_insert", function(...) invisible(1L))
    # Act
    result <- sb_insert("users", data.frame(name = "test"))
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_insert, "sb_db_insert", function(...) invisible(1L))
    # Act
    result <- sb_insert("users", data.frame(name = "test"))
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })
})

describe("sb_update()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_update("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_update, "sb_api_update", function(...) invisible(1L))
    # Act
    result <- sb_update("users", data = list(name = "new"))
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_update, "sb_db_update", function(...) invisible(1L))
    # Act
    result <- sb_update("users", data = list(name = "new"))
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })
})

describe("sb_delete()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_delete("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_delete, "sb_api_delete", function(...) invisible(1L))
    # Act
    result <- sb_delete("users")
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_delete, "sb_db_delete", function(...) invisible(1L))
    # Act
    result <- sb_delete("users")
    # Assert
    expect_equal(result, 1L)
    .sb_env$backend <- NULL
  })
})

describe("sb_upsert()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_upsert("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_upsert, "sb_api_upsert", function(...) invisible(2L))
    # Act
    result <- sb_upsert("users", data.frame(id = 1L))
    # Assert
    expect_equal(result, 2L)
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_upsert, "sb_db_upsert", function(...) invisible(2L))
    # Act
    result <- sb_upsert("users", data.frame(id = 1L))
    # Assert
    expect_equal(result, 2L)
    .sb_env$backend <- NULL
  })
})

describe("sb_query()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_query("t"), error = TRUE)
  })

  it("should error when sql provided with API backend", {
    # Arrange
    .sb_env$backend <- "api"
    .sb_env$api_available <- TRUE
    # Act and Assert
    expect_snapshot(sb_query(table = "t", sql = "SELECT 1"), error = TRUE)
    .sb_env$backend <- NULL
    .sb_env$api_available <- FALSE
  })

  it("should dispatch to API backend without sql", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_query, "sb_api_query", function(...) data.frame(id = 1L))
    # Act
    result <- sb_query("users")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend with sql", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_query, "sb_db_query", function(...) data.frame(id = 1L))
    # Act
    result <- sb_query(sql = "SELECT 1")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })
})

describe("sb_tables()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_tables(), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_tables, "sb_api_tables", function(...) c("users", "posts"))
    # Act
    result <- sb_tables()
    # Assert
    expect_equal(result, c("users", "posts"))
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_tables, "sb_db_tables", function(...) c("users"))
    # Act
    result <- sb_tables()
    # Assert
    expect_equal(result, c("users"))
    .sb_env$backend <- NULL
  })
})

describe("sb_schema()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_schema("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_schema, "sb_api_schema", function(...) data.frame(col = "id"))
    # Act
    result <- sb_schema("users")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_schema, "sb_db_schema", function(...) data.frame(col = "id"))
    # Act
    result <- sb_schema("users")
    # Assert
    expect_s3_class(result, "data.frame")
    .sb_env$backend <- NULL
  })
})

describe("sb_table_exists()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_table_exists("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_table_exists, "sb_api_table_exists", function(...) TRUE)
    # Act
    result <- sb_table_exists("users")
    # Assert
    expect_true(result)
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_table_exists, "sb_db_table_exists", function(...) FALSE)
    # Act
    result <- sb_table_exists("nonexistent")
    # Assert
    expect_false(result)
    .sb_env$backend <- NULL
  })
})

describe("sb_truncate()", {
  it("should error when not connected", {
    # Arrange
    .sb_env$backend <- NULL
    # Act and Assert
    expect_snapshot(sb_truncate("t"), error = TRUE)
  })

  it("should dispatch to API backend", {
    # Arrange
    .sb_env$backend <- "api"
    mockery::stub(sb_truncate, "sb_api_truncate", function(...) invisible(NULL))
    # Act and Assert
    expect_silent(sb_truncate("users"))
    .sb_env$backend <- NULL
  })

  it("should dispatch to DBI backend", {
    # Arrange
    .sb_env$backend <- "db"
    mockery::stub(sb_truncate, "sb_db_truncate", function(...) invisible(NULL))
    # Act and Assert
    expect_silent(sb_truncate("users"))
    .sb_env$backend <- NULL
  })
})
