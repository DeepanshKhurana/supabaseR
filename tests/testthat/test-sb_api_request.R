describe(".sb_api_request()", {
  setup_api <- function() {
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_test_key"
    .sb_env$api_secret_key <- NULL
  }

  it("should build a GET request and return mock response", {
    # Arrange
    setup_api()
    mockery::stub(
      .sb_api_request, "httr2::req_perform",
      function(req, ...) make_mock_response()
    )
    # Act
    resp <- .sb_api_request("GET", "rest/v1/users")
    # Assert
    expect_equal(resp$status_code, 200L)
  })

  it("should add apikey header for new-format key", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users")
    # Assert
    expect_true("apikey" %in% tolower(names(captured$headers)))
  })

  it("should add both apikey and Authorization headers for legacy JWT key", {
    # Arrange
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "eyJtestlegacykey"
    .sb_env$api_secret_key <- NULL
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users")
    # Assert
    header_names <- tolower(names(captured$headers))
    expect_true("apikey" %in% header_names)
    expect_true("authorization" %in% header_names)
  })

  it("should prefer secret_key over api_key when both set", {
    # Arrange
    .sb_env$api_url <- "https://test.supabase.co"
    .sb_env$api_key <- "sb_publishable_pubkey"
    .sb_env$api_secret_key <- "sb_secret_secretkey"
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users")
    # Assert
    expect_equal(captured$headers[["apikey"]], "sb_secret_secretkey")
    .sb_env$api_secret_key <- NULL
  })

  it("should add Accept-Profile header for non-public schema on GET", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users", schema = "myschema")
    # Assert
    expect_true("accept-profile" %in% tolower(names(captured$headers)))
  })

  it("should NOT add Accept-Profile header for public schema on GET", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users", schema = "public")
    # Assert
    expect_false("accept-profile" %in% tolower(names(captured$headers)))
  })

  it("should add Content-Profile header for non-public schema on POST", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("POST", "rest/v1/users", schema = "myschema")
    # Assert
    expect_true("content-profile" %in% tolower(names(captured$headers)))
  })

  it("should append query params to URL", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("GET", "rest/v1/users", params = list(select = "*", limit = 10))
    # Assert
    expect_match(captured$url, "select")
  })

  it("should add Prefer header when prefer is set", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("POST", "rest/v1/users",
                    prefer = c("count=exact", "return=minimal"))
    # Assert
    expect_true("prefer" %in% tolower(names(captured$headers)))
  })

  it("should add body when body is provided", {
    # Arrange
    setup_api()
    captured <- NULL
    mockery::stub(.sb_api_request, "httr2::req_perform", function(req, ...) {
      captured <<- req
      make_mock_response()
    })
    # Act
    .sb_api_request("POST", "rest/v1/users",
                    body = list(name = "Alice", age = 30L))
    # Assert
    expect_false(is.null(captured$body))
  })
})

describe(".parse_count_header()", {
  it("should return 0 for response with no Content-Range header", {
    # Arrange
    resp <- make_mock_response()
    # Act and Assert
    expect_equal(.parse_count_header(resp), 0L)
  })

  it("should parse integer count from */N format", {
    # Arrange
    resp <- make_mock_response(content_range = "*/42")
    # Act and Assert
    expect_equal(.parse_count_header(resp), 42L)
  })

  it("should return 0 for malformed Content-Range header", {
    # Arrange
    resp <- make_mock_response(content_range = "0-9/unknown")
    # Act and Assert
    expect_equal(.parse_count_header(resp), 0L)
  })

  it("should return 0 not NA for */non-numeric count", {
    # Arrange
    resp <- make_mock_response(content_range = "*/unknown")
    # Act
    result <- .parse_count_header(resp)
    # Assert
    expect_equal(result, 0L)
    expect_false(is.na(result))
  })

  it("should parse 0 rows correctly", {
    # Arrange
    resp <- make_mock_response(content_range = "*/0")
    # Act and Assert
    expect_equal(.parse_count_header(resp), 0L)
  })
})

describe(".sb_api_abort()", {
  it("should abort with status code and message from JSON body", {
    # Arrange
    resp <- make_mock_response(
      status_code = 400L,
      body = '{"message":"Bad Request","hint":"Check your query"}'
    )
    # Act and Assert
    expect_snapshot(.sb_api_abort(resp), error = TRUE)
  })

  it("should abort with plain string body when not JSON", {
    # Arrange
    resp <- make_mock_response(status_code = 500L, body = "Internal Server Error")
    resp$headers[["content-type"]] <- "text/plain"
    # Act and Assert
    expect_snapshot(.sb_api_abort(resp), error = TRUE)
  })
})
