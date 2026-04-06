#' Check that API credentials are available
#'
#' @keywords internal
.check_api_available <- function() {
  if (!isTRUE(.sb_env$api_available)) {
    cli::cli_abort(
      c(
        "x" = "API credentials not available.",
        "i" = "Call {.fn sb_api_connect} or set {.envvar SUPABASE_URL} and at least one
               of {.envvar SUPABASE_PUBLISHABLE_KEY}, {.envvar SUPABASE_ANON_KEY},
               {.envvar SUPABASE_SECRET_KEY}, or {.envvar SUPABASE_ROLE_KEY}."
      ),
      call. = FALSE
    )
  }
}

#' Make a PostgREST API request
#'
#' @param method HTTP method: "GET", "POST", "PATCH", or "DELETE"
#' @param path URL path to append after the base URL (e.g. "rest/v1/mytable")
#' @param params Named list of URL query parameters
#' @param body Request body (list or data frame, JSON-encoded)
#' @param prefer Character vector of Prefer header values
#' @param schema Schema name; triggers Accept-Profile (GET) or Content-Profile
#'   (mutations) header when not "public"
#' @return An httr2 response object
#' @keywords internal
.sb_api_request <- function(
  method = "GET",
  path,
  params = NULL,
  body = NULL,
  prefer = NULL,
  schema = "public"
) {
  api_key <- .sb_env$api_key
  secret_key <- .sb_env$api_secret_key
  active_key <- if (!is.null(secret_key)) secret_key else api_key

  req <- httr2::request(.sb_env$api_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_method(method)

  if (startsWith(active_key, "eyJ")) {
    req <- req |>
      httr2::req_headers(
        apikey = active_key,
        Authorization = paste("Bearer", active_key)
      )
  } else {
    req <- req |>
      httr2::req_headers(apikey = active_key)
  }

  if (!is.null(schema) && schema != "public") {
    if (method == "GET") {
      req <- req |> httr2::req_headers(`Accept-Profile` = schema)
    } else {
      req <- req |> httr2::req_headers(`Content-Profile` = schema)
    }
  }

  if (!is.null(params) && length(params) > 0) {
    req <- req |> httr2::req_url_query(!!!params)
  }

  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }

  if (!is.null(prefer)) {
    req <- req |>
      httr2::req_headers(Prefer = paste(prefer, collapse = ","))
  }

  req |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
}

#' Build PostgREST URL query parameters from a where list
#'
#' @param columns Columns to select: "*" or a character vector
#' @param where Named list of filter conditions; same operator vocabulary as
#'   [build_where()]: bare value for equality, or `list(op = val)` for operators
#'   `eq`, `neq`, `gt`, `gte`, `lt`, `lte`, `like`, `ilike`, `in`, `is`
#' @param limit Maximum rows to return; 0 means no limit
#' @return Named list suitable for `httr2::req_url_query(!!!params)`
#' @keywords internal
.build_api_params <- function(
  columns = "*",
  where = NULL,
  limit = 0
) {
  select <- if (length(columns) == 1 && columns == "*") {
    "*"
  } else {
    paste(columns, collapse = ",")
  }

  params <- list(select = select)

  if (!is.null(where) && length(where) > 0) {
    for (col in names(where)) {
      val <- where[[col]]
      if (is.list(val) && length(val) == 1) {
        op <- names(val)[[1]]
        op_val <- val[[1]]
        if (op == "in") {
          params[[col]] <- paste0("in.(", paste(op_val, collapse = ","), ")")
        } else {
          params[[col]] <- paste0(op, ".", op_val)
        }
      } else {
        params[[col]] <- paste0("eq.", val)
      }
    }
  }

  if (limit > 0) {
    params[["limit"]] <- limit
  }

  params
}

#' Parse the affected-row count from a PostgREST Content-Range header
#'
#' @param response An httr2 response object
#' @return Integer row count, or 0L if the header is absent or unparseable
#' @keywords internal
.parse_count_header <- function(response) {
  cr <- httr2::resp_header(response, "Content-Range")
  if (is.null(cr) || !grepl("^\\*/", cr)) return(0L)
  as.integer(sub("^\\*/", "", cr))
}

#' Abort with a structured PostgREST error message
#'
#' @param response An httr2 response object with a non-2xx status
#' @keywords internal
.sb_api_abort <- function(response) {
  body <- tryCatch(
    httr2::resp_body_json(response),
    error = \(e) list(message = httr2::resp_body_string(response))
  )
  cli::cli_abort(
    c(
      "x" = "API request failed with status {httr2::resp_status(response)}.",
      "!" = "{body$message}",
      if (!is.null(body$hint)) c("i" = "{body$hint}") else NULL
    ),
    call. = FALSE
  )
}

#' Build WHERE clause from list
#'
#' @param where A named list of conditions
#' @param conn Database connection for escaping
#' @param include_keyword Include "WHERE" keyword (default TRUE)
#' @return SQL string for WHERE clause
#' @details
#' Supports operators via nested lists:
#' - `list(id = 1)` → `id = 1`
#' - `list(age = list(gt = 25))` → `age > 25`
#' - `list(name = list(like = "A%"))` → `name LIKE 'A%'`
#'
#' Supported operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is
#' @keywords internal
build_where <- function(
  where,
  conn,
  include_keyword = TRUE
) {
  if (is.null(where) || length(where) == 0) {
    return(DBI::SQL(""))
  }

  operators <- list(
    eq = "=",
    neq = "<>",
    gt = ">",
    gte = ">=",
    lt = "<",
    lte = "<=",
    like = "LIKE",
    ilike = "ILIKE",
    "in" = "IN",
    is = "IS"
  )

  conditions <- mapply(
    function(col, val) {
      if (is.list(val) && length(val) == 1) {
        op_name <- names(val)[1]
        op_val <- val[[1]]
        op_sql <- operators[[op_name]]

        if (is.null(op_sql)) {
          stop(glue::glue("Unknown operator: {op_name}"))
        }

        if (op_name == "in") {
          vals <- glue::glue_sql("{op_val*}", .con = conn)
          glue::glue_sql("{`col`} IN ({vals})", .con = conn)
        } else if (op_name == "is") {
          glue::glue_sql("{`col`} IS {DBI::SQL(op_val)}", .con = conn)
        } else {
          glue::glue_sql("{`col`} {DBI::SQL(op_sql)} {op_val}", .con = conn)
        }
      } else {
        glue::glue_sql("{`col`} = {val}", .con = conn)
      }
    },
    names(where),
    where,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  conditions_sql <- DBI::SQL(glue::glue_collapse(conditions, sep = " AND "))
  if (include_keyword) {
    glue::glue_sql("WHERE {conditions_sql}", .con = conn)
  } else {
    conditions_sql
  }
}
