#' Query a table via API
#'
#' @param table The table name
#' @param columns Columns to select (default: all). Character vector or `"*"`.
#' @param where A named list for filtering. Supports operators via nested lists:
#'   `list(id = 1)` for equality, `list(age = list(gt = 25))` for `age > 25`.
#'   Operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is
#' @param limit Maximum rows to return (0 for all)
#' @param schema The schema name
#' @return A `tibble`
#' @example man/examples/sb_api_query.R
#' @export
sb_api_query <- function(
  table = NULL,
  columns = "*",
  where = NULL,
  limit = 0,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_numeric(limit),
    combine = "and"
  )

  params <- .build_api_params(columns = columns, where = where, limit = limit)

  response <- .sb_api_request(
    method = "GET",
    path = paste0("rest/v1/", table),
    params = params,
    schema = schema
  )

  if (httr2::resp_status(response) == 200) {
    response |>
      httr2::resp_body_json(simplifyVector = TRUE) |>
      dplyr::as_tibble()
  } else {
    .sb_api_abort(response)
  }
}
