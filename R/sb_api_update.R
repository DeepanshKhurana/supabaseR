#' Update rows in a table via API
#'
#' @param table The table name
#' @param data A named list of column = value pairs to set
#' @param where A named list for filtering. Supports operators via nested lists.
#' @param schema The schema name
#' @return Number of rows updated (invisibly)
#' @example man/examples/sb_api_update.R
#' @export
sb_api_update <- function(
  table = NULL,
  data = NULL,
  where = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_list(data, min.len = 1),
    checkmate::check_list(where, min.len = 1),
    combine = "and"
  )

  params <- .build_api_params(where = where)
  params[["select"]] <- NULL

  response <- .sb_api_request(
    method = "PATCH",
    path = paste0("rest/v1/", table),
    params = params,
    body = data,
    prefer = c("count=exact", "return=minimal"),
    schema = schema
  )

  if (!httr2::resp_status(response) %in% c(200L, 204L)) {
    .sb_api_abort(response)
  }

  n <- .parse_count_header(response)
  cli::cli_alert_success("Updated {n} row{?s} in {.field {table}}")
  invisible(n)
}
