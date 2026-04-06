#' Delete rows from a table via API
#'
#' @param table The table name
#' @param where A named list for filtering. Supports operators via nested lists.
#' @param schema The schema name
#' @return Number of rows deleted (invisibly)
#' @example man/examples/sb_api_delete.R
#' @export
sb_api_delete <- function(
  table = NULL,
  where = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_list(where, min.len = 1),
    combine = "and"
  )

  params <- .build_api_params(where = where)
  params[["select"]] <- NULL

  response <- .sb_api_request(
    method = "DELETE",
    path = paste0("rest/v1/", table),
    params = params,
    prefer = c("count=exact", "return=minimal"),
    schema = schema
  )

  if (!httr2::resp_status(response) %in% c(200L, 204L)) {
    .sb_api_abort(response)
  }

  n <- .parse_count_header(response)
  cli::cli_alert_success("Deleted {n} row{?s} from {.field {table}}")
  invisible(n)
}
