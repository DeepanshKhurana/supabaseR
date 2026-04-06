#' Read table data via API
#'
#' @param table The table name
#' @param limit Maximum rows to return (0 for all)
#' @param schema The schema name
#' @return A `tibble`
#' @example man/examples/sb_api_read.R
#' @export
sb_api_read <- function(
  table = NULL,
  limit = 0,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert(
    checkmate::check_string(table),
    checkmate::check_numeric(limit),
    combine = "and"
  )

  params <- .build_api_params(limit = limit)

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
