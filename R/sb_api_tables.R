#' List tables via API
#'
#' Queries the PostgREST OpenAPI spec to list exposed tables.
#'
#' @param schema The schema name
#' @return A character vector of table names
#' @example man/examples/sb_api_tables.R
#' @export
sb_api_tables <- function(
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert_string(schema)

  response <- .sb_api_request(
    method = "GET",
    path = "rest/v1/",
    schema = schema
  )

  if (httr2::resp_status(response) != 200) {
    .sb_api_abort(response)
  }

  spec <- httr2::resp_body_json(response)
  paths <- names(spec$paths)

  table_paths <- paths[
    grepl("^/[^/]+$", paths) & !grepl("^/rpc/", paths)
  ]

  sub("^/", "", table_paths)
}
