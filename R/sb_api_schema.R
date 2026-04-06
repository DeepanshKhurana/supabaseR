#' Get table schema via API
#'
#' Queries the PostgREST OpenAPI spec to retrieve column names and types for a
#' table.
#'
#' @param table The table name
#' @param schema The schema name
#' @return A `tibble` with columns `column_name` and `data_type`
#' @example man/examples/sb_api_schema.R
#' @export
sb_api_schema <- function(
  table = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert_string(table)

  response <- .sb_api_request(
    method = "GET",
    path = "rest/v1/",
    schema = schema
  )

  if (httr2::resp_status(response) != 200) {
    .sb_api_abort(response)
  }

  spec <- httr2::resp_body_json(response)

  props <- spec$components$schemas[[table]]$properties %||%
    spec$definitions[[table]]$properties

  if (is.null(props)) {
    cli::cli_abort(
      c(
        "x" = "Table {.field {table}} not found in API schema.",
        "i" = "Check that the table is exposed via PostgREST and the schema is correct."
      ),
      call. = FALSE
    )
  }

  dplyr::tibble(
    column_name = names(props),
    data_type = vapply(props, function(p) {
      if (!is.null(p$format)) p$format else p$type %||% NA_character_
    }, character(1))
  )
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
