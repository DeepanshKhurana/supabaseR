#' Check if a table exists via API
#'
#' @param table The table name
#' @param schema The schema name
#' @return `TRUE` if the table exists, `FALSE` otherwise
#' @example man/examples/sb_api_table_exists.R
#' @export
sb_api_table_exists <- function(
  table = NULL,
  schema = get_schema()
) {
  .check_api_available()
  checkmate::assert_string(table)

  table %in% sb_api_tables(schema = schema)
}
