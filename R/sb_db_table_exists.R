#' Check if table exists
#'
#' @param table The table name
#' @param schema The schema name
#' @return TRUE if table exists, FALSE otherwise
#' @export
sb_db_table_exists <- function(
  table = NULL,
  schema = get_schema()
) {
  checkmate::assert_string(table)
  table %in% sb_db_tables(schema = schema)
}
