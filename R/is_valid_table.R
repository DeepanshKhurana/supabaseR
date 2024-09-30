#' Check if the table exists
#'
#' @param table_name The name of the table.
#' @param conn A database connection object.
#' @return TRUE if the table exists, FALSE otherwise.
is_valid_table <- function(
  table_name = NULL,
  schema = Sys.getenv("SUPABASE_SCHEMA"),
  conn = make_connection()
) {
  checkmate::assert_string(table_name)
  table_name %in% get_table_list(
    schema = schema,
    conn = conn
  )
}
