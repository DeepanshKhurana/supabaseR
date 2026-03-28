#' Filter columns based on schema and operation type
#'
#' @param schema_info A data frame containing schema information.
#' @param is_update Logical indicating whether the operation is an update.
#' @return A vector of column names to include.
filter_columns <- function(
    schema_info,
    is_update = FALSE
) {
  schema_info$column_name[
    schema_info$column_name != "created_at"
  ]
}
