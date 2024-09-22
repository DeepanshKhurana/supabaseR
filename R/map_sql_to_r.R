#' Map SQL data types to R data types
#'
#' @param data_type SQL data type as a character string.
#' @param type_mapping A named list mapping SQL data types to R data types.
#' @return Corresponding R data type as a character string.
map_sql_to_r <- function(
  data_type,
  type_mapping = config::get("type_mapping")
) {
  matched_type <- type_mapping[[data_type]]
  if (is.null(matched_type)) {
    stop(glue::glue("Unrecognized data type: {data_type}"))
  }
  matched_type
}
