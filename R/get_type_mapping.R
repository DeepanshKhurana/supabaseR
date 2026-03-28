#' Get the mapping of R types to PostgreSQL types
#' @return A named list mapping R types to PostgreSQL types
get_type_mapping <- function() {
  list(
    "smallint" = "as.numeric",
    "integer" = "as.integer",
    "bigint" = "as.numeric",
    "real" = "as.numeric",
    "double precision" = "as.numeric",
    "numeric" = "as.numeric",
    "json" = "as.character",
    "jsonb" = "as.character",
    "text" = "as.character",
    "character varying" = "as.character",
    "uuid" = "as.character",
    "date" = "as.Date",
    "time without time zone" = "as.character",
    "time with time zone" = "as.character",
    "timestamp without time zone" = "as.character",
    "timestamp with time zone" = "as.character",
    "boolean" = "as.logical"
  )
}
