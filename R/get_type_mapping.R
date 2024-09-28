#' Get the mapping of R types to PostgreSQL types
#' @return A named list mapping R types to PostgreSQL types
get_type_mapping <- function() {
  list(
    "smallint" = "as.numeric",    # int2
    "integer" = "as.integer",     # int4
    "bigint" = "as.numeric",      # int8
    "real" = "as.numeric",        # float4
    "double precision" = "as.numeric", # float8
    "numeric" = "as.numeric",     # numeric
    "json" = "jsonlite::fromJSON", # json
    "jsonb" = "jsonlite::fromJSON", # jsonb
    "text" = "as.character",      # text
    "character varying" = "as.character", # varchar
    "uuid" = "as.character",      # uuid
    "date" = "as.Date",           # date
    "time without time zone" = "hms::as_hms", # time
    "time with time zone" = "hms::as_hms",    # timetz
    "timestamp without time zone" = "as.POSIXct", # timestamp
    "timestamp with time zone" = "as.POSIXct",    # timestamptz
    "boolean" = "as.logical"      # bool
  )
}
