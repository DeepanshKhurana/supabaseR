#' Get the mapping of R types to PostgreSQL types
#' @return A named list mapping R types to PostgreSQL types
get_type_mapping <- function() {
  list(
    int2 = "smallint",
    int4 = "integer",
    int8 = "bigint",
    float4 = "real",
    float8 = "double precision",
    numeric = "numeric",
    json = "json",
    jsonb = "jsonb",
    text = "text",
    varchar = "character varying",
    uuid = "uuid",
    date = "date",
    time = "time without time zone",
    timetz = "time with time zone",
    timestamp = "timestamp without time zone",
    timestamptz = "timestamp with time zone",
    bool = "boolean"
  )
}
