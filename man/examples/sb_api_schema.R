\dontrun{
sb_api_connect()

# Get column names and types for a table
sb_api_schema("users")

# Get schema for a table in a non-default schema
sb_api_schema("orders", schema = "billing")
}
