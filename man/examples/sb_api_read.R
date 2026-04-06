\dontrun{
sb_api_connect()

# Read all rows
sb_api_read("users")

# Read with a row limit
sb_api_read("users", limit = 10)

# Read from a non-default schema
sb_api_read("orders", schema = "billing")
}
