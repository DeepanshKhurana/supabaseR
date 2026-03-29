\dontrun{
# Read all rows from a table
users <- sb_db_read("users")

# Read with a row limit
recent <- sb_db_read("orders", limit = 100)

# Read from a specific schema
products <- sb_db_read("products", schema = "inventory")
}
