\dontrun{
sb_api_connect()

# Select specific columns
sb_api_query("users", columns = c("id", "name"))

# Filter with a simple equality condition
sb_api_query("users", where = list(status = "active"))

# Filter with operators
sb_api_query("orders", where = list(
  status = "pending",
  total = list(gte = 100)
))

# Available operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

# Limit results
sb_api_query("users", limit = 5)
}
