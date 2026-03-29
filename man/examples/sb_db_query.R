\dontrun{
# Select specific columns
sb_db_query("users", columns = c("id", "name"))

# Filter with where clause
sb_db_query("users", where = list(status = "active"))

# Filter with operators
sb_db_query("orders", where = list(
  status = "pending",
  total = list(gte = 100)
))

# Available operators: eq, neq, gt, gte, lt, lte, like, ilike, in, is

# Raw SQL query
sb_db_query(sql = "SELECT COUNT(*) FROM users WHERE status = 'active'")
}
