\dontrun{
# Update a row by id
sb_db_update(
  "users",
  data = list(email = "newemail@example.com"),
  where = list(id = 1)
)

# Update with operator
sb_db_update(
  "products",
  data = list(in_stock = FALSE),
  where = list(quantity = list(lte = 0))
)
}
