\dontrun{
sb_api_connect()

# Update a row by id
sb_api_update(
  "users",
  data  = list(email = "newemail@example.com"),
  where = list(id = 1)
)

# Update with an operator
sb_api_update(
  "products",
  data  = list(in_stock = FALSE),
  where = list(quantity = list(lte = 0))
)
}
