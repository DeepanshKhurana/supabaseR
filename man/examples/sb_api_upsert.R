\dontrun{
sb_api_connect()

# Upsert a single row (insert or update on conflict)
sb_api_upsert(
  "users",
  data.frame(id = 1, name = "Alice Updated"),
  conflict_columns = "id"
)

# Upsert multiple rows with a composite key
sb_api_upsert(
  "orders",
  data.frame(
    user_id  = c(1, 2),
    order_id = c(10, 11),
    status   = c("shipped", "pending")
  ),
  conflict_columns = c("user_id", "order_id")
)
}
