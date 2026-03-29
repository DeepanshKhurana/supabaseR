\dontrun{
# Upsert: insert or update on conflict
sb_db_upsert(
  "users",
  data = data.frame(id = 1, name = "Alice", email = "alice@new.com"),
  conflict_columns = "id"
)

# Upsert multiple rows with composite key
sb_db_upsert(
  "order_items",
  data = data.frame(
    order_id = c(1, 1),
    product_id = c(10, 20),
    quantity = c(5, 3)
  ),
  conflict_columns = c("order_id", "product_id")
)
}
