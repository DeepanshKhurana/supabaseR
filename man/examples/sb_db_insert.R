\dontrun{
# Insert a single row
sb_db_insert("users", data.frame(name = "Alice", email = "alice@example.com"))

# Insert multiple rows
new_users <- data.frame(
  name = c("Bob", "Carol"),
  email = c("bob@example.com", "carol@example.com")
)
sb_db_insert("users", new_users)
}
