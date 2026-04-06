\dontrun{
sb_api_connect()

# Insert a single row
sb_api_insert("users", data.frame(name = "Alice", email = "alice@example.com"))

# Insert multiple rows
new_users <- data.frame(
  name  = c("Bob", "Carol"),
  email = c("bob@example.com", "carol@example.com")
)
sb_api_insert("users", new_users)
}
