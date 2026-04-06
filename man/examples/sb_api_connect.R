\dontrun{
# Connect using environment variables (SUPABASE_URL + SUPABASE_PUBLISHABLE_KEY)
sb_api_connect()

# Connect with a secret key only (bypasses Row Level Security)
sb_api_connect(
  url = "https://xxx.supabase.co",
  secret_key = "sb_secret_..."
)

# Connect with both publishable and secret keys
sb_api_connect(
  url = "https://xxx.supabase.co",
  key = "sb_publishable_...",
  secret_key = "sb_secret_..."
)

# Disconnect when done
sb_api_disconnect()
}
