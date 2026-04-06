# Connect to Supabase via REST API

At least one key must be supplied alongside the URL. When both `key` and
`secret_key` are available, the secret key is always preferred for
requests because it has higher privileges (bypasses Row Level Security).

## Usage

``` r
sb_api_connect(
  url = Sys.getenv("SUPABASE_URL"),
  key = Sys.getenv(if (nchar(Sys.getenv("SUPABASE_PUBLISHABLE_KEY")) > 0)
    "SUPABASE_PUBLISHABLE_KEY" else "SUPABASE_ANON_KEY"),
  secret_key = Sys.getenv(if (nchar(Sys.getenv("SUPABASE_SECRET_KEY")) > 0)
    "SUPABASE_SECRET_KEY" else "SUPABASE_ROLE_KEY")
)
```

## Arguments

- url:

  The Supabase URL (default: from SUPABASE_URL env var)

- key:

  The Supabase API key. Accepts the legacy `anon` JWT key or the new
  publishable key (`sb_publishable_...`). Optional when `secret_key` is
  provided. (default: from SUPABASE_PUBLISHABLE_KEY or SUPABASE_ANON_KEY
  env var)

- secret_key:

  The Supabase secret key. Accepts the legacy `service_role` JWT key or
  the new secret key (`sb_secret_...`). When present it is used for all
  requests, bypassing Row Level Security. (default: from
  SUPABASE_SECRET_KEY or SUPABASE_ROLE_KEY env var)

## Value

Invisible list with API credentials

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
