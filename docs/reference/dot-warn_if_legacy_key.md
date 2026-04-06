# Detect and warn about legacy JWT-based Supabase API keys

Supabase is migrating from long-lived JWT keys (anon / service_role) to
a new key system with publishable (`sb_publishable_...`) and secret
(`sb_secret_...`) keys. Legacy keys begin with "eyJ" (base64-encoded
JSON).

## Usage

``` r
.warn_if_legacy_key(key, arg_name)
```

## Arguments

- key:

  The key string to check

- arg_name:

  The argument name to include in the warning message
