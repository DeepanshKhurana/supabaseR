# Read Supabase credentials from environment variables

Connection parameters should be obtained from the Connect tab in your
Supabase Dashboard (Project Settings \> Database \> Connection Strings).
All three connection modes work (Direct, Transaction Pooler, Session
Pooler), but if Direct connection fails, try Transaction or Session
Pooler modes. Be sure to use the correct host, port, user, and password
for your chosen mode. Port defaults to 6543 if not specified.

## Usage

``` r
sb_db_creds()
```

## Value

A list of Supabase credentials
