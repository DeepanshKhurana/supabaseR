# supabaseR

<!-- badges: start -->
[![R-CMD-check](https://github.com/deepanshkhurana/supabaseR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/deepanshkhurana/supabaseR/actions/workflows/R-CMD-check.yml)
[![pkgdown](https://github.com/deepanshkhurana/supabaseR/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/deepanshkhurana/supabaseR/actions/workflows/pkgdown.yml)
[![CANDOR: copilot](https://img.shields.io/badge/䷼%20CANDOR-copilot-fee2e2?labelColor=fee2e2)](https://candor.md)
<!-- badges: end -->

The unofficial R package for Supabase. It supports a **dual-backend** design: a DBI/PostgreSQL backend for direct database access and a REST API backend via PostgREST. Both backends expose identical function signatures — unified `sb_*` wrappers auto-dispatch to whichever backend is available, or you can call `sb_db_*` / `sb_api_*` directly.

Supabase now issues four types of API keys. All are supported:

| Type | Env var | Notes |
|------|---------|-------|
| Publishable key | `SUPABASE_PUBLISHABLE_KEY` | New format (`sb_publishable_...`) — recommended |
| Secret key | `SUPABASE_SECRET_KEY` | New format (`sb_secret_...`) — bypasses RLS when present |
| Anon key | `SUPABASE_ANON_KEY` | Legacy JWT (`eyJ...`) — still supported, deprecation warning shown |
| Service role key | `SUPABASE_ROLE_KEY` | Legacy JWT (`eyJ...`) — still supported, deprecation warning shown |

## Installation

```r
# install.packages("remotes")
remotes::install_github("deepanshkhurana/supabaseR")
```

## Setup

### DBI Backend

```bash
SUPABASE_HOST=db.xxx.supabase.co
SUPABASE_DBNAME=postgres
SUPABASE_USER=postgres
SUPABASE_PASSWORD=your_password
SUPABASE_SCHEMA=public          # optional; defaults to "public"
```

### REST API Backend

```bash
# New-format keys (recommended)
SUPABASE_URL=https://xxx.supabase.co
SUPABASE_PUBLISHABLE_KEY=sb_publishable_...
SUPABASE_SECRET_KEY=sb_secret_...   # optional; bypasses RLS when present

# Legacy JWT keys (still supported; deprecation warning shown on load)
# SUPABASE_ANON_KEY=eyJ...
# SUPABASE_ROLE_KEY=eyJ...
```

Both backends can be active at the same time. The startup message shows which are detected:

```r
library(supabaseR)
#> supabaseR v0.1.0
#>   DBI Backend: ✔
#>   API Backend: ✔
```

## Quick Start

Use `sb_connect()` to auto-detect the best available backend, then call the unified `sb_*` wrappers:

```r
sb_connect()           # picks DBI if available, otherwise REST API

sb_tables()
sb_read("users", limit = 10)
sb_query("users", where = list(age = list(gt = 18)))
sb_insert("users", data.frame(name = "Alice"))
sb_update("users", data = list(name = "Bob"), where = list(id = 1))
sb_delete("users", where = list(id = 1))

sb_disconnect()
```

## DBI Backend

```r
sb_db_connect()
#> ✔ Connected to db.xxx.supabase.co

sb_db_tables()
sb_db_schema("users")
sb_db_read("users", limit = 10)

# Query with operators
sb_db_query("users", where = list(id = 1))
sb_db_query("users", where = list(age = list(gt = 18)))
sb_db_query("users", where = list(name = list(like = "A%")))
sb_db_query("users", where = list(id = list("in" = c(1, 2, 3))))

# Raw SQL
sb_db_query(sql = "SELECT * FROM public.users WHERE id = 1")

# Write
sb_db_insert("users", data.frame(name = "Alice"))
#> ✔ Inserted 1 row into users

sb_db_update("users", data = list(name = "Bob"), where = list(id = 1))
#> ✔ Updated 1 row in users

sb_db_upsert("users", data.frame(id = 1, name = "Charlie"), conflict_columns = "id")
#> ✔ Upserted 1 row into users

sb_db_delete("users", where = list(id = 1))
#> ✔ Deleted 1 row from users

sb_db_disconnect()
#> ✔ Disconnected
```

## REST API Backend

```r
sb_api_connect()
#> ✔ API credentials set for https://xxx.supabase.co

sb_api_tables()
sb_api_schema("users")
sb_api_read("users", limit = 10)

# Query with operators (same vocabulary as DBI)
sb_api_query("users", where = list(id = 1))
sb_api_query("users", where = list(age = list(gt = 18)))
sb_api_query("users", where = list(name = list(like = "A%")))
sb_api_query("users", where = list(id = list("in" = c(1, 2, 3))))

# Write
sb_api_insert("users", data.frame(name = "Alice"))
sb_api_update("users", data = list(name = "Bob"), where = list(id = 1))
sb_api_upsert("users", data.frame(id = 1, name = "Charlie"), conflict_columns = "id")
sb_api_delete("users", where = list(id = 1))

sb_api_disconnect()
#> ✔ API credentials cleared
```

> **Note:** Raw SQL (`sql =` parameter) is supported only by the DBI backend. `sb_api_query()` uses PostgREST filter operators instead.

## Operators

Use nested lists in `where` for operators beyond `=`. Works identically in both backends:

| Operator | Example | SQL / PostgREST |
|----------|---------|-----------------|
| `eq` | `list(id = list(eq = 1))` | `id = 1` |
| `neq` | `list(id = list(neq = 1))` | `id <> 1` |
| `gt` | `list(age = list(gt = 18))` | `age > 18` |
| `gte` | `list(age = list(gte = 18))` | `age >= 18` |
| `lt` | `list(age = list(lt = 18))` | `age < 18` |
| `lte` | `list(age = list(lte = 18))` | `age <= 18` |
| `like` | `list(name = list(like = "A%"))` | `name LIKE 'A%'` |
| `ilike` | `list(name = list(ilike = "a%"))` | `name ILIKE 'a%'` |
| `in` | `list(id = list("in" = c(1, 2)))` | `id IN (1, 2)` |
| `is` | `list(col = list(is = "NULL"))` | `col IS NULL` |

## Functions

### Unified API

Auto-dispatches to DBI or REST based on available credentials.

| Function | Description |
|----------|-------------|
| `sb_connect()` | Connect (auto-detects backend) |
| `sb_disconnect()` | Disconnect |
| `sb_status()` | Connection status for both backends |
| `sb_tables()` | List tables |
| `sb_table_exists()` | Check if table exists |
| `sb_schema()` | Get table schema |
| `sb_read()` | Read table data |
| `sb_query()` | Query with filters or raw SQL |
| `sb_insert()` | Insert rows |
| `sb_update()` | Update rows |
| `sb_upsert()` | Insert or update rows |
| `sb_delete()` | Delete rows |
| `sb_truncate()` | Truncate table |

### DBI Backend

| Function | Description |
|----------|-------------|
| `sb_db_connect()` | Connect via PostgreSQL/DBI |
| `sb_db_disconnect()` | Disconnect |
| `sb_db_status()` | DBI connection status |
| `sb_db_tables()` | List tables |
| `sb_db_table_exists()` | Check if table exists |
| `sb_db_schema()` | Get table schema |
| `sb_db_read()` | Read table data |
| `sb_db_query()` | Query with filters or raw SQL |
| `sb_db_insert()` | Insert rows |
| `sb_db_update()` | Update rows |
| `sb_db_upsert()` | Insert or update rows |
| `sb_db_delete()` | Delete rows |
| `sb_db_truncate()` | Truncate table |

### REST API Backend

| Function | Description |
|----------|-------------|
| `sb_api_connect()` | Connect via Supabase REST API |
| `sb_api_disconnect()` | Clear stored credentials |
| `sb_api_status()` | API connection status |
| `sb_api_tables()` | List tables via OpenAPI spec |
| `sb_api_table_exists()` | Check if table exists |
| `sb_api_schema()` | Get table schema via OpenAPI spec |
| `sb_api_read()` | Read table data |
| `sb_api_query()` | Query with filters |
| `sb_api_insert()` | Insert rows |
| `sb_api_update()` | Update rows |
| `sb_api_upsert()` | Insert or update rows |
| `sb_api_delete()` | Delete rows |
| `sb_api_truncate()` | Delete all rows (requires secret key or RLS disabled) |

## Supported Features

- [x] CRUD via DBI (`sb_db_*`)
- [x] Unified API with auto-dispatch (`sb_*`)
- [x] CRUD via REST (`sb_api_*`)
- [ ] Authentication (`sb_auth_*`)
- [ ] Storage (`sb_storage_*`)
- [ ] Realtime subscriptions (`sb_realtime_*`)

## License

MIT
