# supabaseR

R package for Supabase database operations. The package has a dual-API philosophy that uses DBI _and_ the Supabase API. For Db CRUD operations, the DBI functions take precedence if available. Otherwise, they fallback to the Supabase REST API. The goal is to allow users the flexibility to use both together. Additionally, in the future, we plan to bring about more features from Supabase into this package.

The environment variables dictate if DBI, API or both are available on package load or on `sb_db_connect`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("deepanshkhurana/supabaseR")
```

## Setup

Set environment variables:

```bash
SUPABASE_HOST=db.xxx.supabase.co
SUPABASE_DBNAME=postgres
SUPABASE_USER=postgres
SUPABASE_PASSWORD=your_password
SUPABASE_SCHEMA=public
```

## Usage

```r
library(supabaseR)
#> supabaseR v...
#>   DBI Backend: ✔
#>   API Backend: ✖

sb_db_connect()
#> ✔ Connected to db.xxx.supabase.co

# Read
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

## Operators

Use nested lists in `where` for operators beyond `=`:

| Operator | Example | SQL |
|----------|---------|-----|
| `eq` | `list(id = list(eq = 1))` | `id = 1` |
| `neq` | `list(id = list(neq = 1))` | `id <> 1` |
| `gt` | `list(age = list(gt = 18))` | `age > 18` |
| `gte` | `list(age = list(gte = 18))` | `age >= 18` |
| `lt` | `list(age = list(lt = 18))` | `age < 18` |
| `lte` | `list(age = list(lte = 18))` | `age <= 18` |
| `like` | `list(name = list(like = "A%"))` | `name LIKE 'A%'` |
| `ilike` | `list(name = list(ilike = "a%"))` | `name ILIKE 'a%'` |
| `in` | `list(id = list("in" = c(1,2)))` | `id IN (1, 2)` |
| `is` | `list(col = list(is = "NULL"))` | `col IS NULL` |

## Functions

| Function | Description |
|----------|-------------|
| `sb_db_connect()` | Connect to Supabase |
| `sb_db_disconnect()` | Disconnect |
| `sb_db_status()` | Connection status |
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

## Supported Features
- [x] CRUD via DBI (`sb_db_*`)
- [ ] CRUD via REST (`sb_db_*`)
- [ ] Authentication (`sb_auth_*`)
- [ ] Storage (`sb_storage_*`)
- [ ] Realtime subscriptions (`sb_realtime_*`)

## License

MIT
