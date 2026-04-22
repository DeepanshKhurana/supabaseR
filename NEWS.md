# supabaseR 1.0.1 (2026-04-22)

## Documentation

* Updated `README` with [CRAN](https://cran.r-project.org/web/packages/supabaseR/index.html) installation instructions as the primary installation method.

# supabaseR 1.0.0 (2026-04-07)

## Breaking changes

* All functions renamed from `get_*` / `put_*` to namespaced `sb_db_*`, `sb_api_*`, and `sb_*` prefixes. Legacy functions removed.

## New features

* Dual-backend design: DBI/PostgreSQL (`sb_db_*`) and Supabase REST API (`sb_api_*`), with unified wrappers (`sb_*`) that auto-dispatch.
* Full CRUD for both backends: `read`, `query`, `insert`, `update`, `upsert`, `delete`, `truncate`.
* Table metadata helpers: `tables`, `table_exists`, `schema`.
* `where` filter operators across both backends: `gt`, `lt`, `gte`, `lte`, `neq`, `like`, `ilike`, `in`, `is`. Raw SQL supported on the DBI backend.
* All four Supabase key formats supported. Secret / service-role keys bypass Row Level Security (RLS) automatically. Deprecation warnings for legacy JWT keys.
* `SUPABASE_SCHEMA` env var honoured by both backends via `get_schema()`.
* CLI startup message on load showing which backends are detected.
* `pkgdown` documentation website at [deepanshkhurana.github.io/supabaseR](https://deepanshkhurana.github.io/supabaseR/reference/index.html)

## Bug fixes and improvements

* SQL construction uses `glue::glue_sql()` throughout to prevent injection.
* 100% test coverage via `testthat` 3 with `mockery` stubs; no live database required.

# supabaseR 0.0.1

* Initial release with basic CRUD functions and environment variable configuration.
