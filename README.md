# R_weather_plot

Monthly weather heatmaps built from a continuously accumulated feed of
[AEMET OpenData](https://opendata.aemet.es/) surface observations.

## Heatmap examples

### Legacy Vigo demo

![Legacy Vigo temperature heatmap](plots/legacy-vigo-meteogalicia.png)

This December 2024 heatmap uses the previous Meteogalicia export for Porto de
Vigo. It is retained as a visual demo only and is not produced from the current
AEMET feed.

### Current AEMET feed

![Castro Urdiales temperature heatmap](plots/current-castro-urdiales-aemet.png)

This August 2026 heatmap uses the current AEMET feed for Castro Urdiales-EDAR.
It is an in-progress example with partial monthly coverage; the first complete
replacement for the legacy demo is expected in September 2026.

## How the feed works

AEMET's station endpoint
`/api/observacion/convencional/datos/estacion/{idema}` only returns the **last
~12 hours** of observations. There is no sub-daily historical endpoint: the
historical service (`/api/valores/climatologicos/diarios/...`) is **daily
aggregates only** (`tmed`, `tmin`, `tmax`, `prec`, `velmedia`, `racha`, `sol`,
`presMax`/`presMin`) and cannot fill a 3-hour-bin heatmap. The two datasets do
**not** share a parameter set.

Consequently the project polls the station endpoint on a schedule and appends
into the SQLite snapshot stored in R2. `(idema, fint)` is the primary key, so overlapping
windows and re-runs are idempotent: a poll returning a timestamp already stored
replaces that row only if the payload actually changed, otherwise it is skipped.
AEMET does revise recent observations, so this is last-write-wins — superseded
values are not retained.

The default station is **`1083L` — CASTRO URDIALES-EDAR** (Cantabria), which
reports hourly. Fields vary by station: this one leaves `ts`, `tss5cm`,
`tss20cm`, `nieve`, `inso`, `rviento` and `vis` empty, which the client treats
as `NA` rather than an error.

Requests use the two-step AEMET protocol: the first response is a JSON envelope
carrying a short-lived `datos` URL; the payload behind it is ISO-8859-15.

<!-- feed-stats:start -->
_Generated from the active SQLite snapshot in Cloudflare R2._
<!-- feed-stats:end -->

## Layout

| Path | Purpose |
| --- | --- |
| [R/config.R](R/config.R) | Default station and environment overrides |
| [R/aemet.R](R/aemet.R) | API client, response normalisation |
| [R/store.R](R/store.R) | SQLite schema, upsert, queries |
| [R/plot_heatmap.R](R/plot_heatmap.R) | Binning and `pheatmap` rendering |
| [scripts/fetch.R](scripts/fetch.R) | Polling entry point |
| [scripts/render_heatmap.R](scripts/render_heatmap.R) | Plot entry point |
| [scripts/update_stats.R](scripts/update_stats.R) | Generated README feed statistics |
| [scripts/archive_month.R](scripts/archive_month.R) | Dry-run and monthly R2 archive logic |
| [tests/testthat](tests/testthat) | Network-free tests for parsing, storage, and binning |
| [heatmap_project.r](heatmap_project.r) | Interactive fetch + plot |

## Setup

```r
install.packages(c("httr2", "jsonlite", "DBI", "RSQLite",
                   "dplyr", "lubridate", "reshape2", "pheatmap", "testthat"))
```

Put the key in a `.Renviron` file **in the project root** (gitignored):

```
AEMET_API_KEY=your_key_here
```

R reads `.Renviron` from the working directory at startup before falling back to
`~`. Project-local is preferred here because `~` may be redirected to a
cloud-synced folder (OneDrive et al.), which is a poor place for a credential.
Check with `path.expand("~")` if the key seems not to load, and remember
`.Renviron` is only read at session start.

Verify with:

```sh
Rscript scripts/setup.R
```

## Usage

```sh
Rscript scripts/fetch.R
Rscript scripts/render_heatmap.R 2026 8 ta
```

The default station is configured once in `R/config.R`. Pass station codes as
arguments for a one-off fetch or set `AEMET_STATIONS` to a comma- or
space-separated list to fetch multiple stations without changing the scripts.

Supported parameters include `ta` (temperature), `hr`, `pres`, `prec`, `vv`.
Precipitation is summed per bin, everything else is averaged.

## Tests

The test suite uses synthetic observations and never calls AEMET:

```sh
Rscript tests/testthat.R
```

It covers optional fields and timestamps in the AEMET normaliser, idempotent
and revision-aware SQLite writes, UTC-to-local-time conversion, complete month
matrix dimensions, and precipitation sums within a 3-hour bin. The same suite
runs in GitHub Actions on pushes and pull requests.

## Monthly archive

After a month is complete, review its heatmap before removing its detailed rows
from the active SQLite snapshot. The manual [archive workflow](.github/workflows/archive-month.yml)
requires an explicit year and month and defaults to a non-destructive dry run:

```text
Actions -> Archive completed weather month -> Run workflow
year: 2026
month: 9
dry_run: true
```

The dry run renders a preview artifact and reports the row count without
uploading, deleting, or changing the R2 snapshot. After checking that preview,
run the same month again with `dry_run: false`. The destructive run uploads:

```text
archives/2026-09/observations.csv.gz
archives/2026-09/heatmap.png
```

It then removes the reviewed month from the active SQLite file, runs `VACUUM`,
uploads the reduced snapshot to R2, and commits the reviewed PNG and updated
README statistics. The current and future months cannot be archived, and an
existing archive cannot be overwritten.

## Automation

[.github/workflows/fetch-aemet.yml](.github/workflows/fetch-aemet.yml) polls
every 6 hours and updates the SQLite snapshot in Cloudflare R2.

Two repository secrets are required:

| Secret | Purpose |
| --- | --- |
| `AEMET_API_KEY` | OpenData API key |
| `FEED_PUSH_TOKEN` | Fine-grained PAT with `Contents: Read and write` on this repo |

The PAT exists because pushes made with the default `GITHUB_TOKEN` are
attributed to `github-actions[bot]`, and bot pushes do not appear to count as
repository activity. Public repositories have their scheduled workflows
disabled after 60 days without activity, so a feed that only ever commits as
the bot would eventually switch itself off. Pushing as a user avoids that.
Fine-grained tokens expire, so the token needs rotating before it does.

The fetch workflow stores the current SQLite snapshot in Cloudflare R2 and
restores it before each poll. This keeps the active database out of Git history.
R2 is now required by the fetch workflow because `data/weather.sqlite` is no
longer tracked in Git. Create these repository secrets:

| Secret | Purpose |
| --- | --- |
| `R2_ACCOUNT_ID` | Cloudflare account ID used in the S3-compatible endpoint |
| `R2_ACCESS_KEY_ID` | R2 API token access key |
| `R2_SECRET_ACCESS_KEY` | R2 API token secret |
| `R2_BUCKET` | Target R2 bucket name |

The workflow uses `snapshots/weather.sqlite` as its durable live database and
uploads it after each successful fetch.
The credentials are used only by the upload step and are never written to the
repository. The workflow fails clearly if an R2 secret is missing.

For the R2 token, create an R2 API token with `Object Read & Write` permission
for the selected bucket. The endpoint is based on the Cloudflare account ID:

```text
https://<account-id>.r2.cloudflarestorage.com
```

R2 currently includes 10 GB-month of Standard storage, 1 million Class A
operations, 10 million Class B operations, and free egress each month. This
project is far below those limits.

## Design trade-offs

### A rolling API window, not historical backfill

AEMET's conventional observation endpoint exposes only the latest roughly 12
hours. The historical endpoint provides daily aggregates, not the hourly
observations needed for this heatmap. Polling every 6 hours gives overlapping
coverage: a delayed or missed run can usually be recovered by the next poll,
but two consecutive missed runs can create a permanent gap.

### SQLite in R2

The feed uses SQLite because it is portable, has no server to operate, and is
more than sufficient for one hourly station and a small personal project. The
live file is stored in R2 rather than committed to Git; the workflow downloads
it, updates it, and uploads the replacement.

This is a deliberate hobby-project trade-off, not a claim that Git is a good
general-purpose database. R2 provides durable object storage while SQLite keeps
the data model simple and reproducible.

The manual monthly retention plan will archive that month's rows to R2 after its
heatmap has been checked, remove them from the active SQLite file, and commit
the reviewed PNG and summary metadata to Git. The archive step will be explicit
and manual so an incomplete month cannot be deleted by accident.

If the feed grows beyond this project's scale, the next options are:

    locally when needed;
    history;
    or multiple stations justify operating a service.

Containerising the R scripts would improve reproducibility, but it would not
solve Git's binary-history problem or replace the need for durable storage.

## Previous data source

Earlier revisions read a one-off CSV export from the
[Meteogalicia](https://www.meteogalicia.gal/observacion/estacionshistorico/historico.action)
portal (station *Porto de Vigo*), kept here as
[resultadoCSV_24.12.csv](resultadoCSV_24.12.csv). That export was **long**
format — one row per parameter per timestamp, with `Código.parámetro` / `Valor`
columns — whereas AEMET returns **wide** rows, one column per parameter. The
plotting code no longer filters by parameter name; it selects a column instead.
