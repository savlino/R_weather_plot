# R_weather_plot

Monthly weather heatmaps built from a continuously accumulated feed of
[AEMET OpenData](https://opendata.aemet.es/) surface observations.

## How the feed works

AEMET's station endpoint
`/api/observacion/convencional/datos/estacion/{idema}` only returns the **last
~12 hours** of observations. There is no sub-daily historical endpoint: the
historical service (`/api/valores/climatologicos/diarios/...`) is **daily
aggregates only** (`tmed`, `tmin`, `tmax`, `prec`, `velmedia`, `racha`, `sol`,
`presMax`/`presMin`) and cannot fill a 3-hour-bin heatmap. The two datasets do
**not** share a parameter set.

Consequently the project polls the station endpoint on a schedule and appends
into `data/weather.sqlite`. `(idema, fint)` is the primary key, so overlapping
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

## Layout

| Path | Purpose |
| --- | --- |
| [R/aemet.R](R/aemet.R) | API client, response normalisation |
| [R/store.R](R/store.R) | SQLite schema, upsert, queries |
| [R/plot_heatmap.R](R/plot_heatmap.R) | Binning and `pheatmap` rendering |
| [scripts/fetch.R](scripts/fetch.R) | Polling entry point |
| [scripts/render_heatmap.R](scripts/render_heatmap.R) | Plot entry point |
| [heatmap_project.r](heatmap_project.r) | Interactive fetch + plot |

## Setup

```r
install.packages(c("httr2", "jsonlite", "DBI", "RSQLite",
                   "dplyr", "lubridate", "reshape2", "pheatmap"))
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
Rscript scripts/fetch.R 1083L
Rscript scripts/render_heatmap.R 1083L 2026 8 ta
```

Supported parameters include `ta` (temperature), `hr`, `pres`, `prec`, `vv`.
Precipitation is summed per bin, everything else is averaged.

## Automation

[.github/workflows/fetch-aemet.yml](.github/workflows/fetch-aemet.yml) polls
every 6 hours and commits the updated database back to the repository. Add the
key as repository secret `AEMET_API_KEY`.

## Previous data source

Earlier revisions read a one-off CSV export from the
[Meteogalicia](https://www.meteogalicia.gal/observacion/estacionshistorico/historico.action)
portal (station *Porto de Vigo*), kept here as
[resultadoCSV_24.12.csv](resultadoCSV_24.12.csv). That export was **long**
format — one row per parameter per timestamp, with `Código.parámetro` / `Valor`
columns — whereas AEMET returns **wide** rows, one column per parameter. The
plotting code no longer filters by parameter name; it selects a column instead.
