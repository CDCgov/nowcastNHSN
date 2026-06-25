# nowcastNHSN 0.1.0

Initial release of `nowcastNHSN`, an R package providing tooling for
nowcasting National Healthcare Safety Network (NHSN) reporting data.

## Nowcast workflow

* `nowcast_config()` and `nowcast_config_test()` build and validate
  configuration objects for a nowcast run, with a dedicated `print()`
  method for inspection.
* `run_single_nowcast()` fits a single nowcast and `run_state_nowcasts()`
  fans the workflow out across U.S. states.
* `summarize_nowcast()` collapses posterior draws into the quantile
  summaries expected by downstream consumers (e.g. FluSight).
* `read_nowcast_parquet()` / `write_nowcast_parquet()` provide a
  parquet-based on-disk format for nowcast outputs.

## Reporting-data sources

* `fetch_reporting_data()` is an S3 generic for pulling reporting data
  from a configurable source, with methods for:
  * `cfa_dataops_source()` — CFA DataOps backend (#30).
  * `delphi_epidata_source()` — Delphi `epidatr` API, with a
    lower-level `fetch_reporting_data_epidatr()` helper.
  * `hub_data_source()` — hubverse `hubData` archives (#24), including
    handling of FluSight reference dates via `target_end_date` (#27).
* `fetch_nhsn_data()` provides a convenience wrapper for the common
  NHSN pull.

## Uncertainty utilities

* `fit_normal()` / `fit_skellam()` add parametric models for errors around
  point nowcasts and `sample_normal()` / `sample_skellam()` sample from them.
* `get_uncertainty_fns()` selects the matched fit/sample pair for use
  inside the nowcast pipeline.
* `nowcast_config()` gains opt-in `robust_sample` and `nonneg_pred` flags
  (both default `FALSE`) that stop failure-prone error distributions such
  as the Skellam from failing: `robust_sample` widens the variance instead of
  erroring when `variance <= |pred|`, and `nonneg_pred` clamps negative
  predicted counts to `0` (#34).

## Date and reporting helpers

* `cumulative_to_incremental()` converts cumulative reporting series to
  incremental counts.
* `saturdays_to_epirange()` maps a vector of Saturday dates to the
  `epirange` format used by `epidatr`.
