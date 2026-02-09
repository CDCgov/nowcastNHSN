#!/usr/bin/env Rscript
#
# NHSN State-Level Nowcast Entrypoint Script
#
# Run nowcasts for US states/territories with configurable parameters.
# Designed to be called from command line or Azure container entrypoint.
#
# Usage:
#   Rscript run_nowcast.R --location all --output-dir output/nowcasts
#   Rscript run_nowcast.R --location ca --uncertainty-model skellam
#   Rscript run_nowcast.R --help
#

# Parse command line arguments
parser <- argparse::ArgumentParser(
  description = "NHSN State-Level Nowcast Script"
)
parser$add_argument(
  "--test",
  action = "store_true",
  default = FALSE,
  help = paste(
    "Run in test mode with minimal settings.",
    "Uses: ca, 10 draws, max_delay=4. Overrides other config options."
  )
)
parser$add_argument(
  "--location",
  default = "all",
  help = paste(
    "Location(s) to process. Use 'all' for all states,",
    "or comma-separated codes (e.g., 'ca,ny,tx'). [default: %(default)s]"
  )
)
parser$add_argument(
  "--target",
  default = "covid",
  help = "Disease target: 'covid', 'flu', or 'rsv'. [default: %(default)s]"
)
parser$add_argument(
  "--max-delay",
  type = "integer",
  default = 7L,
  help = "Maximum delay in weeks. [default: %(default)s]"
)
parser$add_argument(
  "--scale-factor",
  type = "double",
  default = 3.0,
  help = "Scale factor for training volume. [default: %(default)s]"
)
parser$add_argument(
  "--prop-delay",
  type = "double",
  default = 0.5,
  help = "Proportion of data for delay estimation. [default: %(default)s]"
)
parser$add_argument(
  "--uncertainty-model",
  default = "negative_binomial",
  help = paste(
    "Uncertainty model: 'negative_binomial', 'normal',",
    "or 'skellam'. [default: %(default)s]"
  )
)
parser$add_argument(
  "--draws",
  type = "integer",
  default = 1000L,
  help = "Number of posterior draws. [default: %(default)s]"
)
parser$add_argument(
  "--output-dir",
  default = "output/nowcasts",
  help = "Output directory for parquet files. [default: %(default)s]"
)
parser$add_argument(
  "--parallel",
  action = "store_true",
  default = FALSE,
  help = "Enable parallel processing via mirai."
)

args <- parser$parse_args()

# Load package
library(nowcastNHSN)

if (args$test) {
  # Test mode: use minimal settings
  cli::cli_alert_info("Running in TEST MODE")
  target <- args$target
  output_dir <- args$output_dir
  config <- nowcast_config_test(output_dir = output_dir)
  locations <- config$location # Single location from test config
  parallel <- FALSE
} else {
  # Normal mode
  target <- args$target
  output_dir <- args$output_dir
  parallel <- args$parallel

  # Parse locations
  if (args$location == "all") {
    locations <- "all"
  } else {
    locations <- trimws(strsplit(args$location, ",")[[1]])
  }

  # Note: parallel processing is handled internally by run_state_nowcasts()
  # via purrr::in_parallel() and mirai

  # Create configuration
  config <- nowcast_config(
    max_delay = args$max_delay,
    scale_factor = args$scale_factor,
    prop_delay = args$prop_delay,
    uncertainty_model = args$uncertainty_model,
    draws = args$draws,
    output_dir = output_dir
  )
}

cli::cli_h1("NHSN Nowcast Pipeline")
print(config)

# Create data source
cli::cli_alert_info("Creating data source for {.val {target}}")
source <- delphi_epidata_source(
  target = target,
  geo_types = "state"
)

# Define date ranges - use reasonable defaults
# Get data from last 30 weeks for reference dates
# and all available report dates
today <- Sys.Date()
date_to_yyyyww <- function(x) {
  lubridate::epiyear(x) * 100 + lubridate::epiweek(x)
}
end_epiweek <- date_to_yyyyww(today)
start_epiweek <- date_to_yyyyww(today - lubridate::weeks(30))

reference_dates <- epidatr::epirange(start_epiweek, end_epiweek)
report_dates <- "*"

# Determine geo_values based on locations
if (length(locations) == 1 && locations == "all") {
  geo_values <- "*"
} else {
  geo_values <- locations
}

# Fetch data
cli::cli_alert_info("Fetching reporting data...")
reporting_data <- fetch_reporting_data(
  source = source,
  reference_dates = reference_dates,
  report_dates = report_dates,
  locations = geo_values
)

cli::cli_alert_success(
  "Fetched {nrow(reporting_data)} rows for {length(unique(reporting_data$location))} locations"
)

# Run nowcasts - cumulative to incremental conversion happens internally
cli::cli_alert_info("Running nowcasts...")
results <- run_state_nowcasts(
  data = reporting_data,
  config = config,
  locations = locations,
  parallel = parallel
)

# Write output
if (!is.null(output_dir)) {
  cli::cli_alert_info("Writing results to parquet...")
  write_nowcast_parquet(results, output_dir)
}

# Report failures
failed <- attr(results, "failed_locations")
if (length(failed) > 0) {
  cli::cli_alert_warning("Failed locations: {.val {failed}}")
  # Exit with non-zero status if any failures
  quit(status = 1)
}

cli::cli_alert_success("Pipeline complete!")
