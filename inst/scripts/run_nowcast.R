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
args <- commandArgs(trailingOnly = TRUE)

# Help message
if ("--help" %in% args || "-h" %in% args) {
  cat(
    "
NHSN State-Level Nowcast Script

Usage:
  Rscript run_nowcast.R [options]

Options:
  --test                    Run in test mode with minimal settings.
                            Uses: ca, 10 draws, max_delay=4.
                            Overrides other config options.

  --location <loc>          Location(s) to process. Use 'all' for all states,
                            or comma-separated codes (e.g., 'ca,ny,tx').
                            Default: all

  --target <target>         Disease target: 'covid', 'flu', or 'rsv'.
                            Default: covid

  --max-delay <n>           Maximum delay in weeks. Default: 7

  --scale-factor <n>        Scale factor for training volume. Default: 3

  --prop-delay <n>          Proportion of data for delay estimation.
                            Default: 0.5

  --uncertainty-model <m>   Uncertainty model: 'negative_binomial', 'normal',
                            or 'skellam'. Default: negative_binomial

  --draws <n>               Number of posterior draws. Default: 1000

  --output-dir <path>       Output directory for parquet files.
                            Default: output/nowcasts

  --parallel                Enable parallel processing via furrr.

  --help, -h                Show this help message.

Examples:
  # Quick test run
  Rscript run_nowcast.R --test

  # Run all states with defaults
  Rscript run_nowcast.R --location all --output-dir output/nowcasts

  # Run single state for Azure task
  Rscript run_nowcast.R --location ca --output-dir output/nowcasts

  # Run with custom parameters
  Rscript run_nowcast.R --location ca,ny --uncertainty-model skellam --draws 500

"
  )
  quit(status = 0)
}

# Parse argument function
parse_arg <- function(args, flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) {
    return(default)
  }
  if (idx + 1 > length(args)) {
    stop(sprintf("Missing value for %s", flag))
  }
  args[idx + 1]
}

# Parse flag function (boolean)
parse_flag <- function(args, flag) {
  flag %in% args
}

# Check for test mode first
test_mode <- parse_flag(args, "--test")

# Load package
library(nowcastNHSN)

if (test_mode) {
  # Test mode: use minimal settings
  cli::cli_alert_info("Running in TEST MODE")
  target <- parse_arg(args, "--target", "covid")
  output_dir <- parse_arg(args, "--output-dir", NULL)
  config <- nowcast_config_test(output_dir = output_dir)
  locations <- config$location # Single location from test config
  parallel <- FALSE
} else {
  # Normal mode: parse all arguments
  location_arg <- parse_arg(args, "--location", "all")
  target <- parse_arg(args, "--target", "covid")
  max_delay <- as.integer(parse_arg(args, "--max-delay", "7"))
  scale_factor <- as.numeric(parse_arg(args, "--scale-factor", "3"))
  prop_delay <- as.numeric(parse_arg(args, "--prop-delay", "0.5"))
  uncertainty_model <- parse_arg(
    args,
    "--uncertainty-model",
    "negative_binomial"
  )
  draws <- as.integer(parse_arg(args, "--draws", "1000"))
  output_dir <- parse_arg(args, "--output-dir", "output/nowcasts")
  parallel <- parse_flag(args, "--parallel")

  # Parse locations
  if (location_arg == "all") {
    locations <- "all"
  } else {
    locations <- strsplit(location_arg, ",")[[1]]
    locations <- trimws(locations)
  }

  # Set up parallel if requested
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Package 'future' required for parallel processing")
    }
    future::plan(future::multisession)
    cli::cli_alert_info("Parallel processing enabled")
  }

  # Create configuration
  config <- nowcast_config(
    max_delay = max_delay,
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    uncertainty_model = uncertainty_model,
    draws = draws,
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
current_epiweek <- lubridate::epiweek(today)
current_year <- lubridate::epiyear(today)

# Convert to YYYYWW format
end_epiweek <- current_year * 100 + current_epiweek
# Go back ~30 weeks
start_year <- if (current_epiweek > 30) current_year else current_year - 1
start_week <- if (current_epiweek > 30) {
  current_epiweek - 30
} else {
  52 - (30 - current_epiweek)
}
start_epiweek <- start_year * 100 + start_week

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
