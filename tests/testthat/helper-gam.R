# Synthetic weekly reports with known series keys and a finite delay horizon.
gam_reporting_data <- function(
  locations = c("ca", "ny"),
  weeks = 36L,
  cutoff = as.Date("2025-04-12")
) {
  withr::with_seed(714, {
    dates <- cutoff - 7 * rev(seq_len(weeks))
    data <- expand.grid(
      reference_date = dates,
      delay = 1:12,
      location = locations,
      stringsAsFactors = FALSE
    )
    data$report_date <- data$reference_date + 7 * data$delay
    data <- data[data$report_date <= cutoff, ]
    volume <- 300 + 130 * sin(as.numeric(data$reference_date - min(dates)) / 40)
    pmf <- c(.62, .18, .08, .045, .03, .025, .015, .005, 0, 0, 0, 0)
    data$count <- stats::rnbinom(
      nrow(data),
      mu = volume * pmf[data$delay],
      size = 0.8
    )
    data$signal <- "synthetic_hospitalizations"
    data <- data[order(data$location, data$reference_date, data$report_date), ]
    data <- data |>
      dplyr::group_by(.data$location, .data$reference_date) |>
      dplyr::mutate(count = cumsum(.data$count)) |>
      dplyr::ungroup()
    as.data.frame(data[setdiff(names(data), "delay")])
  })
}

gam_triangles <- function(locations = c("ca", "ny")) {
  suppressMessages(as_reporting_triangles(
    gam_reporting_data(locations),
    "location"
  ))
}

skip_without_gam <- function() {
  skip_if_not_installed("mgcv", minimum_version = "1.9-5")
  skip_if_not_installed("gratia", minimum_version = "0.11.2")
}

sort_nowcast_rows <- function(x) {
  x <- as.data.frame(x)
  keys <- setdiff(names(x), c("pred_count", "draw", "output_type", "nowcast"))
  x <- x[do.call(order, x[c(keys, "draw")]), , drop = FALSE]
  rownames(x) <- NULL
  x
}
