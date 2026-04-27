write_cfa_dataops_csv <- function(download_dir, version, data) {
  version_dir <- file.path(download_dir, version)
  dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(
    data,
    file = file.path(version_dir, "part-0.csv"),
    row.names = FALSE,
    quote = FALSE
  )
  writeLines("{}", file.path(version_dir, "metadata.json"))
}

mock_cfa_dataops_download <- function(command, args) {
  download_dir <- args[[2]]
  write_cfa_dataops_csv(
    download_dir,
    "2024-12-11T12-00-00",
    data.frame(
      weekendingdate = c("2024-12-07", "2024-12-07", "2024-11-30"),
      jurisdiction = c("CA", "USA", "CA"),
      totalconfc19newadm = c(100, 900, 50)
    )
  )
  write_cfa_dataops_csv(
    download_dir,
    "2024-12-14T12-00-00",
    data.frame(
      weekendingdate = c("2024-12-07", "2024-12-07"),
      jurisdiction = c("CA", "USA"),
      totalconfc19newadm = c(130, 999)
    )
  )
  invisible(character())
}

test_that("fetch_reporting_data works with mocked cfa-dataops download", {
  captured <- new.env(parent = emptyenv())
  runner <- function(command, args) {
    captured$command <- command
    captured$args <- args
    mock_cfa_dataops_download(command, args)
  }

  cache_dir <- tempfile("mock-cfa-dataops-")
  src <- cfa_dataops_source(
    target = "covid",
    command = "fake_dataops_save",
    cache_dir = cache_dir,
    run_dataops_save = runner
  )

  expect_warning(
    result <- fetch_reporting_data(
      source = src,
      reference_dates = as.Date("2024-12-07"),
      report_dates = as.Date("2024-12-14"),
      locations = c("ca", "us")
    ),
    "Multiple cfa-dataops versions"
  )

  expect_equal(captured$command, "fake_dataops_save")
  expect_equal(captured$args[[1]], "public.stf.nhsn_hrd_prelim")
  expect_equal(captured$args[[2]], cache_dir)
  expect_equal(
    captured$args[[which(captured$args == "--stage") + 1L]],
    "load"
  )
  expect_equal(
    captured$args[[which(captured$args == "--version") + 1L]],
    ">=2024-12-08,<2024-12-15"
  )
  expect_true("--full_range" %in% captured$args)

  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("reference_date", "report_date", "location", "count", "signal")
  )
  expect_equal(result$reference_date, rep(as.Date("2024-12-07"), 2))
  expect_equal(result$report_date, rep(as.Date("2024-12-14"), 2))
  expect_equal(result$location, c("ca", "us"))
  expect_equal(result$count, c(130, 999))
  expect_equal(result$signal, rep("totalconfc19newadm", 2))
})

test_that("cfa-dataops dedup can keep earliest version", {
  cache_dir <- tempfile("mock-cfa-dataops-")
  dir.create(cache_dir, recursive = TRUE)
  mock_cfa_dataops_download("fake_dataops_save", c("dataset", cache_dir))

  src <- cfa_dataops_source(target = "covid", cache_dir = cache_dir)
  expect_warning(
    result <- read_cfa_dataops_download(cache_dir, src, dedup = "earliest"),
    "Multiple cfa-dataops versions"
  )

  result <- result[
    result$reference_date == as.Date("2024-12-07") &
      result$location == "ca",
  ]
  expect_equal(result$count, 100)
})

test_that("cfa-dataops source reports missing CLI clearly", {
  src <- cfa_dataops_source(
    command = "nowcastnhsn-definitely-missing-dataops-save"
  )

  expect_error(
    fetch_reporting_data(
      source = src,
      reference_dates = "*",
      report_dates = as.Date("2024-12-14"),
      locations = "ca"
    ),
    "Cannot find"
  )
})

test_that("cfa-dataops source warns before unrestricted version download", {
  expect_warning(
    cfa_dataops_version_query("*"),
    "Downloading all matching"
  )
})

test_that("cfa-dataops reader rejects unsupported file formats", {
  cache_dir <- tempfile("mock-cfa-dataops-")
  version_dir <- file.path(cache_dir, "2024-12-11T12-00-00")
  dir.create(version_dir, recursive = TRUE)
  writeLines("not,a,supported,download", file.path(version_dir, "part-0.txt"))

  src <- cfa_dataops_source(target = "covid", cache_dir = cache_dir)
  expect_error(
    read_cfa_dataops_download(cache_dir, src, dedup = "latest"),
    "Unsupported cfa-dataops file format"
  )
})

test_that("cfa-dataops integration can fetch real NHSN data", {
  skip_if_not(
    identical(Sys.getenv("NOWCASTNHSN_RUN_DATAOPS_TESTS"), "true"),
    "Set NOWCASTNHSN_RUN_DATAOPS_TESTS=true to run cfa-dataops integration tests."
  )

  src <- cfa_dataops_source(cache_dir = tempfile("real-cfa-dataops-"))
  result <- fetch_reporting_data(
    source = src,
    reference_dates = as.Date("2024-12-07"),
    report_dates = as.Date("2024-12-14"),
    locations = "ca"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(
    c("reference_date", "report_date", "location", "count", "signal") %in%
      names(result)
  ))
})
