test_that("parse_date_time parses character strings correctly", {
  # Start time
  s_char <- nrwAPI:::parse_date_time("2026-10-25", is_end = FALSE)
  expect_s3_class(s_char, "POSIXct")
  expect_equal(format(s_char, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 00:00:00")

  # End time for date-only string includes whole day
  e_char <- nrwAPI:::parse_date_time("2026-10-25", is_end = TRUE)
  expect_s3_class(e_char, "POSIXct")
  expect_equal(format(e_char, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 23:59:59")

  # String with explicit time
  dt_char <- nrwAPI:::parse_date_time("2026-10-25 14:30:00", is_end = FALSE)
  expect_equal(format(dt_char, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 14:30:00")
})

test_that("parse_date_time handles Date and POSIXt objects correctly", {
  d_start <- nrwAPI:::parse_date_time(as.Date("2026-10-25"), is_end = FALSE)
  expect_equal(format(d_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 00:00:00")

  d_end <- nrwAPI:::parse_date_time(as.Date("2026-10-25"), is_end = TRUE)
  expect_equal(format(d_end, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 23:59:59")

  posix <- as.POSIXct("2026-10-25 12:00:00", tz = "UTC")
  res_posix <- nrwAPI:::parse_date_time(posix)
  expect_equal(format(res_posix, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 12:00:00")
})

test_that("parse_date_time throws errors on invalid inputs", {
  expect_error(nrwAPI:::parse_date_time("not-a-date"), "Could not parse date string")
  expect_error(nrwAPI:::parse_date_time(12345), "Date parameters must be Date, POSIXct, POSIXlt, or character strings")
})

test_that("nrw_measure_values validates date range and ordering", {
  expect_error(
    nrw_measure_values("fake_key", "fake_measure", start = "2026-10-26", end = "2026-10-25"),
    "`start` date cannot be after `end` date"
  )

  expect_error(
    nrw_measure_values("fake_key", "fake_measure", start = "2024-01-01", end = "2025-06-01"),
    "The date range must cover less than one year"
  )
})

test_that("nrw_measure_values returns empty data.frame on empty API response", {
  # Mock nrw_api_request to return empty list
  with_mocked_bindings(
    {
      res <- nrw_measure_values("fake_key", "fake_measure", start = "2026-10-25", end = "2026-10-25")
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 0)
      expect_named(res, c("measureId", "quality", "timestamp", "value"))
      expect_s3_class(res$timestamp, "POSIXct")
      expect_type(res$value, "double")
    },
    nrw_api_request = function(endpoint, api_key, query_params) {
      list()
    }
  )
})

test_that("nrw_measure_values parses timestamps in UTC and formats values as numeric", {
  mock_data <- data.frame(
    measureId = "M01",
    quality = 0L,
    timestamp = "2026-10-25 12:00:00",
    value = "1.523",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    {
      res <- nrw_measure_values("fake_key", "M01", start = "2026-10-25", end = "2026-10-25")
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 1)
      expect_equal(res$value, 1.523)
      expect_equal(format(res$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-25 12:00:00")
      expect_equal(attr(res$timestamp, "tzone"), "UTC")
    },
    nrw_api_request = function(endpoint, api_key, query_params) {
      mock_data
    }
  )
})
