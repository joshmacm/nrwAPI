test_that("live API calls work with various start/end formats", {
  skip_if_offline()
  key <- Sys.getenv("NRW_API_KEY")
  skip_if(!nzchar(key), "NRW_API_KEY environment variable is not set")

  # Quick check if API can be reached
  reachable <- tryCatch({
    res <- httr::GET("https://api.naturalresources.wales/telemetry/api/measurement-areas",
                     httr::add_headers("Ocp-Apim-Subscription-Key" = key),
                     httr::timeout(5))
    httr::status_code(res) == 200
  }, error = function(e) FALSE)

  skip_if(!reachable, "NRW API unreachable or key invalid")

  mid <- "H059000001 Water Level Stage"

  # 1. Test with character strings
  res_str <- nrw_measure_values(key, mid, start = "2026-10-25", end = "2026-10-25")
  expect_s3_class(res_str, "data.frame")
  expect_gt(nrow(res_str), 0)
  expect_named(res_str, c("measureId", "quality", "timestamp", "value"))

  # 2. Test with Date objects
  res_date <- nrw_measure_values(key, mid, start = as.Date("2026-10-25"), end = as.Date("2026-10-25"))
  expect_s3_class(res_date, "data.frame")
  expect_equal(nrow(res_str), nrow(res_date))

  # 3. Test with POSIXct objects
  s_posix <- as.POSIXct("2026-10-25 00:00:00", tz = "UTC")
  e_posix <- as.POSIXct("2026-10-25 01:00:00", tz = "UTC")
  res_posix <- nrw_measure_values(key, mid, start = s_posix, end = e_posix)
  expect_s3_class(res_posix, "data.frame")
  expect_equal(nrow(res_posix), 5) # 00:00, 00:15, 00:30, 00:45, 01:00

  # 4. Test station search with space
  stations <- nrw_stations(key, stationName = "Craig Y Nos")
  expect_s3_class(stations, "data.frame")
  expect_gt(nrow(stations), 0)
})
