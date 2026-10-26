### API Functions



#' Function to Return Stations based on Query Parameters
#' @param api_key NRW Telemetry API key. String.
#' @description
#' Returns stations matching any of the provided query parameters.
#' @param stationName String. Partial match of all stations.
#' @param riverName String. Partial match of all river names.
#' @param hydrometricAreaId String. Hydrometric area ID as defined by CEH.
#' @param localAuthority String. Exact match for name of local authority.
#' @param nrwArea String. NRW Area.
#' @param floodTargetArea String. NRW Flood area.
#' @param status String. Exact match.
#'
#' @export
#' @returns data.frame containing matching stations and associated metadata.
nrw_stations <- function(api_key, stationName = NULL, riverName = NULL, hydrometricAreaId = NULL, localAuthority = NULL, nrwArea = NULL, floodTargetArea = NULL, status = NULL) {
  endpoint <- "/api/stations"

  # Create a list of parameters for the API request
  params <- list(
    stationName = stationName,
    riverName = riverName,
    hydrometricAreaId = hydrometricAreaId,
    localAuthority = localAuthority,
    nrwArea = nrwArea,
    floodTargetArea = floodTargetArea,
    status = status
  )

  # Remove NULL parameters
  params <- params[!sapply(params, is.null)]


  station_data <- nrw_api_request(endpoint, api_key, params)

  return(station_data)
}



#' Retrieve Measures for a Given Station
#' @param api_key NRW Telemetry API key. String.
#' @description
#' Retrieve measures for a given station and station type. The Measure_id can then be used to retreive values using nrw_measure_values.
#'
#' @param station 10 digit monitoring station reference code e.g H059000001 for Ynystanglws River Station. String.
#' @param type Station type, leave blank for measures of any type. River Level, Flow, Rainfall
#' @returns A data.frame where each row represents a different measure for a given station.
#' @export
nrw_station_measures <- function(api_key, station, type = NULL) {
  # Set the API endpoint
  endpoint <- "/api/measures"
  # Initialize query parameters with the station reference
  params <- list(
    monitoringStationReference = station  # Station reference ID
  )
  # Only include measureType if type is not NULL
  if (!is.null(type)) {
    params$measureType <- type
  }
  # Make the API request
  measures_data <- nrw_api_request(endpoint, api_key, params)

  return(measures_data)
}


#' Parse date-time parameter to POSIXct in UTC
#' @noRd
parse_date_time <- function(x, is_end = FALSE) {
  if (is.null(x)) return(NULL)

  if (inherits(x, "Date") && !inherits(x, "POSIXt")) {
    if (is_end) {
      x <- as.POSIXct(paste(format(x, "%Y-%m-%d"), "23:59:59"), tz = "UTC")
    } else {
      x <- as.POSIXct(paste(format(x, "%Y-%m-%d"), "00:00:00"), tz = "UTC")
    }
  } else if (is.character(x)) {
    parsed <- tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) as.POSIXct(NA))
    if (is.na(parsed)) {
      stop("Could not parse date string: '", x, "'. Please provide a Date, POSIXct, or string in 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM:SS' format.")
    }
    if (is_end && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trimws(x))) {
      x <- as.POSIXct(paste(trimws(x), "23:59:59"), tz = "UTC")
    } else {
      x <- parsed
    }
  } else if (inherits(x, "POSIXt")) {
    x <- as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  } else {
    stop("Date parameters must be Date, POSIXct, POSIXlt, or character strings.")
  }

  return(x)
}


#' Return Values for a Specific Measure
#' @param api_key NRW Telemetry API key. String.
#' @description
#' Returns values for a specific measure code. A time period can be provided to narrow the response. If no start or end date is provided then the default period is the preceding 24 hours from last value.
#'
#' @param measure String. Measure_id as given by nrw_station_measures.
#' @param start Optional. Date, POSIXt, or character string (e.g. 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM:SS'). Start of period.
#' @param end Optional. Date, POSIXt, or character string. End of period. If start is provided but end is omitted, defaults to current time. If end is provided but start is omitted, defaults to 24 hours prior to end.
#' @returns data.frame with measureId, quality code, timestamp, value
#' @export
nrw_measure_values <- function(api_key, measure, start = NULL, end = NULL) {
  # Define the API endpoint
  endpoint <- "/api/readings"

  # Parse date/time inputs
  start_dt <- parse_date_time(start, is_end = FALSE)
  end_dt <- parse_date_time(end, is_end = TRUE)

  # If only start date is provided, default end date to current time (UTC)
  if (!is.null(start_dt) && is.null(end_dt)) {
    end_dt <- as.POSIXct(Sys.time(), tz = "UTC")
  } else if (is.null(start_dt) && !is.null(end_dt)) {
    # If only end date is provided, default start date to 24 hours prior to end
    start_dt <- end_dt - 86400
  }

  # Validate date ordering and range limit (NRW API requires date range < 1 year)
  if (!is.null(start_dt) && !is.null(end_dt)) {
    if (start_dt > end_dt) {
      stop("`start` date cannot be after `end` date.")
    }
    if (as.numeric(difftime(end_dt, start_dt, units = "days")) >= 365) {
      stop("The date range must cover less than one year.")
    }
  }

  # Initialize query parameters with the measure ID
  params <- list(measureId = measure)

  if (!is.null(start_dt) && !is.null(end_dt)) {
    params$start <- format(start_dt, "%Y-%m-%dT%H:%M:%S")
    params$end <- format(end_dt, "%Y-%m-%dT%H:%M:%S")
  }

  # Make the API request
  measure_data <- nrw_api_request(endpoint, api_key, params)

  # Return an empty data.frame with proper column types if no data was returned
  empty_df <- data.frame(
    measureId = character(0),
    quality = integer(0),
    timestamp = as.POSIXct(character(0), tz = "UTC"),
    value = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(measure_data) || length(measure_data) == 0 || (is.data.frame(measure_data) && nrow(measure_data) == 0)) {
    return(empty_df)
  }

  # Format datetime in UTC and value as numeric
  measure_data$timestamp <- as.POSIXct(measure_data$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  measure_data$value <- as.numeric(measure_data$value)

  # Return the retrieved data
  return(measure_data)
}


#' Return all available metadata
#'
#' @param api_key String. NRW Telemetry API key. String.
#' @description
#' Returns a list with the following metadata queries: measurement areas, station statuses, operating areas, local authorities and measure types.
#'
#' @return list of length 5 with data.frames of all metadata queries
#' @export
nrw_api_meta <- function(api_key) {
  # Set the API endpoint
  base <- "/api/"
  endpoint_areas <- "measurement-areas"
  endpoint_statuses <- "statuses"
  endpoint_op_areas <- "operating-areas"
  endpoint_LA <- "local-authorities"
  endpoint_measure_types <- "measure-types"

  endpoints <- list(endpoint_areas, endpoint_statuses, endpoint_op_areas, endpoint_LA, endpoint_measure_types)
  endpoints_req <- as.list(paste0(base, endpoints))

  metadata <- lapply(endpoints_req, nrw_api_request, api_key)
  names(metadata) <- endpoints

  return(metadata)
}

