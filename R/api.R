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
    type <- URLencode(type)
    params$measureType <- type
  }
  # Make the API request
  measures_data <- nrw_api_request(endpoint, api_key, params)

  return(measures_data)
}


#' Return Values for a Specific Measure
#' @param api_key NRW Telemetry API key. String.
#' @description
#' Returns values for a specific measure code. A time period can be provided to narrow the response. If no start or end date is provided then the default period is the preceding 24 hours from last value.
#'
#' @param measure String. Measure_id as given by nrw_station_measures.
#' @param start Optional. Date/Datetime object. Start of period.
#' @param end Optional. Date/Datetime object. End of period, leave blank to use current datetime.
#' @returns data.frame with measureId, quality code, timestamp, value
#' @export
nrw_measure_values <- function(api_key, measure, start = NULL, end = NULL) {
  # Define the API endpoint
  endpoint <- "/api/readings"
  measure <- URLencode(measure)

  # Initialize query parameters with the measure ID
  params <- list(measureId = measure)

  # If both start and end dates are provided, add them to the parameters
  if (!is.null(start) & !is.null(end)) {
    start <- format(start, "%Y-%m-%dT%H:%M:%S")
    end <- format(end, "%Y-%m-%dT%H:%M:%S")
    params$start <- start
    params$end <- end
  }

  # If only the start date is provided, use the current time as the end date
  if (!is.null(start) & is.null(end)) {
    start <- format(start, "%Y-%m-%dT%H:%M:%S")
    params$start <- start
    params$end <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")  # Format current time as a string
  }

  # Make the API request
  measure_data <- nrw_api_request(endpoint, api_key, params)

  # Format datetime
  measure_data$timestamp <- as.POSIXct(measure_data$timestamp)

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

