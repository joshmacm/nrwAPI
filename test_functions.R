library(nrwAPI)

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


nrw_api_request_2 <- function(endpoint, api_key, retries = 3) {
  baseurl <- "https://api.naturalresources.wales/telemetry"
  url <- paste0(baseurl, endpoint)

  for (i in 1:retries) {
    result <- tryCatch({
      # Make the API request
      response <- httr::GET(url,
                            httr::add_headers(`Ocp-Apim-Subscription-Key` = api_key),
                            httr::timeout(60))  # Increased timeout

      # Check for non-200 status codes
      if (httr::status_code(response) != 200) {
        stop("API request failed with status: ", httr::status_code(response))
      }

      # Parse the JSON response and return it
      return(jsonlite::fromJSON(httr::content(response, as = "text")))
    }, error = function(e) {
      message("Attempt ", i, " failed: ", e$message)
      return(NULL)
    })

    # If successful, return the result
    if (!is.null(result)) {
      return(result)
    }

    Sys.sleep(5)  # Wait 5 seconds before retrying
  }

  # Stop if all retries failed
  stop("Failed after ", retries, " attempts.")
}


