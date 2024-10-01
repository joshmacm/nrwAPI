library(jsonlite)
library(httr)


#' NRW Telemetry API Request for custom endpoints and parameters
#'
#' This function sends a GET request to the NRW Telemetry API with support for query parameters and basic error handling.
#' @param endpoint String for specific endpoint of the api.
#'
#' @param api_key NRW Telemetry API key. String.
#' @param query_params A list of key-value pairs for query parameters. Parameters must be strings.
#' @importFrom utils URLencode
#' @returns Parsed JSON response from the API
#' @export
#'
nrw_api_request <- function(endpoint, api_key, query_params = list()) {
  base_url <- "https://api.naturalresources.wales/telemetry"
  url <- paste0(base_url, endpoint)

  # Add query parameters if provided
  if (length(query_params) > 0) {
    url <- paste0(url, "?", paste0(names(query_params), "=", query_params, collapse = "&"))
  }

  response <- httr::GET(url, httr::add_headers("Ocp-Apim-Subscription-Key" = api_key), httr::timeout(60))

  if (response$status_code == 200) {
    return(jsonlite::fromJSON(httr::content(response, as = "text")))
  } else {
    stop("API request failed with status: ", response$status_code)
  }
}
