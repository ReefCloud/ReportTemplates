# =============================================================
# File: get_disturbance.r
# Description: Retrieves environmental disturbance data (thermal stress, storm exposure) for a specified region from the ReefCloud API.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, lubridate
# =============================================================

#' Retrieve Environmental Disturbance Data from ReefCloud API
#'
#' This function fetches environmental disturbance data (thermal stress or storm exposure) for a specified region (tier) from the ReefCloud API.
#' Returns a data frame with disturbance details and the corresponding year.
#'
#' @param tier_id Character. Unique identifier for the region/tier to query.
#' @param e_type Character. Type of environmental disturbance to retrieve. Options: "thermal_stress", "storm_exposure_year" or "storm_exposure_event".
#'
#' @return Data frame containing disturbance data for the specified region and event type, with an added 'Year' column.
#'
#' @details
#' The function sends a GET request to the ReefCloud dashboard API, parses the JSON response,
#' and returns a tidy data frame. Requires the `httr`, `jsonlite`, `dplyr`, and `lubridate` packages.
#'
#' @examples
#' # Example usage:
#' disturbances <- get_disturbance("your_tier_id", "thermal_stress")
#' head(disturbances)
#' 
#' @export

get_disturbance <- function(tier_id, e_type) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(lubridate)

  url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/environmental/%s?env_type=%s", 1705, "thermal_stress")
  response <- httr::GET(url)
  data <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))
  data <- data$data %>%
    mutate(year = year(start_date))
  return(data)
}
