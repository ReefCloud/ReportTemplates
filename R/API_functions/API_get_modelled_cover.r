# =============================================================
# File: API_get_modelled_cover.r
# Description: Retrieves modelled estimations of regional cover trends from the ReefCloud API.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, tidyr
# =============================================================

#' Retrieve Regional Cover Trend Data from ReefCloud API
#'
#' This function fetches and processes survey data for a specified region (tier) from the ReefCloud dashboard API.
#' It returns a data frame containing estimated cover (percent) and credible intervals for each surveyed year and major functional groups.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#'
#' @return A data frame containing the survey compositions for the specified region.
#'         The data frame excludes the survey ID and unnests the compositions field.
#'
#' @details
#' The function sends a GET request to the ReefCloud dashboard API, parses the JSON response,
#' and returns the survey data as a tidy data frame. Requires the `httr`, `jsonlite`, `dplyr`, and `tidyr` packages.
#'
#' @examples
#' # Example usage:
#' surveys <- get_region_covers("your_tier_id")
#'
get_region_covers <- function(tier_id) {
    require(httr)
    require(jsonlite)
    require(dplyr)
    require(tidyr)

  url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/surveys/%s", tier_id)
  response <- GET(url)
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response))
  }
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  surveys <- as.data.frame(data$data) |>
    select(-id) |>
    unnest(compositions)
  return(surveys)
}