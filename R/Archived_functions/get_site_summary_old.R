# =============================================================
# File: get_site_summary.r
# Description: Retrieves site summaries (longitude, latitude) for a set of site IDs from the ReefCloud API and returns a spatial data frame.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, sf
# =============================================================

#' Retrieve Site Summaries from ReefCloud API
#'
#' This function fetches site summary (longitude, latitude) for a set of site IDs from the ReefCloud API
#' and returns a spatial data frame (sf object) with site coordinates.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#'
#' @return An sf spatial data frame containing longitude and latitude for each site.
#'
#' @details
#' The function iterates over the provided site IDs, queries the ReefCloud API for each site,
#' and combines the results into a spatial data frame. Requires the `httr`, `jsonlite`, `dplyr`, and `sf` packages.
#'
#' @examples
#' # Example usage:
#' sites_sf <- get_site_summary("exampleTierID")
#' print(sites_sf)
#' 
#' @export

get_site_summary <- function(tier_id) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(sf)

  source("R/API_functions/get_regional_summary.r")
  info <- get_regional_summary(tier_id = tier_id)
  sites <- data.frame(site_longitude = numeric(), 
                      site_latitude = numeric(), 
                      local_region = character(),
                      stringsAsFactors = FALSE)
  for (i in info$site_id) {
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/sites/", i)
    response <- httr::GET(url)
    data <- jsonlite::fromJSON(content(response, "text"))
    data <- lapply(data$data, function(x) if (is.null(x)) NA else x) # Change null values to NAs 
    site <- as.data.frame(data, stringsAsFactors = FALSE)
    sites <- dplyr::bind_rows(sites, site)
  }
  sites <- sf::st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326)
  # Return a spatial dataframe containing the longitude and latitude of the sites
  return(sites)
  }

