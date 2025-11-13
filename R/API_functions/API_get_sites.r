# =============================================================
# File: API_get_sites.r
# Description: Retrieves site information (longitude, latitude) for a set of site IDs from the ReefCloud API and returns a spatial data frame.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, sf
# =============================================================

#' Retrieve Site Information as Spatial Data Frame
#'
#' This function fetches site information (longitude, latitude) for a set of site IDs from the ReefCloud API
#' and returns a spatial data frame (sf object) with site coordinates.
#'
#' @param info List. Must contain a vector of site IDs in info$site_id.
#'
#' @return An sf spatial data frame containing longitude and latitude for each site.
#'
#' @details
#' The function iterates over the provided site IDs, queries the ReefCloud API for each site,
#' and combines the results into a spatial data frame. Requires the `httr`, `jsonlite`, `dplyr`, and `sf` packages.
#'
#' @examples
#' # Example usage:
#' info <- list(site_id = c("site1", "site2"))
#' sites_sf <- get_sites_info(info)

get_sites_info <- function(tier_id) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(sf)

  source("R/API_functions/API_get_region_info.r")
  info <- getRegionalSummary(tierID = tier_id)
  sites <- data.frame(site_longitude = numeric(), site_latitude = numeric(), stringsAsFactors = FALSE)
  for (i in info$site_id) {
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/sites/", i)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    site <- as.data.frame(data$data)
    sites <- bind_rows(sites, site)
  }
  sites <- st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326)
  # Return a spatial dataframe containing the longitude and latitude of the sites
  return(sites)
}
