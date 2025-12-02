# =============================================================
# File: get_regional_summary.r
# Description: Retrieves summary metadata from the ReefCloud API for a region using the Tier ID
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite
# =============================================================

#' Get Regional Summary from ReefCloud API
#'
#' Retrieves a summary of regional data for a specified tier ID from the ReefCloud API.
#' Returns a list with region name, site count, photo quadrats, data contributors, site ID, and data source.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#'
#' @return A list containing:
#'   \item{region_name}{The name of the region.}
#'   \item{site_count}{The number of surveyed sites in the region.}
#'   \item{photo_quadrats}{The number of photo quadrats in the region.}
#'   \item{data_contributors}{The identifiers of data owners for this region.}
#'   \item{site_id}{The ID for each surveyed site within the region.}
#'   \item{source}{The source of the data, which is "www.reefcloud.ai".}
#'
#' @details
#' The function sends a GET request to the ReefCloud dashboard API, parses the JSON response,
#' and returns a summary list. Requires the `httr` and `jsonlite` packages.
#'
#' @examples
#' # Example usage:
#' summary <- get_regional_summary("exampleTierID")
#' print(summary)
#' 
#' @export


get_regional_summary <- function(tier_id){
	require(httr)
	require(jsonlite)
	url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/", tier_id)
	response <- httr::GET(url)
	data <- jsonlite::fromJSON(content(response, "text", encoding="UTF-8"))
	info <- list(
		region_name = data$data$name,
		site_count = data$data$site_count,
		photo_quadrats = data$data$image_count,
		data_contributors = data$data$contributors,
		site_id = data$data$site_id,
		source = "www.reefcloud.ai"
	)
	return(info)
}
