
# =============================================================
# File: API_get_region_info.r
# Description: Retrieves a summary of the metadata from a region (tier) using the ReefCloud API.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: httr, jsonlite
# =============================================================

#' Get Regional Summary from ReefCloud API
#'
#' Retrieves a summary of regional data for a specified tier ID from the ReefCloud API.
#' Returns a list with region name, site count, photo quadrats, data contributors, site ID, and data source.
#'
#' @param tierID Character. The unique identifier for the region/tier to query.
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
#' summary <- getRegionalSummary("exampleTierID")
#' print(summary)

getRegionalSummary <- function(tierID){
	require(httr)
	require(jsonlite)
	url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/", tierID)
	response <- GET(url)
	data <- fromJSON(content(response, "text", encoding="UTF-8"))
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