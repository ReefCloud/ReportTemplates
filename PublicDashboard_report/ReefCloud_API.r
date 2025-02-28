library(sf)
library(httr)
library(jsonlite)
library(gt)

# Miscellaneous Functions to use ReefCloud API and fetch data from the public dashboard

##Get regional summary ######
#' Get Regional Summary
#'
#' This function retrieves a summary of regional data from the ReefCloud API based on the provided tier ID.
#'
#' @param tierID A character string representing the tier ID for which the regional summary is to be retrieved.
#' @return A list containing the following elements:
#' \item{region_name}{The name of the region.}
#' \item{site_count}{The number of sites in the region.}
#' \item{photo_quadrats}{The number of photo quadrats in the region.}
#' \item{data_contributors}{The number of data contributors in the region.}
#' \item{site_id}{The site ID associated with the region.}
#' \item{source}{The source of the data, which is "www.reefcloud.ai".}
#' @examples
#' \dontrun{
#'   summary <- getRegionalSummary("exampleTierID")
#'   print(summary)
#' }
#' @export
getRegionalSummary <- function(tierID){
url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/", tierID)
response <- GET(url)
data <- fromJSON(content(response, "text"))
info <- list(region_name=data$data$name, site_count=data$data$site_count, photo_quadrats=data$data$image_count, data_contributors=data$data$contributors, site_id=data$data$site_id, source="www.reefcloud.ai")
return(info)
}

## Get Sites Information #######
#'
#' This function retrieves information about monitoring sites within a region (tier) from the ReefCloud API.
#'
#' @param info A list containing information about the tier from getRegionalSummary function.
#' @return A spatial dataframe containing the longitude and latitude of the sites.
#' @examples
#' \dontrun{
#'   sites_info <- get_sites_info(tier_info)
#'   print(sites_info)
#' }
#' @export
get_sites_info <- function(info) {
  sites <- data.frame(site_longitude = numeric(), site_latitude = numeric(), stringsAsFactors = FALSE)
  for (i in info$site_id){
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/sites/", i)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    site <- as.data.frame(data$data)
    sites <- bind_rows(sites, site)
    }
  sites <- st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326)
  # Return a spatial dataframe containing the longitude and latitude of the sites
  sites
}
