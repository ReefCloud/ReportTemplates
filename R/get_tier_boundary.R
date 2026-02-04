# =============================================================
# File: get_tiers_frombbox.r
# Description: Retrieves spatial tier boundaries from the ReefCloud API based on tier level and bounding box.
# Author: Samuel Chan
# Date: 2025-11-25
# Dependencies: sf
# =============================================================

#' Retrieve Tier IDs and boundaries from ReefCloud API
#'
#' Retrieves spatial tier boundaries from the ReefCloud API based on a specified tier level and bounding box.
#'
#' @param tier_level Integer. The tier level to retrieve (must be between 2 and 6, default = 4).
#' @param bbox A named list or vector containing bounding box coordinates.
#'
#' @return An `sf` object representing the tier boundaries.
#' 
#' @details
#' The function gets all tier ids for a specified tier level and bounding box using the ReefCloud API.
#' Tier levels defaults to 4 (region) but can be from 2 (country) to 6 (site).
#' 
#' @examples
#' # Example usage:
#' bbox <- list(xmin = 142, ymin = -20, xmax = 146, ymax = -18)
#' tiers_sf <- get_tiers_frombbox(tier_level = 4, bbox = bbox)
#' print(tiers_sf)
#' }
#'
#' @export

get_tiers_frombbox <- function(tier_level = 4, bbox) {
  
  # Validate bbox using helper
  source("R/validate_bbox.R")
  validate_bbox(bbox)
  
  # Construct URL
  url <- sprintf(
    "https://api.reefcloud.ai/reefcloud/dashboard-api/tiers?tier_level=%s&xmin=%s&ymin=%s&xmax=%s&ymax=%s",
    tier_level, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax
  )
  
  # Read spatial data
  boundary <- tryCatch(
    sf::st_read(url, quiet = TRUE),
    error = function(e) {
      stop("Failed to retrieve data from ReefCloud API: ", e$message)
    }
  )
  
  return(boundary)
}
