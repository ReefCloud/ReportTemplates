# =============================================================
# File: get_tier_summary.r
# Description: Retrieves tier summaries for a tier ID from the ReefCloud API and returns a spatial data frame.
# Author: Samuel Chan
# Date: 2026-03-02
# Dependencies: httr, jsonlite, dplyr, sf
# =============================================================
#' Retrieve Tier Summary from ReefCloud API
#'
#' Fetches tier summary (longitude, latitude, local region) for a tier ID
#' from the ReefCloud API and returns an sf object with tier boundaries
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#'
#' @return An sf spatial data frame (EPSG:4326) containing boundaries for the tier.
#' @details Requires httr, jsonlite, dplyr, sf, purrr.
#' @examples
#' # tier_sf <- get_tier_summary("exampleTierID")
#' # print(tier_sf)
#' @export

get_tier_summary <- function(tier_id) {
  
  # load helpers
  source("R/get_bbox.R")
  
  # Get the tier
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/", tier_id)
    resp <- httr::GET(
      url,
      httr::timeout(30)
    )
    
    if (httr::http_error(resp)) {
      # Return a row of NAs but keep id to avoid losing record
      return(dplyr::tibble(
        tier_id        = NA_real_,
        tier_level     = NA_real_,
        name           = NA_character_,
        site_latitude  = NA_real_,
        site_longitude = NA_real_,
        site_count     = NA_real_    ))
    }
    
    # Explicitly set encoding to avoid: "No encoding supplied: defaulting to UTF-8."
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # Parse JSON
    dat <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
    
    # Flatten and null-protect
    row <- dat$data
    if (is.null(row)) {
      return(dplyr::tibble(
        tier_id        = NA_real_,
        tier_level     = NA_real_,
        name           = NA_character_,
        site_latitude  = NA_real_,
        site_longitude = NA_real_,
        site_count     = NA_real_ 
      ))
    }
    
    # Normalize fields
    tier_id        <- if (is.null(row$tier_id)) NA_real_ else as.numeric(row$tier_id) 
    tier_level     <- if (is.null(row$tier_level)) NA_real_ else as.numeric(row$tier_level)
    name           <- if (is.null(row$name)) NA_character_ else as.character(row$name)
    site_latitude  <- if (is.null(row$site_latitude))   NA_real_ else as.numeric(row$site_latitude)
    site_longitude <- if (is.null(row$site_longitude))  NA_real_ else as.numeric(row$site_longitude)
    site_count     <- if (is.null(row$site_count)) NA_real_ else as.numeric(row$site_count)
    
    tdf <- dplyr::tibble(
      tier_id        = tier_id,
      tier_level     = tier_level,
      tier_name      = name,
      tier_latitude  = site_latitude,
      tier_longitude = site_longitude,
      site_count     = site_count
    )
  
  bbox <- get_bbox(tier_id)
  
  # Convert to sf poly
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(bbox$bbox), crs = 4326)
  tier_sf <- 
    sf::st_sf(
      tdf, 
      geometry = bbox_sf)
  
  tier_sf
}