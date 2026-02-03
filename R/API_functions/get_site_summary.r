# =============================================================
# File: get_site_summary.r
# Description: Retrieves site summaries (longitude, latitude) for a set of site IDs from the ReefCloud API and returns a spatial data frame.
# Author: Manuel Gonzalez-Rivero and Samuel Chan
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, sf
# =============================================================#' Retrieve Site Summaries from ReefCloud API
#'
#' Fetches site summary (longitude, latitude, local region) for a set of site IDs
#' from the ReefCloud API and returns an sf object with site coordinates.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#'
#' @return An sf spatial data frame (EPSG:4326) containing longitude and latitude for each site.
#' @details Requires httr, jsonlite, dplyr, sf, purrr.
#' @examples
#' # sites_sf <- get_site_summary("exampleTierID")
#' # print(sites_sf)
#' @export

get_site_summary <- function(tier_id) {
  # Input validation
  stopifnot(is.character(tier_id), length(tier_id) == 1, !is.na(tier_id), nzchar(tier_id))

  # Get site list for the tier/region
  # Prefer using your package import: get_regional_summary()
  info <- get_regional_summary(tier_id = tier_id)

  if (!"site_id" %in% names(info) || nrow(info) == 0) {
    stop("No sites found for the provided tier_id.")
  }

  # Helper to safely fetch a single site
  fetch_one <- function(site_id) {
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/sites/", site_id)
    resp <- httr::GET(
      url,
      httr::timeout(30)
    )

    if (httr::http_error(resp)) {
      # Return a row of NAs but keep id to avoid losing record
      return(dplyr::tibble(
        site_longitude = NA_real_,
        site_latitude  = NA_real_,
        local_region   = NA_character_
      ))
    }

    # Explicitly set encoding to avoid: "No encoding supplied: defaulting to UTF-8."
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")

    # Parse JSON
    dat <- jsonlite::fromJSON(txt, simplifyVector = TRUE)

    # Flatten and null-protect
    row <- dat$data
    if (is.null(row)) {
      return(dplyr::tibble(
        site_longitude = NA_real_,
        site_latitude  = NA_real_,
        local_region   = NA_character_
      ))
    }

    # Normalize fields
    site_longitude <- if (is.null(row$site_longitude)) NA_real_ else as.numeric(row$site_longitude)
    site_latitude  <- if (is.null(row$site_latitude))  NA_real_ else as.numeric(row$site_latitude)
    local_region   <- if (is.null(row$local_region))   NA_character_ else as.character(row$local_region)

    dplyr::tibble(
      site_longitude = site_longitude,
      site_latitude  = site_latitude,
      local_region   = local_region
    )
  }

  # Vectorized fetch over site IDs
  sites <- purrr::map_dfr(info$site_id, fetch_one)

  # Convert to sf
  sites_sf <- sf::st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326, remove = TRUE)

  sites_sf
}