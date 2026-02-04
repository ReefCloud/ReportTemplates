# =============================================================
# File: get_regional_summary.R
# Description: Retrieves summary metadata from the ReefCloud API for a tier ID
# Author: Manuel Gonzalez-Rivero and Samuel Chan
# Date: 2026-02-03
# Dependencies: httr, jsonlite
# =============================================================

#' Get Regional Summary from ReefCloud API
#'
#' Retrieves summary metadata for a ReefCloud tier ID, including region name,
#' site counts, image counts, contributors, and site identifiers.
#'
#' @param tier_id Character scalar. ReefCloud Tier ID.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{region_name}
#'   \item \code{site_count}
#'   \item \code{photo_quadrats}
#'   \item \code{data_contributors}
#'   \item \code{site_id}
#'   \item \code{source}
#' }
#'
#' @details
#' Sends a GET request to the ReefCloud Dashboard API and parses the JSON response.
#' Fails early with informative errors if the request or response is invalid.
#'
#' @examples
#' \dontrun{
#' summary <- get_regional_summary("1705")
#' str(summary)
#' }
#'
#' @export
get_regional_summary <- function(tier_id) {
  
  # ---- Input validation ----
  if (missing(tier_id) || length(tier_id) != 1 || is.na(tier_id)) {
    stop("`tier_id` must be a single, non-NA value.", call. = FALSE)
  }
  tier_id <- as.character(tier_id)
  
  # ---- Build request ----
  url <- paste0(
    "https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/",
    tier_id
  )
  
  resp <- httr::GET(
    url,
    httr::timeout(30)  # avoids hanging sessions
  )
  
  # ---- HTTP status handling ----
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "ReefCloud API request failed [%s]: %s",
        httr::status_code(resp),
        httr::http_status(resp)$message
      ),
      call. = FALSE
    )
  }
  
  # ---- Parse JSON safely ----
  txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  
  if (is.null(json$data)) {
    stop("Malformed API response: missing `data` field.", call. = FALSE)
  }
  
  data <- json$data
  
  # ---- Return normalized output ----
  list(
    region_name       = data$name            %||% NA_character_,
    site_count        = data$site_count      %||% NA_integer_,
    photo_quadrats    = data$image_count     %||% NA_integer_,
    data_contributors = data$contributors    %||% NA_character_,
    site_id           = data$site_id         %||% NA_character_,
    source            = "www.reefcloud.ai"
  )
}

# Null-coalescing operator (reuse project-wide)
`%||%` <- function(x, y) if (is.null(x)) y else x