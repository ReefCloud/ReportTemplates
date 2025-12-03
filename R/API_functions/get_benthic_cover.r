# =============================================================
# File: get_benthic_cover.r
# Description: Retrieves modelled estimations of benthic cover trends from the ReefCloud API.
# Author: Manuel Gonzalez-Rivero and Samuel Chan
# Date: 2025-11-13
# Dependencies: httr, jsonlite, dplyr, tidyr, lubridate
# =============================================================

#' Retrieve Benthic Cover Trend Data for One or Multiple Tiers from ReefCloud API
#'
#' This function fetches and processes survey data for one or more specified tiers from the ReefCloud dashboard API.
#' It returns a tidy data frame containing estimated cover (percent) and credible intervals for each surveyed year,
#' major functional groups, and tier identifier.
#'
#' @param tier_ids Character vector. One or more unique identifiers for the region/tier(s) to query.
#'
#' @return A data frame containing the survey compositions for the specified tier(s).
#'         The data frame excludes the survey ID, unnests the compositions field, and includes a `tier_id` column.
#'
#' @details
#' The function sends a GET request to the ReefCloud dashboard API for each tier ID, parses the JSON response,
#' and returns the combined survey data as a tidy data frame. Requires the `httr`, `jsonlite`, `dplyr`, `tidyr`, and `lubridate` packages.
#'
#' If a tier ID returns no data or the API request fails, a warning is issued and that tier is skipped.
#'
#' @examples
#' # Example usage:
#' # Single tier
#' surveys_single <- get_benthic_cover("exampleTierID")
#' print(surveys_single)
#'
#' # Multiple tiers
#' surveys_multi <- get_benthic_cover(c("tierID1", "tierID2"))
#' print(surveys_multi)
#' 
#' @export

get_benthic_cover <- function(tier_ids) {
  
  all_surveys <- lapply(tier_ids, function(tier_id) {
    url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/surveys/%s", tier_id)
    response <- httr::GET(url, httr::timeout(30))
    
    if (httr::status_code(response) != 200) {
      warning("API request failed for tier_id: ", tier_id, " (status: ", httr::status_code(response), ")")
      return(NULL)
    }
    
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    
    if (is.null(data$data) || nrow(as.data.frame(data$data)) == 0) {
      warning("No survey data found for tier_id: ", tier_id)
      return(NULL)
    }
    
    surveys <- as.data.frame(data$data) |>
      dplyr::select(-id) |>
      tidyr::unnest(compositions) |>
      dplyr::mutate(year = lubridate::year(date),
                    tier_id = tier_id) |>  # Add tier_id for reference
      dplyr::relocate(year, .after = date) |>
      dplyr::relocate(tier_id, .after = tier_level) |> 
      dplyr::mutate(depth_cat = dplyr::case_when(
        depth == "deep_gt_5m" ~ "deep",
        depth == "deep_lt_5m" ~ "shallow",
        depth == "no_depth" ~ NA
      ))
    
    return(surveys)
  })
  
  # Combine all non-null results
  combined <- dplyr::bind_rows(all_surveys)
  return(combined)
}
