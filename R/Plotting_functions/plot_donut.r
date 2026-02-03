
# =============================================================
# File: plot_donut_cover.R
# Description: Builds benthic data from ReefCloud API (regional summary + benthic cover),
#              defaults to HARD CORAL, classifies into range bins, and plots a whole-donut chart.
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: ggplot2, dplyr, tidyr, rlang
# Requires: get_benthic_cover(), get_regional_summary(), add_cover_categories(), palette_for()
# =============================================================

#' Plot Donut for Benthic Cover (Defaults to HARD CORAL)
#'
#' Retrieves survey compositions via \code{get_benthic_cover()}, filters to a requested
#' benthic \code{cover_type} (default \code{"HARD CORAL"}), classifies cover into ranges
#' using \code{add_cover_categories()}, and renders a whole-donut plot. Uses
#' \code{get_regional_summary()} to add a center label (region name and sites).
#'
#' @param tier_id Character. ReefCloud tier ID to query.
#' @param cover_type Character. Benthic group to plot (e.g., \code{"HARD CORAL"}, \code{"MACROALGAE"}, \code{"SOFT CORAL"}).
#'   Default \code{"HARD CORAL"}.
#' @param show_labels Logical. Show per-segment labels (percentages or counts). Default \code{FALSE}.
#' @param label_format Character. Label type: \code{"percent"} or \code{"count"}. Default \code{"percent"}.
#' @param donut_width Numeric in (0, 1]. Ring thickness (e.g., \code{0.6}). Default \code{0.6}.
#'
#' @return A \code{ggplot} donut chart.
#'
#' @details
#' - Detects the benthic group column among \code{c("type", "major_functional_group", "functional_group", "group", "category")}.
#' - Classifies using the \code{"median"} cover column; adjust if you need \code{"mean"} etc.
#' - Palette auto-maps from \code{cover_type} to \code{hc/ma/sc} (case-insensitive).
#' - Factor order follows \code{add_cover_categories()} (descending for clean legends).
#'
#' @examples
#' # Default HARD CORAL donut for a tier:
#' # p <- plot_donut("exampleTierID")
#' # print(p)
#'
#' # Show labels:
#' # p <- plot_donut("exampleTierID", cover_type = "MACROALGAE", show_labels = TRUE)
#'
#' @export
plot_donut <- function(
    tier_id,
    cover_type   = "HARD CORAL",
    show_labels  = FALSE,
    label_format = c("percent", "count"),
    donut_width  = 0.6
) {
  
  # ---- Load external helper functions and palettes ----
  source("R/API_functions/get_benthic_cover.r")
  source("R/API_functions/get_regional_summary.r")
  source("R/Plotting_functions/add_cover_categories.r")
  source("R/Plotting_functions/load_plot_palette.r")
  
  # ---- Validate inputs ----
  if (missing(tier_id) || length(tier_id) != 1) {
    stop("`tier_id` must be a single character ID.")
  }
  if (!is.character(cover_type) || length(cover_type) != 1) {
    stop("`cover_type` must be a single string (e.g., 'HARD CORAL').")
  }
  label_format <- match.arg(label_format)
  if (!is.numeric(donut_width) || !is.finite(donut_width) || donut_width <= 0 || donut_width > 1) {
    stop("`donut_width` must be a numeric value in (0, 1].")
  }
  
  # ---- Fetch info for center label ----
  info <- tryCatch(
    get_regional_summary(tier_id),
    error = function(e) NULL
  )
  
  # ---- Retrieve and filter benthic data for the requested cover type ----
  surveys <- tryCatch(
    get_benthic_cover(tier_id),
    error = function(e) stop("Failed to retrieve surveys via get_benthic_cover(): ", conditionMessage(e))
  )
  if (is.null(surveys) || !is.data.frame(surveys) || nrow(surveys) == 0) {
    stop("No survey data returned from get_benthic_cover().")
  }
  
  # Detect group column (robust)
  candidate_group_cols <- c("type", "major_functional_group", "functional_group", "group", "category")
  group_col <- candidate_group_cols[candidate_group_cols %in% names(surveys)][1]
  if (is.na(group_col)) {
    stop("Could not detect a benthic group column. Expected one of: ",
         paste(candidate_group_cols, collapse = ", "), ".")
  }
  
  # Filter to requested cover_type (case-insensitive compare)
  xdf <- dplyr::filter(
    surveys,
    tolower(.data[[group_col]]) == tolower(cover_type)
  )
  if (nrow(xdf) == 0) {
    stop(sprintf("No rows found for cover_type='%s' in column '%s'.", cover_type, group_col))
  }
  
  # ---- Classify into range-based categories using median cover ----
  xdf <- add_cover_categories(xdf, column = "median")  # adds `cover_prop` + `cover_cat`
  
  # ---- Summarise: number of rows per range (proxy for site counts) ----
  # Use factor levels from add_cover_categories() (descending order)
  range_levels <- levels(xdf$cover_prop)
  if (is.null(range_levels)) {
    range_levels <- c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%")
  }
  
  xdf_sum <- xdf |>
    dplyr::count(cover_prop, name = "Site_No") |>
    dplyr::mutate(
      cover_prop = factor(cover_prop, levels = range_levels, ordered = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct = if (sum(Site_No) > 0) 100 * Site_No / sum(Site_No) else 0
    )
  
  # ---- Palette selection based on cover_type ----
  # Map cover_type to palette group (case-insensitive)
  ct_upper <- toupper(cover_type)
  group_map <- list(
    "HARD CORAL"  = "hc",
    "MACROALGAE"  = "ma",
    "SOFT CORAL"  = "sc"
  )
  gp <- group_map[[ct_upper]]
  if (is.null(gp)) gp <- "hc"  # sensible fallback
  
  pal <- palette_for(group = gp, use = "prop")
  
  # Validate palette names vs. factor levels
  missing_names <- setdiff(range_levels, names(pal))
  if (length(missing_names) > 0) {
    stop(sprintf(
      "Palette missing colors for: %s. Ensure palette names match range levels.",
      paste(missing_names, collapse = ", ")
    ))
  }
  
  # ---- Build whole-donut plot ----
  xdf_sum$x <- 1
  
  p <- ggplot2::ggplot(xdf_sum, ggplot2::aes(x = x, y = Site_No, fill = cover_prop)) +
    ggplot2::geom_col(width = donut_width, color = NA) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::theme_void()
  
  # Segment labels (optional)
  if (isTRUE(show_labels)) {
    labs <- if (label_format == "percent") paste0(round(xdf_sum$pct), "%") else as.character(xdf_sum$Site_No)
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = labs),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 4
    )
  }
  
  # Center label (region name and sites if available)
  center_text <- NULL
  if (!is.null(info) && is.list(info)) {
    nm <- info$region_name
    sc <- info$site_count
    if (!is.null(nm) && !is.null(sc)) {
      center_text <- paste0(nm, "\nSites: ", sc)
    } else if (!is.null(nm)) {
      center_text <- nm
    }
  }
  if (is.null(center_text)) {
    center_text <- cover_type
  }
  
  p <- p + ggplot2::annotate(
    "text", x = 0, y = 0,
    label = center_text,
    size = 6, fontface = "bold", color = "#333333"
  )
  
  return(p)
}
