# =============================================================
# File: plot_half_donut.R
# Description: Generates and saves a half-donut plot for a specified benthic range category using ReefCloud data.
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: ggplot2, dplyr, tidyr, stringr
# =============================================================

#' Plot Half Donut Showing Number of Sites by Cover Range
#'
#' Retrieves benthic cover data for a specified tier from ReefCloud, classifies sites into
#' \strong{range-based} categories via \code{cover_prop} (e.g., \code{"50-100%"}), and plots a half-donut chart
#' showing the number of sites in each range. The half-donut can be oriented left or right.
#' Internally, a transparent "spacer" slice equal to the total site count is added
#' so real slices occupy exactly 180° (a true semicircle).
#'
#' @param tier_id Character. Unique identifier for the region/tier to query.
#' @param cover_type Character. Benthic cover type to classify (e.g., `"HARD CORAL"`, `"MACROALGAE"`, `"SOFT CORAL"`). Default `"HARD CORAL"`.
#' @param side Character. Position of the half donut: `"left"` or `"right"`. Default `"left"`.
#'
#' @return A `ggplot` object representing the half-donut chart.
#'
#' @details
#' - Uses \code{cover_prop} (ranges) for fill and legend.
#' - Color palettes are keyed to range labels; see \code{hc.pal}, \code{ma.pal}, \code{sc.pal}.
#' - Zero-count ranges are included for completeness.
#'
#' @examples
#' \dontrun{
#' p1 <- plot_half_donut(tier_id = "exampleTierID", cover_type = "HARD CORAL", side = "left")
#' p2 <- plot_half_donut(tier_id = "exampleTierID", cover_type = "MACROALGAE", side = "right")
#' }
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_polar xlim scale_fill_manual geom_text labs theme_void theme annotate element_text ggsave position_stack
#' @importFrom dplyr filter count mutate if_else bind_rows
#' @importFrom tidyr complete
#' @importFrom stringr str_replace_all str_to_title
plot_half_donut <- function(tier_id, cover_type = "HARD CORAL", side = "left") {
  
  # ---- Load external helper functions and palettes ----
  source("R/API_functions/get_benthic_cover.r")
  source("R/API_functions/get_regional_summary.r")
  source("R/Plotting_functions/add_cover_categories.r")
  source("R/Plotting_functions/load_plot_palette.r")
  
  # ---- Fetch info for titles/captions ----
  info <- get_regional_summary(tier_id)
  
  # ---- Retrieve and filter benthic data for the requested cover type ----
  xdf <- get_benthic_cover(tier_id) |>
    dplyr::filter(type == cover_type)
  
  # ---- Classify into range-based categories using median cover ----
  xdf <- add_cover_categories(xdf, column = "median")
  
  # ---- Summarise: number of sites per range; include all ranges ----
  range_levels <- c("50-100%", "30-<50%", "10%-<30%", "0-<10%")
  xdf_sum <- xdf |>
    dplyr::count(cover_prop, name = "Site_No") |>
    dplyr::mutate(cover_prop = factor(cover_prop, levels = range_levels, ordered = TRUE)) |>
    tidyr::complete(
      cover_prop = factor(range_levels, levels = range_levels, ordered = TRUE),
      fill = list(Site_No = 0)
    )
  
  total_sites <- sum(xdf_sum$Site_No, na.rm = TRUE)
  
  # ---- Build a transparent spacer slice to form an exact semicircle ----
  all_levels <- c(range_levels, "Spacer")
  
  spacer <- dplyr::tibble(
    cover_prop = factor("Spacer", levels = all_levels, ordered = TRUE),
    Site_No    = total_sites
  )
  
  # ---- Make sure xdf_sum uses the same expanded levels (ordered = TRUE) ----
  xdf_sum <- xdf_sum |>
    dplyr::mutate(
      cover_prop = factor(as.character(cover_prop), levels = all_levels, ordered = TRUE)
    )
  
  plot_df <- dplyr::bind_rows(
    xdf_sum,
    spacer
  ) |>
    dplyr::mutate(
      cover_prop = factor(as.character(cover_prop), levels = c(range_levels, "Spacer")),
      label = dplyr::if_else(cover_prop == "Spacer", "", as.character(Site_No))
    )
  
  # ---- Choose palette keyed to ranges; robust fallback ----
  palette <- if (cover_type == "HARD CORAL") {
    hc.pal
  } else if (cover_type == "MACROALGAE") {
    ma.pal
  } else if (cover_type == "SOFT CORAL") {
    sc.pal
  } else {
    NULL
  }
  if (is.null(palette) || !all(range_levels %in% names(palette))) {
    palette <- c("50-100%" = "#00734D", "30-<50%" = "#F0C918", "10%-<30%" = "#F47721", "0-<10%" = "#ED1C24")
  }
  
  # ---- Orientation: choose start angle for left/right half ----
  start_angle <- if (identical(side, "left")) pi else 0
  
  # ---- Build the plot ----
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = 2, y = Site_No, fill = cover_prop)) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y", start = start_angle) +
    # Donut thickness controlled by x-limits
    ggplot2::xlim(0.5, 2.5) +
    ggplot2::scale_fill_manual(
      values = c(palette, "Spacer" = NA),
      breaks = range_levels,
      labels = range_levels
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white", size = 4
    ) +
    ggplot2::labs(
      title = paste("Coral Reef Habitat Condition", info$region_name, "Region"),
      subtitle = sprintf("Number of sites by %s cover range", stringr::str_to_title(cover_type))
      # caption = stringr::str_wrap(sprintf("Data Credits: %s", paste(info$data_contributors, collapse = ". ")), 70)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title   = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      plot.caption  = ggplot2::element_text(size = 10)
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0,  # near inner radius center
      label = "Source: ReefCloud.ai",
      size = 4,
      color = "black"
    )
  
  # ---- Save the figure ----
  ggplot2::ggsave(
    filename = paste0(
      "figures/",
      "SiteCondition_",
      stringr::str_replace_all(cover_type, " ", "_"),
      "_half_donut.png"
    ),
    plot = p,
    bg = "transparent", width = 8, height = 6
  )
  
  return(p)
}
plot_half_donut(1703)
