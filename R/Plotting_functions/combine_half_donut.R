# =============================================================
# File: combine_half_donut.r
# Description: Combines two saved half donut plots for two specified benthic category using data extracted from the ReefCloud Public Dashboard.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: ggplot2, dplyr, stringr, patchwork
# =============================================================

#' Combine Two Half Donut Plots
#'
#' This function generates two half-donut plots (left and right) for different cover types
#' and combines them into a single figure.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#' @param left_cover_type Character. Cover type for the left half donut (default = "HARD CORAL").
#' @param right_cover_type Character. Cover type for the right half donut (default = "MACROALGAE").
#'
#' @return A combined `ggplot` object with both half-donut charts.
#'
#' @examples
#' combine_half_donuts(tier_id = "exampleTierID")
#'
#' @export

combine_half_donut <- function(tier_id, left_cover_type = "HARD CORAL", right_cover_type = "MACROALGAE") {

  source("R/Plotting_functions/plot_half_donut.r")
  library(patchwork)
  p_left <- plot_half_donut(tier_id, cover_type = left_cover_type, side = "left")
  p_right <- plot_half_donut(tier_id, cover_type = right_cover_type, side = "right")
  
  combined_plot <- p_left + p_right + plot_layout(ncol = 2)
  
  ggsave(combined_plot,
         filename = paste0("figures/", "SiteCondition_combined_half_donuts.png"),
         bg = "transparent", width = 14, height = 7)
  
  return(combined_plot)
}
