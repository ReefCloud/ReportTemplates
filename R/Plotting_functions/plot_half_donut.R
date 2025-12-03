# =============================================================
# File: plot_half_donut.r
# Description: Generates and saves a half donut plot for a specified benthic category using data extracted from the ReefCloud Public Dashboard.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: ggplot2, dplyr, stringr
# =============================================================
# 
#' Plot Half Donut Showing Number of Sites by Cover Category
#'
#' This function retrieves benthic cover data for a given tier using ReefCloud API,
#' classifies sites into cover categories (A–D) based on the specified benthic cover type,
#' and plots a left-side half-donut chart showing the number of sites in each category.
#'
#' @param tier_id Character. The unique identifier for the region/tier to query.
#' @param cover_type Character. The benthic cover type to classify (default = "HARD CORAL").
#' @param side Character. Position of the half donut: "left" or "right" (default = "left").
#'
#' @return A `ggplot` object representing the half-donut chart.
#'
#' @examples
#' plot_half_donut(tier_id = "exampleTierID", cover_type = "HARD CORAL", side = "left")
#' plot_half_donut(tier_id = "exampleTierID", cover_type = "MACROALGAE", side = "right")
#'
#' @export

plot_half_donut <- function(tier_id, cover_type = "HARD CORAL", side = "left") {
  
  source("R/API_functions/get_benthic_cover.r")
  source("R/API_functions/get_regional_summary.r")
  source("R/Plotting_functions/add_cover_categories.r")
  source("R/Plotting_functions/load_plot_palette.r")
  
  info <- get_regional_summary(tier_id)
  
  xdf <- get_benthic_cover(tier_id) |> 
    dplyr::filter(type == cover_type) 
  
  xdf <- add_cover_categories(xdf, column = "median")
  
  xdf_sum <- xdf |> 
    dplyr::group_by(cover_cat, cover_prop) |> 
    dplyr::summarise(Site_No = dplyr::n(), .groups = "drop")
  
  palette <- if (cover_type == "HARD CORAL") {
    hc.pal
  } else if (cover_type == "MACROALGAE") {
    ma.pal
  } else if (cover_type == "SOFT CORAL") {
    sc.pal
  } else {
    c("A" = "#00734D", "B" = "#F0C918", "C" = "#F47721", "D" = "#ED1C24") # fallback
  }
  
  p <- ggplot(xdf_sum, aes(x = 2, y = Site_No, fill = cover_prop)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = start_angle) +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = palette) +
    geom_text(aes(label = Site_No),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4) +
    labs(
      title = paste("Coral Reef Habitat Condition", info$region_name, "Region"),
      subtitle = sprintf("Number of sites by %s cover category", stringr::str_to_title(cover_type)),
      #caption = stringr::str_wrap(sprintf("Data Credits: %s", paste(info$data_contributors, collapse = ". ")), 70)
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10)
    ) +
    annotate("text",
             x = 0,
             y = 0,
             label = "Source: ReefCloud.ai",
             size = 4,
             color = "black"
    )
  
  # Save plot
  ggsave(p,
         filename = paste0("figures/", "SiteCondition_", stringr::str_replace_all(cover_type, " ", "_"), "_half_donut.png"),
         bg = "transparent", width = 8, height = 6
  )
  
  return(p)
}
