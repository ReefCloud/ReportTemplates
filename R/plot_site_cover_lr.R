# =============================================================
# File: plot_site_cover_lr.r
# Description: Generates and saves a site-level cover plot for a specified benthic category and tier id at the local region using data extracted from the ReefCloud Public Dashboard.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: ggplot2, dplyr, stringr
# =============================================================
# 
#' Create Local Region specific Site-Level Cover Plot
#'
#' This function generates a site-level plot of benthic cover and then a local region 
#' site-level plot of benthic cover (e.g., hard coral, macroalgae, soft coral)
#' for a specified tier ID and year. If the year is not provided, the function defaults to the maximum
#' year available in the dataset. The plot displays median cover estimates with confidence intervals
#' and categorizes sites based on cover proportions.
#'
#' @param tier_id Character or numeric. The tier ID for which site-level data will be retrieved.
#' @param year Numeric (optional). The survey year to filter data. Defaults to the maximum year available.
#' @param cover_type Character. The benthic cover type to plot. Defaults to `"HARD CORAL"`.
#' @param depth Character. The depth category to filter data. Defaults to `"shallow"`.
#'
#' @return A `ggplot` object showing site-level cover estimates with confidence intervals.
#' @details
#' The function uses internal API functions to retrieve site summaries and benthic cover data,
#' applies cover category classification, and generates a plot with color-coded cover categories.
#'
#' @examples
#' \dontrun{
#' create_site_plot(tier_id = 1703, cover_type = "HARD CORAL")
#' create_site_plot(tier_id = 1703, year = 2022, cover_type = "MACROALGAE")
#' }
#'
#' @import ggplot2 dplyr forcats stringr sf
#' @export

plot_site_cover_lr <- function(tier_id, year = NULL, cover_type = "HARD CORAL", depth = "shallow") {
  
  # Load required functions
  source("R/get_regional_summary.R")
  source("R/get_site_summary.R")
  source("R/get_benthic_cover.R")
  source("R/get_disturbance.R")
  source("R/add_cover_categories.R")
  source("R/integer_breaks.R")
  source("R/plot_site_cover.R")
  
  xdf <- plot_site_cover(tier_id)
  
  fullplot <- xdf$plot
  xdf <- xdf$df.sum
  
  # Replace NA with "None"
  xdf$local_region[is.na(xdf$local_region)] <- "Unspecified"
  
  # Split into multiple dataframes by local_region
  xdf_lr <- split(xdf, xdf$local_region)
  
  # Generate a plot for each region
  plots <- lapply(names(xdf_lr), function(region) {
    df <- xdf_lr[[region]]
    ggplot(df, aes(x = fct_reorder(site_name, desc(median)), y = median, ymin = low, ymax = high, col = cover_prop)) +
      geom_pointrange() +
      geom_hline(aes(yintercept = 30), col = palette[2], linetype = 2) +
      scale_fill_manual(values = palette) +
      scale_y_continuous(name = "Cover (%)", limits = c(0, 100)) +
      scale_x_discrete(name = "Sites") +
      theme_void() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste("Coral Reef Habitat Condition - Local Region", region),
        subtitle = sprintf("%s cover category", stringr::str_to_title(cover_type))
      )
  })
  
  # Name plots
  names(plots) <- names(xdf_lr)
  
  # Save all plots
  lapply(names(plots), function(region) {
    filename <- paste0("figures/SiteCover_", region, "_", stringr::str_replace_all(cover_type, " ", "_"), ".png")
    ggsave(filename = filename, plot = plots[[region]], width = 8, height = 6)
  })
  
  ggsave(fullplot,
         filename = paste0("figures/", "SiteCover_", stringr::str_replace_all(cover_type, " ", "_"), ".png"),
         bg = "transparent", width = 12, height = 6
  )
  
  return(plots)
  }
