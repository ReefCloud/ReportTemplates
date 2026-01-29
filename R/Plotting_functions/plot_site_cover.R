# =============================================================
# File: plot_site_cover.r
# Description: Generates and saves a site-level cover plot for a specified benthic category and tier id using data extracted from the ReefCloud Public Dashboard.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: ggplot2, dplyr, stringr
# =============================================================
# 

#' Create Site-Level Cover Plot
#'
#' This function generates a site-level plot of benthic cover (e.g., hard coral, macroalgae, soft coral)
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
#' create_site_plot(tier_id = "GBR", cover_type = "HARD CORAL")
#' create_site_plot(tier_id = "GBR", year = 2022, cover_type = "MACROALGAE")
#' }
#'
#' @import ggplot2 dplyr forcats stringr sf
#' @export
plot_site_cover <- function(tier_id, year = NULL, cover_type = "HARD CORAL", depth = "shallow") {
  
  # Load required functions
  source("R/API_functions/get_regional_summary.r")
  source("R/API_functions/get_site_summary.r")
  source("R/API_functions/get_benthic_cover.r")
  source("R/API_functions/get_disturbance.r")
  source("R/Plotting_functions/add_cover_categories.r")
  source("R/Plotting_functions/integer_breaks.r")
  
  # Retrieve site summary
  sdf <- get_site_summary(tier_id) |>
    st_drop_geometry() |>
    select(-id, -org_id, -org_name)
  
  # Retrieve benthic cover data and join with site summary
  xdf <- get_benthic_cover(sdf$site_id) |>
    filter(type == !!cover_type) |> 
    filter(depth_cat == !!depth) |>
    select(-element_id, -id, -survey_id, -tier_level) |>
    left_join(sdf, by = c("tier_id" = "site_id")) |>
    add_cover_categories()
  
  # Set default year if not provided
  if (is.null(year)) {
    year <- max(xdf$year, na.rm = TRUE)
  }
  
  # Filter by year
  xdf <- xdf |> filter(year == !!year)
  
  # Define palette
  palette <- if (cover_type == "HARD CORAL") {
    hc.pal
  } else if (cover_type == "MACROALGAE") {
    ma.pal
  } else if (cover_type == "SOFT CORAL") {
    sc.pal
  } else {
    c("A" = "#00734D", "B" = "#F0C918", "C" = "#F47721", "D" = "#ED1C24") # fallback
  }
  
  # Generate plot
  plot <- xdf |>
    mutate(site_name = fct_reorder(site_name, desc(median))) |>
    ggplot() +
    geom_pointrange(aes(x = site_name, y = median, ymin = low, ymax = high, col = cover_prop)) +
    geom_hline(aes(yintercept = 30), col = palette[2], linetype = 2) +
    scale_color_manual(values = palette) +
    scale_y_continuous(name = "Cover (%)", limits = c(0, 100)) +
    scale_x_discrete(name = "Sites") +
    theme_void() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste("Coral Reef Habitat Condition", info$region_name, "Region"),
      subtitle = sprintf("Number of sites by %s cover category", stringr::str_to_title(cover_type))
    )
  
  # Save plot
  ggsave(plot,
         filename = paste0("figures/", "SiteCover_", stringr::str_replace_all(cover_type, " ", "_"), ".png"),
         bg = "transparent", width = 12, height = 6
  )
  
  return(list(
    plot = plot,
    df.sum = xdf
  ))
}
