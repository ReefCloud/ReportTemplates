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

plot_site_cover <- function(tier_id, year = NULL, cover_type = "hard coral", depth = "shallow") {
  
  # Load required functions
  source("R/get_regional_summary.R")
  source("R/get_site_summary.R")
  source("R/get_benthic_cover.R")
  source("R/add_cover_categories.R")
  source("R/integer_breaks.R")
  source("R/load_plot_palette.R")
  
  # Retrieve regional summary
  info <- get_regional_summary(tier_id)
  
  # Retrieve site summary
  sdf <- get_site_summary(tier_id) |>
    sf::st_drop_geometry()
  
  # Retrieve benthic cover data and join with site summary
  xdf <- get_benthic_cover(sdf$site_id) |>
    dplyr::filter(type == !!cover_type) |> 
    dplyr::filter(depth == !!depth) |>
    dplyr::select(-tier_level) |>
    dplyr::left_join(sdf, by = c("tier_id" = "site_id")) |>
    add_cover_categories()
  
  # Set default year if not provided
  if (is.null(year)) {
    year <- max(xdf$year, na.rm = TRUE)
  }
  
  # Filter by year
  xdf <- xdf |> dplyr::filter(year == !!year)
  
  # Define palette
  palette <- if (cover_type == "hard coral") {
    hc.pal_prop
  } else if (cover_type == "macroalgae") {
    ma.pal_prop
  } else if (cover_type == "soft coral") {
    sc.pal_prop
  } else {
    c("A" = "#00734D", "B" = "#F0C918", "C" = "#F47721", "D" = "#ED1C24") # fallback
  }
  
  # Generate plot
  plot <- xdf |>
    dplyr::mutate(site_name = forcats::fct_reorder(site_name, dplyr::desc(median))) |>
    ggplot2::ggplot() +
    ggplot2::geom_pointrange(
      ggplot2::aes(x = site_name, y = median, ymin = low, ymax = high, col = cover_prop)) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 30), col = palette[2], linetype = 2) +
    ggplot2::scale_color_manual(name = "Cover Proportion", values = palette) +
    ggplot2::scale_y_continuous(name = "Cover (%)", limits = c(0, 100)) +
    ggplot2::scale_x_discrete(name = "Sites") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),  # Remove major grid lines
                   panel.grid.minor = ggplot2::element_blank()   # Remove minor grid lines
                   ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste("Coral Reef Site Condition for ", info$region_name),
      subtitle = sprintf("Sites by descending %s cover fo reefs in Depth: %s, Year: %s", 
                         stringr::str_to_title(cover_type), 
                         stringr::str_to_title(depth), 
                         year)
    )
  
  # Save plot
  ggplot2::ggsave(plot,
         filename = paste0("figures/", "SiteCover_", stringr::str_replace_all(cover_type, " ", "_"), ".png"),
         bg = "transparent", width = 12, height = 8
  )
  
  return(list(
    plot = plot,
    df.sum = xdf
  ))
}
