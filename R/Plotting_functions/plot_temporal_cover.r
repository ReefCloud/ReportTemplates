# =============================================================
# File: plot_temporal_cover.R
# Description: Generates and saves a plot of percent cover trends for a specified region and functional group, using data extracted from the ReefCloud Public Dashboard.
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: ggimage, ggplot2, tidyverse, scales, hrbrthemes
# =============================================================

#' Generate and Save Temporal Hard Coral Cover Plot
#'
#' This function creates a temporal plot of hard coral cover trends for a specified region (tier),
#' including disturbance events (thermal stress and storms) and contributor credits. The plot is saved as a PNG file.
#'
#' @param tier_id Character. Unique identifier for the region/tier to plot.
#'
#' @return Data frame of modelled cover data for the specified region.
#'
#' @details
#' The function retrieves modelled cover data and disturbance events, then generates a plot showing
#' median hard coral cover with 95% credible intervals and disturbance icons. Data credits are annotated.
#' Requires the following packages: ggimage, ggplot2, tidyverse, scales, hrbrthemes.
#'
#' @examples
#' # Example usage:
#' plot_temporal_cover("your_tier_id")

plot_temporal_cover <- function(tier_id, cover_type = "HARD CORAL") {
  require(ggimage)
  require(ggplot2)
  require(tidyverse)
  require(scales)
  require(hrbrthemes)

  # Load dependent functions
  source("R/API_functions/get_regional_summary.r")
  source("R/API_functions/get_benthic_cover.r")
  source("R/API_functions/get_disturbance.r")
  source("R/Plotting_functions/integer_breaks_helper.r")

  # Get info and modeled cover over time for this tier_id
  info <- get_regional_summary(tier_id)
  s.df <- get_benthic_cover(tier_id)
  
  # Get disturbances for this tier id

  events <- get_disturbance(tier_id, e_type = "thermal_stress") |>
    filter(severity > 1, percentage_total_reef > 0.05) |>
    mutate(Dist = "DHW", Year = Year - 0.5, icon = "_media/icons/bleaching.png") |>
    select(Year, Dist, icon) |>
    distinct() |>
    bind_rows(
      get_disturbance(tier_id, e_type = "storm_exposure_year") |>
        select(Year, severity, percentage_total_reef) |>
        filter(severity > 1, percentage_total_reef > 0.05) |>
        mutate(Dist = "TC", Year = Year - 0.5, icon = "_media/icons/cyclone.png") |>
        select(Year, Dist, icon) |>
        distinct()
    ) |>
    filter(Year >= min(year(s.df$date)))

  plot <- s.df |>
    filter(type == taxa) |>
    mutate(year = year(date)) |>
    ggplot() +
      geom_pointrange(aes(x = year, y = median, ymin = low, ymax = high), size = 2, color = "black", linetype = "dashed") +
      geom_line(aes(x = year, y = median), size = 1.5) +
      geom_image(
        data = events,
        mapping = aes(x = Year, y = max(s.df$high[s.df$type == taxa]) - max(s.df$high[s.df$type == taxa]) / 20, image = "_media/icons/down_arrow.png")
      ) +
      geom_image(
        data = events,
        mapping = aes(x = Year, y = max(s.df$high[s.df$type == taxa]), image = icon),
        position = position_dodge(width = 1)
      ) +
      labs(
        title = "Hard Coral Cover Trend",
        subtitle = "Median Cover with 95% Credible Intervals",
        x = "Year",
        y = "Cover (%)",
        caption = "Source: ReefCloud.ai"
      ) +
      scale_x_continuous(breaks = integer_breaks()) +
      theme_ipsum(grid = "") +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")
      ) +
      annotate(
        "text",
        x = Inf,
        y = -Inf,
        label = sprintf("Data Credits: %s", paste(info$data_contributors, collapse = ". ")),
        hjust = 1.1,
        vjust = -1.1,
        size = 3,
        color = "black"
      )

  ggsave(
    plot,
    filename = "figures/temporal_HardCoral_cover.png",
    bg = "transparent", width = 8, height = 7
  )
  return(s.df)
}
