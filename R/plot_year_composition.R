# =============================================================
# File: plot_year_composition.R
# Description: Generates a stacked barplot of benthic cover per year for a
#              specified tier id, and depth using ReefCloud Public Dashboard data.
# Author: Samuel Chan
# Date: 2026-03-02
# Dependencies: ggplot2, dplyr, forcats, stringr, sf
# =============================================================

#' Create Stacked Year-Level Benthic Cover Plot 
#'
#' @description
#' Fetches year-level benthic cover via internal helpers and \code{get_benthic_cover()},
#' then produces a stacked bar chart of benthic groups per year for a single tier and depth.
#'
#' @param tier_id Character or numeric. Region/tier ID used to retrieve site list and metadata.
#' @param depth Character. Depth category to filter (e.g., "shallow", "deep" or "none"). Default: "shallow". 
#' At tiers broader than the site level, depth is not assigned and regional data should assume "NA" for depth.
#' @param drop_zero_years Logical. If TRUE (default), remove years whose total stacked cover is 0 or all-NA.
#' @param fill_by Character. Choose which variable to use for fill: \code{"group"} (default) or \code{"group_code"}
#' @param coord_flip Logical. If TRUE, flip coordinates for readability. Default: TRUE.
#'
#' @return A list with components:
#'   \item{plot}{A \code{ggplot} object.}
#'   \item{df}{The data frame used for plotting (tier_id, year, depth, group, cover, plotting_cover).}
#'
#' @details
#' This function mirrors the structure of \code{plot_temporal_cover()}, but stacks all benthic groups
#' instead of focusing on a single group with confidence intervals. It pulls group-wise cover via 
#' \code{get_benthic_cover()}. If multiple rows exist per group-depth, values are aggregated 
#' by mean (after ensuring percent scale).
#'
#' @examples
#' \dontrun{
#' # Minimal regional plot
#' res <- plot_year_composition(tier_id = 1705, depth = "NA")
#' print(res$plot)
#' 
#' # Site level variation
#' site_res <- plot_year_composition(tier_id = 437, depth = "shallow", fill_by = "group_code")
#' }
#'
#' @import ggplot2 dplyr forcats stringr sf
#' @export
plot_year_composition <- function(
    tier_id,
    depth = "shallow",
    drop_zero_years = TRUE,
    fill_by = c("group", "group_code"),
    coord_flip = TRUE
) {
  
  # ---- Load required internal helpers (mirroring your reference) ----
  source("R/get_regional_summary.R")
  source("R/get_tier_summary.R")
  source("R/get_benthic_cover.R")
  source("R/load_plot_palette.R")
  
  # ---- Suppress messages ----
  old_opts <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(old_opts), add = TRUE)
  
  # ---- Validate inputs ----
  fill_by <- match.arg(fill_by)
  
  if (missing(tier_id) || is.null(tier_id) || length(tier_id) != 1L) {
    stop("`tier_id` must be a single numeric identifier.")
  }
  if (!is.character(depth) || length(depth) != 1L) {
    stop("`depth` must be a single character scalar (e.g., 'shallow' or 'deep').")
  }
  
  # ---- Region metadata and site list ----
  info <- get_regional_summary(tier_id)
  
  # ---- Fetch benthic cover for all sites in this tier ----
  raw <- get_benthic_cover(tier_id)
  
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    stop("No benthic cover data returned by `get_benthic_cover()` for the provided sites. Check the depth provided.")
  }
  
  tdf <- get_tier_summary(tier_id) |> 
    sf::st_drop_geometry() |> 
    dplyr::select(- site_count)
  
  # ---- Filter by depth, join site names, and set default year if needed ----
  xdf <- raw |>
    dplyr::filter(depth == !!depth) |>
    # keep minimal needed columns + join
    dplyr::left_join(tdf)
  
  # ---- Trim to target and select/rename plotting columns ----
  xdf <- xdf |>
    dplyr::select(
      tier       = tier_name,
      tier_id    = tier_id,
      tier_level = tier_level,
      year       = year,
      depth      = depth,
      group      = type,
      group_code = type_code,
      cover      = mean
    )
  
  if (nrow(xdf) == 0) {
    stop("No rows after filtering for depth = '", depth, "'.")
  }
  
  # ---- Ensure numeric percent; convert from proportion if needed ----
  xdf <- xdf |>
    dplyr::mutate(cover = suppressWarnings(as.numeric(cover)))
  
  # If multiple rows per year-group (replicates), aggregate by mean to get one bar segment
  xdf <- xdf |>
    dplyr::group_by(tier, year, depth, group, group_code) |>
    dplyr::summarise(plotting_cover = mean(cover, na.rm = TRUE), .groups = "drop")
  
  # ---- Optionally drop zero/NA-total sites ----
  if (isTRUE(drop_zero_years)) {
    totals <- xdf |>
      dplyr::group_by(year) |>
      dplyr::summarise(total = sum(plotting_cover, na.rm = TRUE), .groups = "drop")
    keep_years <- totals$year[totals$total > 0]
    xdf <- dplyr::filter(xdf, year %in% keep_years)
  }
  
  if (nrow(xdf) == 0) {
    stop("No years remain after removing zero/NA total cover (if enabled).")
  }
  
  # ---- Change order based on group or group_code---- 
  xdf <- xdf |>
    dplyr::mutate(
      fill_var = if (fill_by == "group") group else group_code,
      fill_fct = if (fill_by == "group") {
        forcats::fct_relevel(fill_var, names(group.pal))
      } else {
        forcats::fct_relevel(fill_var, names(groupcode.pal))
        }
    ) |> 
    dplyr::select(- fill_var)
  
  # ---- Build plot ----
  plot <- ggplot2::ggplot(xdf, ggplot2::aes(x = year, y = plotting_cover, fill = fill_fct)) +
    ggplot2::geom_col(width = 0.8, position = "fill") +
    ggplot2::scale_fill_manual(name = if (fill_by == "group") "Benthic Groups" else "Benthic Codes",  
                               values = if (fill_by == "group") group.pal else groupcode.pal) +
    ggplot2::scale_y_continuous(name = "Cover (%)", labels = scales::label_percent(suffix = "")) +
    ggplot2::scale_x_continuous(name = "Years") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),  # Remove major grid lines
                   panel.grid.minor = ggplot2::element_blank()   # Remove minor grid lines
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste("Coral Reef Composition for ", info$region_name),
      subtitle = sprintf("Annual compositional data for reefs in Depth: %s", stringr:: str_to_title(depth))
    )
  
  
  # ---- Save plot ----
  ggplot2::ggsave(plot,
                  filename = paste0("figures/", "YearComposition_", info$region_name, stringr:: str_to_title(depth), ".png"),
                  bg = "transparent", width = 12, height = 8
  )
  
  
  # ---- Return ----
  return(list(
    plot = plot,
    df.sum = xdf
  ))
}
