# =============================================================
# File: plot_site_composition.R
# Description: Generates a stacked barplot of benthic cover per site for a
#              specified tier id, year, and depth using ReefCloud Public Dashboard data.
# Author: Samuel Chan
# Date: 2026-02-24
# Dependencies: ggplot2, dplyr, forcats, stringr, sf
# =============================================================

#' Create Stacked Site-Level Benthic Cover Plot
#'
#' @description
#' Fetches site-level benthic cover via internal helpers and \code{get_benthic_cover()},
#' then produces a stacked bar chart of benthic groups per site for a single year and depth.
#'
#' @param tier_id Character or numeric. Region/tier ID used to retrieve site list and metadata.
#' @param year Numeric (optional). Survey year to plot. Defaults to the maximum available after filtering by depth.
#' @param depth Character. Depth category to filter (e.g., "shallow", "deep" or "none"). Default: "shallow".
#' @param drop_zero_sites Logical. If TRUE (default), remove sites whose total stacked cover is 0 or all-NA.
#' @param order_sites_by Character. One of \code{c("alphabetical","total_cover")}.
#'        Default "alphabetical" orders sites by descending total stacked cover.
#' @param fill_by Character. Choose which variable to use for fill: \code{"group"} (default) or \code{"group_code"}
#' @param coord_flip Logical. If TRUE, flip coordinates for readability. Default: TRUE.
#'
#' @return A list with components:
#'   \item{plot}{A \code{ggplot} object.}
#'   \item{df}{The data frame used for plotting (site, year, depth, group, cover, plotting_cover).}
#'
#' @details
#' This function mirrors the structure of \code{plot_site_cover()}, but stacks all benthic groups
#' instead of focusing on a single group with confidence intervals. It joins site names/metadata
#' from \code{get_site_summary()} and pulls group-wise cover via \code{get_benthic_cover()}.
#' If multiple rows exist per site-group-year-depth, values are aggregated by mean (after ensuring percent scale).
#'
#' @examples
#' \dontrun{
#' # Minimal
#' res <- plot_site_composition(tier_id = 1705, depth = "shallow")
#' print(res$plot)
#'
#' # Choose grouping column and save output
#' res <- plot_site_composition(
#'   tier_id = 1705,
#'   year = 2024,
#'   depth = "deep",
#'   fill_by = "group_code"
#' )
#' }
#'
#' @import ggplot2 dplyr forcats stringr sf
#' @export
plot_site_composition <- function(
    tier_id,
    year = NULL,
    depth = "shallow",
    drop_zero_sites = TRUE,
    order_sites_by = c("alphabetical", "total_cover"),
    order_group = NULL,
    fill_by = c("group", "group_code"),
    coord_flip = TRUE
    ) {
  # ---- Load required internal helpers ----
  source("R/get_regional_summary.R")
  source("R/get_site_summary.R")
  source("R/get_benthic_cover.R")
  source("R/load_plot_palette.R")
  
  # ---- Suppress messages ----
  old_opts <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(old_opts), add = TRUE)
  
  # ---- Validate inputs ----
  fill_by <- match.arg(fill_by)
  order_sites_by <- match.arg(order_sites_by)
  
  if (missing(tier_id) || is.null(tier_id) || length(tier_id) != 1L) {
    stop("`tier_id` must be a single numeric identifier.")
  }
  if (!is.character(depth) || length(depth) != 1L) {
    stop("`depth` must be a single character scalar (e.g., 'shallow' or 'deep').")
  }
  if (order_sites_by == "group_cover" && (is.null(order_group) || !is.character(order_group))) {
    stop("When `order_sites_by = 'group_cover'`, you must provide `order_group` as a character scalar.")
  }
  
  # ---- Region metadata and site list ----
  info <- get_regional_summary(tier_id)
  sdf <- get_site_summary(tier_id) |>
    sf::st_drop_geometry()
  
  if (is.null(sdf) || nrow(sdf) == 0 || is.null(sdf$site_id)) {
    stop("No sites returned by `get_site_summary()` for the provided `tier_id`.")
  }
  
  # ---- Fetch benthic cover for all sites in this tier ----
  raw <- get_benthic_cover(sdf$site_id)
  
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    stop("No benthic cover data returned by `get_benthic_cover()` for the provided sites.")
  }
  
  # ---- Filter by depth, join site names, and set default year if needed ----
  xdf <- raw |>
    dplyr::filter(.data$depth == !!depth) |>
    # keep minimal needed columns + join
    dplyr::left_join(sdf, by = c("tier_id" = "site_id"))
  
  if (!"site_name" %in% names(xdf)) {
    stop("Joined data lacks `site_name` after joining with site summary. ",
         "Ensure `get_site_summary()` returns `site_id` and `site_name`.")
  }
  
  # If year is NULL, set to max available for this depth
  if (is.null(year)) {
    if (!"year" %in% names(xdf)) stop("Fetched data lacks a `year` column.")
    year <- max(xdf$year, na.rm = TRUE)
  }
  
  # ---- Trim to target year and select/rename plotting columns ----
  xdf <- xdf |>
    dplyr::filter(year == !!year) |>
    dplyr::select(
      site    = site_name,
      site_id = tier_id,
      year    = year,
      depth   = depth,
      group      = type,
      group_code = type_code,
      cover   = mean
    )
  
  if (nrow(xdf) == 0) {
    stop("No rows after filtering for year = ", year, " and depth = '", depth, "'.")
  }
  
  # ---- Ensure numeric percent; convert from proportion if needed ----
  xdf <- xdf |>
    dplyr::mutate(cover = suppressWarnings(as.numeric(cover)))
  
  # If multiple rows per site-group (replicates), aggregate by mean to get one bar segment
  xdf <- xdf |>
    dplyr::group_by(site, year, depth, group, group_code) |>
    dplyr::summarise(plotting_cover = mean(cover, na.rm = TRUE), .groups = "drop")
  
  # ---- Optionally drop zero/NA-total sites ----
  if (isTRUE(drop_zero_sites)) {
    totals <- xdf |>
      dplyr::group_by(site) |>
      dplyr::summarise(total = sum(plotting_cover, na.rm = TRUE), .groups = "drop")
    keep_sites <- totals$site[totals$total > 0]
    xdf <- dplyr::filter(xdf, site %in% keep_sites)
  }
  
  if (nrow(xdf) == 0) {
    stop("No sites remain after removing zero/NA total cover (if enabled).")
  }
  
  # ---- Site ordering ----
  if (order_sites_by == "alphabetical") {
    xdf <- xdf |>
      dplyr::mutate(site = factor(site, levels = sort(unique(site))))
  } else if (order_sites_by == "total_cover") {
    totals <- xdf |>
      dplyr::group_by(site) |>
      dplyr::summarise(total = sum(plotting_cover, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total))
    xdf <- xdf |>
      dplyr::mutate(site = factor(site, levels = totals$site))
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
  plot <- ggplot2::ggplot(xdf, ggplot2::aes(x = site, y = plotting_cover, fill = fill_fct)) +
    ggplot2::geom_col(width = 0.8, position = "fill") +
    ggplot2::scale_fill_manual(name = if (fill_by == "group") "Benthic Groups" else "Benthic Codes",  
                               values = if (fill_by == "group") group.pal else groupcode.pal) +    
    ggplot2::scale_y_continuous(name = "Cover (%)", labels = scales::label_percent(suffix = "")) +
    ggplot2::scale_x_discrete(name = "Sites") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),  # Remove major grid lines
                   panel.grid.minor = ggplot2::element_blank()   # Remove minor grid lines
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste("Coral Reef Composition for ", info$region_name),
      subtitle = sprintf("Sites compositional data for reefs in Depth: %s, Year: %s", stringr:: str_to_title(depth), year)
    )

  
  # ---- Save plot ----
  ggplot2::ggsave(plot,
                  filename = paste0("figures/", "SiteComposition_", info$region_name, stringr:: str_to_title(depth), year, ".png"),
                  bg = "transparent", width = 12, height = 8
  )
  
  
  # ---- Return ----
  return(list(
    plot = plot,
    df.sum = xdf
  ))
}
