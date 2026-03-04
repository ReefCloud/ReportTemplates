#' Plot Donut for Benthic Cover (Defaults to HARD CORAL)
#'
#' Retrieves survey compositions via \code{get_benthic_cover()}, filters to a requested
#' benthic \code{cover_type} (default \code{"HARD CORAL"}), classifies cover into ranges
#' using \code{add_cover_categories()}, and renders a whole-donut plot. Uses
#' \code{get_regional_summary()} to add region/site context for labels.
#'
#' @param tier_id Character. ReefCloud tier ID to query.
#' @param year Numeric (optional). Survey year to filter. Defaults to the maximum year available.
#' @param depth Character. Depth category to filter (e.g., \code{"shallow"}, \code{"deep"} or \code{"none"})
#' @param cover_type Character. Benthic group to plot (e.g., \code{"hard coral"}, \code{"macroalgae"}, 
#' \code{"soft coral"}, \code{"turf algae"}, \code{"crustose coralline algae"}, \code{"other"}).
#'   Default \code{"hard coral"}.
#' @param show_labels Logical. Show per-segment labels (percentages or counts). Default \code{TRUE}.
#' @param label_format Character. Label type: \code{"percent"} or \code{"count"}. Default \code{"count"}.
#' @param donut_width Numeric in (0, 1]. Ring thickness (e.g., \code{0.6}). Default \code{0.6}.
#'
#' @return A \code{ggplot} donut chart.
#'
#' @export
plot_donut <- function(
    tier_id,
    year         = NULL,    
    depth        = "shallow",
    cover_type   = "hard coral",
    show_labels  = TRUE,
    label_format = c("count", "percent"),
    donut_width  = 0.6
) {
  
  # ---- Load external helper functions and palettes ----
  source("R/get_benthic_cover.R")
  source("R/get_regional_summary.R")
  source("R/add_cover_categories.R")
  source("R/load_plot_palette.R")
  source("R/select_palette.R")
  
  # ---- Validate inputs ----
  if (missing(tier_id) || length(tier_id) != 1) {
    stop("`tier_id` must be a single character ID.")
  }
  if (!is.character(cover_type) || length(cover_type) != 1) {
    stop("`cover_type` must be a single string (e.g., 'hard coral').")
  }
  if (!is.character(depth) || length(depth) != 1) {
    stop("`depth` must be a single string (e.g., 'shallow', 'deep' or 'none').")
  }
  label_format <- match.arg(label_format)
  if (!is.numeric(donut_width) || !is.finite(donut_width) || donut_width <= 0 || donut_width > 1) {
    stop("`donut_width` must be a numeric value in (0, 1].")
  }
  if (!is.null(year) && !(is.numeric(year) || is.integer(year)) ) {
    stop("`year` must be NULL or a numeric/integer year.")
  }
  
  # ---- Fetch info (region name + site count) ----
  info <- tryCatch(
    get_regional_summary(tier_id),
    error = function(e) NULL
  )
  region_name <- if (!is.null(info) && is.list(info)) info$region_name else NULL
  site_count  <- if (!is.null(info) && is.list(info)) info$site_count  else NULL
  
  # ---- Retrieve and filter by year ----
  surveys <- tryCatch(
    get_benthic_cover(info$site_id),
    error = function(e) stop("Failed to retrieve surveys via get_benthic_cover(): ", conditionMessage(e))
  )
  if (is.null(surveys) || !is.data.frame(surveys) || nrow(surveys) == 0) {
    stop("No survey data returned from get_benthic_cover().")
  }
  
  # Filter by depth
  surveys <- dplyr::filter(surveys, depth == !!depth)
  
  # Ensure year column exists
  if (!"year" %in% names(surveys)) {
    stop("Fetched survey data lacks a `year` column; cannot filter/select year.")
  }
  
  # Determine default year if not supplied
  if (is.null(year)) {
    year <- max(surveys$year, na.rm = TRUE)
  }
  
  # Filter to selected year             
  surveys <- dplyr::filter(surveys, year == !!year)
  if (nrow(surveys) == 0) {
    stop("No survey rows found after filtering to year = ", year, ".")
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
    stop(sprintf("No rows found for cover_type='%s' in depth '%s' for year %s.", cover_type, depth, year))
  }
  
  # ---- Classify into range-based categories using median cover ----
  xdf <- add_cover_categories(xdf, column = "median")  # adds `cover_prop` + `cover_cat`
  
  # ---- Summarise: number of rows per range (proxy for site counts) ----
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
  ct_upper <- toupper(cover_type)
  group_map <- list(
    "HARD CORAL"  = "hc",
    "MACROALGAE"  = "ma",
    "SOFT CORAL"  = "sc"
  )
  gp <- group_map[[ct_upper]]
  if (is.null(gp)) gp <- "hc"  # sensible fallback
  
  pal <- select_palette(group = gp, use = "prop")
  
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
  
  plot <- ggplot2::ggplot(xdf_sum, ggplot2::aes(x = x, y = Site_No, fill = cover_prop)) +
    ggplot2::geom_col(width = donut_width, color = NA) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_fill_manual(name = "Cover Category", values = pal) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal") +
    ggplot2::labs(
      title = paste("Coral Reef Site Overview for ", ifelse(is.null(region_name), tier_id, region_name)),
      subtitle = sprintf("Overall %s condition for reefs in Depth: %s, Year: %s",
                         stringr::str_to_title(cover_type), stringr::str_to_title(depth), year)
    )
  
  
  # Segment labels (optional)
  if (isTRUE(show_labels)) {
    labs <- if (label_format == "percent") paste0(round(xdf_sum$pct), "%") else as.character(xdf_sum$Site_No)
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = labs),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 4
    )
  }
  
  # Save plot (filename now includes year)
  ggplot2::ggsave(
    plot,
    filename = paste0(
      "figures/",
      "SiteOverview_",
      ifelse(is.null(region_name), tier_id, region_name), "_",
      stringr::str_replace_all(cover_type, " ", "_"), "_",
      depth, "_",
      year, ".png"
    ),
    bg = "transparent", width = 12, height = 8
  )
  
  return(plot)
}