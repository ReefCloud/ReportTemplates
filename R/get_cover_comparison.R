# =============================================================
# File: get_cover_comparison.R
# Description: Produce a single flat table at tier and site levels 
#              with one row per comparison
# Author: Samuel Chan
# Date: 2026-02-23
# Dependencies: dplyr, tidyr, stringr, sf
# =============================================================

#' Compare Two Years of Benthic Cover
#'
#' Creates a single tidy table containing **tier** and **site** rows for the
#' comparison between the **first** and **last** available years (by default),
#' adding absolute and percent change as columns.
#'
#' Columns returned (only):
#' \itemize{
#'   \item \code{tier_id}, \code{region_name}, \code{cover_type}, \code{depth}
#'   \item \code{row_type}  ("Tier" OR "Site: <site_name>")
#'   \item \code{year_first} (baseline year) and \code{year_last} (comparison year)
#'   \item \code{n_sites}   (tier row: #sites in last year; site rows: 1)
#'   \item \code{mean_cover}            (tier/site value in last year)
#'   \item \code{diff_from_first}       (last - first)
#'   \item \code{pct_change_from_first} (percent change vs first; NA if baseline is 0)
#' }
#'
#' @param tier_id Character or numeric. Region/tier identifier used by your helpers.
#' @param years Optional numeric length-2 vector. If omitted, uses c(min(year), max(year)).
#' @param cover_type Character. One of "hard coral", "macroalgae", "soft coral",
#'        "turf algae", "crustose coralline algae", "other". Default "hard coral".
#' @param depth Character. One of "shallow", "deep" or "none. Default "shallow".
#'
#' @return A tibble with the columns described above. Each **site** that has
#'         both years contributes **one row**; the **tier** contributes one row.
#'
#' @examples
#' \dontrun{
#' tab <- get_cover_comparison(1705)
#' tab_ma <- get_cover_comparison(1705, cover_type = "macroalgae", depth = "deep")
#' tab_custom <- get_cover_comparison(1705, years = c(2009, 2016))
#' }
#'
#' @import dplyr tidyr stringr sf
#' @export
get_cover_comparison <- function(tier_id,
                                 years = NULL,
                                 cover_type = "hard coral",
                                 depth = "shallow") {
  # ---- Load required helpers ----------------------------------------------
  source("R/get_regional_summary.R")
  source("R/get_site_summary.R")
  source("R/get_benthic_cover.R")  # dashboard-api wrapper you provided
  
  # ---- Validate inputs -----------------------------------------------------
  if (missing(tier_id) || length(tier_id) != 1) {
    stop("`tier_id` must be length-1 (character or numeric).", call. = FALSE)
  }
  allowed_types <- c("hard coral", "macroalgae", "soft coral",
                     "turf algae", "crustose coralline algae", "other")
  if (!cover_type %in% allowed_types) {
    stop("`cover_type` must be one of: ", paste(allowed_types, collapse = ", "), call. = FALSE)
  }
  if (!is.character(depth) || length(depth) != 1L) {
    stop("`depth` must be a single string.", call. = FALSE)
  }
  depth <- tolower(depth)
  if (!depth %in% c("shallow", "deep", "none")) {
    stop("`depth` must be either 'shallow', 'deep' or 'none'.", call. = FALSE)
  }
  if (!is.null(years)) {
    if (length(years) != 2L || any(!is.finite(years))) {
      stop("`years` (if provided) must be numeric vector of length 2.", call. = FALSE)
    }
    years <- sort(as.integer(years))
  }
  
  # ---- Fetch region + sites + cover ---------------------------------------
  info <- get_regional_summary(tier_id)
  sdf  <- get_site_summary(tier_id) |> sf::st_drop_geometry()
  
  xdf <- get_benthic_cover(sdf$site_id) |>
    dplyr::filter(.data$type == !!cover_type, .data$depth == !!depth) |>
    dplyr::left_join(sdf, by = c("tier_id" = "site_id"))
  
  if (nrow(xdf) == 0) {
    stop("No data found for these filters (tier/cover_type/depth).", call. = FALSE)
  }
  
  # ---- Choose comparison years (first vs last by default) ------------------
  available_years <- sort(unique(stats::na.omit(xdf$year)))
  if (length(available_years) < 2) {
    stop("Fewer than two distinct years available for these filters.", call. = FALSE)
  }
  if (is.null(years)) {
    years <- c(min(available_years), max(available_years))
  } else {
    missing_years <- setdiff(years, available_years)
    if (length(missing_years) > 0) {
      stop("Requested years not found in data: ", paste(missing_years, collapse = ", "), call. = FALSE)
    }
  }
  year_a <- years[1]
  year_b <- years[2]
  
  # ---- Pick the central estimate column (prefer mean -> median) -----------
  central_candidates <- c("mean", "median")
  central_col <- central_candidates[central_candidates %in% names(xdf)][1]
  if (is.na(central_col)) {
    stop("Central estimate column not found (looked for: mean, median).", call. = FALSE)
  }
  
  # ---- Collapse to ONE value per site-year to prevent list-cols ------------
  site_year <- xdf |>
    dplyr::filter(.data$year %in% !!years) |>
    dplyr::group_by(
      site_id   = .data$tier_id,   # keep API-returned site id
      site_name = .data$site_name,
      year      = .data$year
    ) |>
    dplyr::summarise(
      value = mean(!!rlang::sym(central_col), na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Tier means from the site-year table (consistent basis) --------------
  tier_means <- site_year |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      n_sites    = sum(!is.na(.data$value)),
      mean_cover = mean(.data$value, na.rm = TRUE),
      .groups    = "drop"
    )
  
  mean_a <- tier_means$mean_cover[tier_means$year == year_a]
  mean_b <- tier_means$mean_cover[tier_means$year == year_b]
  n_b    <- tier_means$n_sites   [tier_means$year == year_b]
  
  tier_row <- dplyr::tibble(
    tier_id     = tier_id,
    region_name = info$region_name,
    cover_type  = cover_type,
    depth       = depth,
    row_type    = "Tier",
    year_first  = as.integer(year_a),
    year_last   = as.integer(year_b),
    n_sites     = as.integer(n_b),
    mean_cover  = as.numeric(mean_b),
    diff_from_first       = as.numeric(mean_b - mean_a),
    pct_change_from_first = ifelse(is.finite(mean_a) && mean_a != 0,
                                   100 * (mean_b - mean_a) / mean_a, NA_real_)
  )
  
  # ---- Pivot to wide for site-level comparisons (after deduplication) ------
  site_vals <- site_year |>
    tidyr::pivot_wider(
      names_from  = .data$year,
      values_from = .data$value,
      names_sort  = TRUE
    )
  
  # Keep only sites that have values for BOTH years
  col_a <- as.character(year_a)
  col_b <- as.character(year_b)
  if (!all(c(col_a, col_b) %in% names(site_vals))) {
    site_vals[[col_a]] <- site_vals[[col_a]] %||% NA_real_
    site_vals[[col_b]] <- site_vals[[col_b]] %||% NA_real_
  }
  site_vals <- site_vals |>
    dplyr::filter(!is.na(.data[[col_a]]) & !is.na(.data[[col_b]]))
  
  # ---- Build site rows ------------------------------------------------------
  if (nrow(site_vals) > 0) {
    site_rows <- site_vals |>
      dplyr::transmute(
        tier_id     = tier_id,
        region_name = info$region_name,
        cover_type  = cover_type,
        depth       = depth,
        row_type    = paste0("Site: ", .data$site_name),
        year_first  = as.integer(year_a),
        year_last   = as.integer(year_b),
        n_sites     = 1L,
        mean_cover  = as.numeric(.data[[col_b]]),
        diff_from_first = as.numeric(.data[[col_b]] - .data[[col_a]]),
        pct_change_from_first = ifelse(
          is.finite(.data[[col_a]]) & .data[[col_a]] != 0,
          100 * (.data[[col_b]] - .data[[col_a]]) / .data[[col_a]],
          NA_real_
        )
      )
  } else {
    site_rows <- dplyr::tibble(
      tier_id     = character(),
      region_name = character(),
      cover_type  = character(),
      depth       = character(),
      row_type    = character(),
      year_first  = integer(),
      year_last   = integer(),
      n_sites     = integer(),
      mean_cover  = double(),
      diff_from_first = double(),
      pct_change_from_first = double()
    )
  }
  
  # ---- Bind and order columns ----------------------------------------------
  out <- dplyr::bind_rows(tier_row, site_rows) |>
    dplyr::select(tier_id, region_name, cover_type, depth, row_type,
                  year_first, year_last, n_sites, mean_cover,
                  diff_from_first, pct_change_from_first)
  
  return(out)
}

# Helper: null-coalescing operator (if you don't already have one in your utils)
`%||%` <- function(x, y) if (is.null(x)) y else x