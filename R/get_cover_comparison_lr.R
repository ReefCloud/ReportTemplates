# =============================================================
# File: get_cover_comparison_lr.R
# Description: Produce a single flat table at tier and local regions
#              with one row per comparison
# Author: Samuel Chan
# Date: 2026-02-23
# Dependencies: dplyr, tidyr, stringr, sf
# =============================================================

#' Compare Two Years of Benthic Cover at Local Region Level (plus Tier)
#'
#' Creates a single tidy table containing a **tier** row and **local-region**
#' rows for the comparison between the **first** and **last** available years
#' (by default), adding absolute and percent change as columns.
#'
#' Columns returned (only):
#' \itemize{
#'   \item \code{tier_id}, \code{region_name}, \code{cover_type}, \code{depth}
#'   \item \code{row_type}  ("Tier" OR "Local Region: <label>")
#'   \item \code{year_first} (baseline year) and \code{year_last} (comparison year)
#'   \item \code{n_sites}   (tier row: #sites in last year; local-region rows: #sites in local region in last year)
#'   \item \code{mean_cover}            (value in last year)
#'   \item \code{diff_from_first}       (last - first)
#'   \item \code{pct_change_from_first} (percent change vs first; NA if baseline is 0)
#' }
#'
#' @param tier_id Character or numeric. Region/tier identifier used by your helpers.
#' @param years Optional numeric length-2 vector. If omitted, uses c(min(year), max(year)).
#' @param cover_type Character. One of "HARD CORAL", "MACROALGAE", "SOFT CORAL",
#'        "TURF ALGAE", "CRUSTOSE CORALLINE ALGAE", "OTHER". Default "HARD CORAL".
#' @param depth Character. One of "shallow", "deep". Default "shallow".
#' @param local_region_col Character scalar. Name of the column in \code{xdf}
#'        (after joining site metadata) that contains the Local Region label.
#'        Default \code{"local_region"}.
#'
#' @return A tibble with the columns described above. Each **local region** that
#'         has both years contributes **one row**; the **tier** contributes one
#'         row.
#'
#' @examples
#' \dontrun{
#' tab_lr <- get_cover_comparison_lr(1705)
#' tab_lr_ma <- get_cover_comparison_lr(1705, cover_type = "MACROALGAE", depth = "deep")
#' tab_lr_custom <- get_cover_comparison_lr(1705, years = c(2009, 2016))
#' # If your local region column is named differently:
#' tab_lr_named <- get_cover_comparison_lr(1705, local_region_col = "local_region_name")
#' }
#'
#' @import dplyr tidyr stringr sf
#' @export
get_cover_comparison_lr <- function(tier_id,
                                    years = NULL,
                                    cover_type = "HARD CORAL",
                                    depth = "shallow",
                                    local_region_col = "local_region") {
  # ---- Load required helpers ----------------------------------------------
  source("R/get_regional_summary.R")
  source("R/get_site_summary.R")
  source("R/get_benthic_cover.R")  # dashboard-api wrapper you provided
  
  # ---- Validate inputs -----------------------------------------------------
  if (missing(tier_id) || length(tier_id) != 1) {
    stop("`tier_id` must be length-1 (character or numeric).", call. = FALSE)
  }
  cover_type <- toupper(cover_type)
  allowed_types <- c("HARD CORAL", "MACROALGAE", "SOFT CORAL",
                     "TURF ALGAE", "CRUSTOSE CORALLINE ALGAE", "OTHER")
  if (!cover_type %in% allowed_types) {
    stop("`cover_type` must be one of: ", paste(allowed_types, collapse = ", "), call. = FALSE)
  }
  if (!is.character(depth) || length(depth) != 1L) {
    stop("`depth` must be a single string.", call. = FALSE)
  }
  depth <- tolower(depth)
  if (!depth %in% c("shallow", "deep")) {
    stop("`depth` must be either 'shallow' or 'deep'.", call. = FALSE)
  }
  if (!is.null(years)) {
    if (length(years) != 2L || any(!is.finite(years))) {
      stop("`years` (if provided) must be numeric vector of length 2.", call. = FALSE)
    }
    years <- sort(as.integer(years))
  }
  if (!is.character(local_region_col) || length(local_region_col) != 1L) {
    stop("`local_region_col` must be a single column name (string).", call. = FALSE)
  }
  
  # ---- Fetch region + sites + cover ---------------------------------------
  info <- get_regional_summary(tier_id)
  sdf  <- get_site_summary(tier_id) |> sf::st_drop_geometry()
  
  xdf <- get_benthic_cover(sdf$site_id) |>
    dplyr::filter(type == !!cover_type, depth == !!depth) |>
    # Note: API returns site id in `tier_id` for this endpoint; join preserves site metadata.
    dplyr::left_join(sdf, by = c("tier_id" = "site_id"))
  
  if (nrow(xdf) == 0) {
    stop("No data found for these filters (tier/cover_type/depth).", call. = FALSE)
  }
  
  if (!local_region_col %in% names(xdf)) {
    stop("Column `", local_region_col, "` not found in data. Available: ",
         paste(sort(names(xdf)), collapse = ", "), call. = FALSE)
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
  col_a  <- as.character(year_a)
  col_b  <- as.character(year_b)
  
  # ---- Pick the central estimate column (prefer mean -> median) -----------
  central_candidates <- c("mean", "median")
  central_col <- central_candidates[central_candidates %in% names(xdf)][1]
  if (is.na(central_col)) {
    stop("Central estimate column not found (looked for: mean, median).", call. = FALSE)
  }
  
  # Symbol for local region column
  lr_sym <- rlang::sym(local_region_col)
  
  # ---- Collapse to ONE value per site-year ---------------------------------
  # Keep API-returned site id in `site_id` from xdf$tier_id (as in your site-level fn)
  site_year <- xdf |>
    dplyr::filter(year %in% !!years) |>
    dplyr::group_by(
      site_id = tier_id,
      local_region = !!lr_sym,
      year = year
    ) |>
    dplyr::summarise(
      value = mean(!!rlang::sym(central_col), na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Tier means from the site-year table (consistent basis) --------------
  tier_means <- site_year |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n_sites    = sum(!is.na(value)),
      mean_cover = mean(value, na.rm = TRUE),
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
  
  # ---- Local Region means (derive from site-year) --------------------------
  # Summarise per local region and year
  lr_year <- site_year |>
    dplyr::group_by(local_region, year) |>
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      n_sites    = sum(!is.na(value)),
      .groups    = "drop"
    )
  
  # Pivot to get values for both years side-by-side
  lr_means <- lr_year |>
    dplyr::select(local_region, year, mean_value) |>
    tidyr::pivot_wider(
      names_from  = year,
      values_from = mean_value,
      names_sort  = TRUE
    )
  
  # n_sites in last year per local region
  lr_n_last <- lr_year |>
    dplyr::filter(year == !!year_b) |>
    dplyr::select(local_region, n_sites_last = n_sites)
  
  # Ensure columns exist (in case some year entirely missing after filtering)
  if (!col_a %in% names(lr_means)) lr_means[[col_a]] <- NA_real_
  if (!col_b %in% names(lr_means)) lr_means[[col_b]] <- NA_real_
  
  # Keep only local regions that have values for BOTH years
  lr_means <- lr_means |>
    dplyr::left_join(lr_n_last, by = "local_region")
  
  # ---- Build Local Region rows --------------------------------------------
  if (nrow(lr_means) > 0) {
    lr_rows <- lr_means |>
      dplyr::transmute(
        tier_id     = tier_id,
        region_name = info$region_name,
        cover_type  = cover_type,
        depth       = depth,
        row_type    = paste0("Local Region: ", local_region),
        year_first  = as.integer(year_a),
        year_last   = as.integer(year_b),
        n_sites     = as.integer(n_sites_last %||% 0L),
        mean_cover  = as.numeric(.data[[col_b]]),
        diff_from_first = as.numeric(.data[[col_b]] - .data[[col_a]]),
        pct_change_from_first = ifelse(
          is.finite(.data[[col_a]]) & .data[[col_a]] != 0,
          100 * (.data[[col_b]] - .data[[col_a]]) / .data[[col_a]],
          NA_real_
        )
      )
  } else {
    lr_rows <- dplyr::tibble(
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
  out <- dplyr::bind_rows(tier_row, lr_rows) |>
    dplyr::select(tier_id, region_name, cover_type, depth, row_type,
                  year_first, year_last, n_sites, mean_cover,
                  diff_from_first, pct_change_from_first)
  
  return(out)
}

# Local infix for simple null-coalesce used above
`%||%` <- function(x, y) if (is.null(x)) y else x