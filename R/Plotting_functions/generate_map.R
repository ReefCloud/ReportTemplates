
# =============================================================
# File: generate_map.R
# Description: Generates main, inset, and region maps for a ReefCloud tier (WGS84).
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: sf, ggplot2, rnaturalearth, cowplot, ggspatial, basemaps, dplyr
# Requires: get_regional_summary(), get_sites_summary(), getTiers()
# =============================================================

#' Generate Tier Maps (Main, Inset, Region) in WGS84
#'
#' Builds a basemap-centered figure showing the tier boundary and site locations
#' with all geometries in WGS84 (\code{EPSG:4326}). Produces:
#' \itemize{
#'   \item Main map (boundary + sites) with scale bar
#'   \item Inset map (country extent + boundary highlight)
#'   \item Region map (country + boundary highlight)
#' }
#' Saves PNGs to \code{figures/Site_Map.png} and \code{figures/Region_Map.png}.
#'
#' @param tier_id Character. ReefCloud Tier ID used to fetch regional summary, sites, and boundary.
#' @param buffer_m Numeric. Geodesic buffer distance in meters applied to the sites extent
#'   prior to computing the tier \code{bbox}. Default \code{10000} (10 km).
#' @param boundary_level Integer. Tier level to request for the boundary via \code{getTiers()}.
#'   Default \code{4}.
#' @param basemap_service Character. Basemap service for \code{basemaps::set_defaults()}, e.g., \code{"carto"}.
#'   Default \code{"carto"}.
#' @param basemap_type Character. Basemap type for \code{basemaps::set_defaults()}, e.g., \code{"light"}.
#'   Default \code{"light"}.
#' @param out_dir Character. Output directory to save PNGs. Default \code{"figures"}.
#' @param site_point_size Numeric. Point size for site markers. Default \code{3}.
#' @param site_point_color Character. Color for site markers. Default \code{"red"}.
#' @param site_point_alpha Numeric in [0,1]. Alpha for site markers. Default \code{0.5}.
#' @param boundary_line_color Character. Color for boundary outline. Default \code{"black"}.
#' @param boundary_line_size Numeric. Line width for boundary outline. Default \code{3}.
#' @param inset_fill Character. Fill color for country in the inset map. Default \code{"lightblue"}.
#' @param boundary_fill Character. Fill color for boundary polygon in inset/region maps. Default \code{"red"}.
#' @param boundary_alpha Numeric in [0,1]. Alpha for boundary fill in inset/region maps. Default \code{0.5}.
#' @param inset_pos Named list with \code{x, y, width, height} for positioning the inset on the main map.
#'   Defaults to \code{list(x = 0.88, y = 0.3, width = 0.25, height = 0.25)}.
#'
#' @return Invisibly returns a list with \code{main_map}, \code{inset_map}, \code{region_map}, and
#'   \code{combined_map} (all \code{ggplot} objects).
#'
#' @details
#' - All geometries are kept in WGS84 (\code{EPSG:4326}).
#' - If \strong{\code{lwgeom}} is available, sites extent is buffered geodesically in meters using
#'   \code{lwgeom::st_geod_buffer()} before computing the \code{bbox}. Otherwise, the function falls back
#'   to a degree-based bbox expansion (conservative, approximate).
#' - If country detection fails (\code{sites$site_country} missing or mismatched against Natural Earth),
#'   inset/region maps fall back to the world extent.
#' - Output PNGs are saved to \code{out_dir}. The directory is created if it does not exist.
#'
#' @examples
#' \dontrun{
#' generate_map("tier_id")                 # default settings, WGS84
#' generate_map("tier_id", buffer_m = 20000, basemap_type = "voyager")
#' }
#'
#' @export
generate_map <- function(
    tier_id,
    buffer_m = 10000,
    boundary_level = 4,
    basemap_service = "carto",
    basemap_type = "light",
    out_dir = "figures",
    site_point_size = 3,
    site_point_color = "red",
    site_point_alpha = 0.5,
    boundary_line_color = "black",
    boundary_line_size = 3,
    inset_fill = "lightblue",
    boundary_fill = "red",
    boundary_alpha = 0.5,
    inset_pos = list(x = 0.88, y = 0.3, width = 0.25, height = 0.25)
) {
  # ---- Validate inputs ----
  if (missing(tier_id) || length(tier_id) != 1) {
    stop("`tier_id` must be specified.")
  }
  if (!is.numeric(buffer_m) || !is.finite(buffer_m) || buffer_m < 0) {
    stop("`buffer_m` must be a non-negative, finite numeric value (meters).")
  }
  if (!is.numeric(boundary_level) || length(boundary_level) != 1) {
    stop("`boundary_level` must be a single integer-like numeric value.")
  }
  stopifnot(is.list(inset_pos), all(c("x","y","width","height") %in% names(inset_pos)))
  
  # ---- Load external helper functions ----
  # Replace `source()` with Imports + namespace in a package.
  source("R/API_functions/get_regional_summary.r")
  source("R/API_functions/get_site_summary.r")         
  source("R/API_functions/get_tier_id.r")              # Assuming getTiers() lives here
  
  # ---- Fetch data (defensive) ----
  info  <- tryCatch(get_regional_summary(tier_id), error = function(e) NULL)
  sites <- tryCatch(get_site_summary(tier_id),    error = function(e) NULL)
  
  if (is.null(sites)) stop("Failed to retrieve sites via `get_sites_summary()`.")
  if (!inherits(sites, "sf")) stop("`sites` must be an sf object with point geometries.")
  if (nrow(sites) == 0) stop("No site records returned for the specified `tier_id`.")
  
  # ---- Ensure WGS84 CRS ----
  sites_wgs <- if (sf::st_is_longlat(sites)) sites else sf::st_transform(sites, 4326)
  
  # ---- Compute buffered bbox in WGS84 ----
  # Prefer geodesic buffer in meters if lwgeom is available; else degree expansion fallback.
  bbox_wgs <- tryCatch({
    if (requireNamespace("lwgeom", quietly = TRUE)) {
      hull   <- sf::st_convex_hull(sf::st_union(sf::st_geometry(sites_wgs)))
      hull_b <- lwgeom::st_geod_buffer(hull, dist = buffer_m)
      sf::st_bbox(hull_b)
    } else {
      # Fallback: expand bbox by degrees approx. (1 deg ~ 111 km at equator)
      base_bbox <- sf::st_bbox(sites_wgs)
      deg_pad   <- buffer_m / 111000  # ~ meters to degrees
      sf::st_bbox(c(
        xmin = base_bbox["xmin"] - deg_pad,
        ymin = base_bbox["ymin"] - deg_pad,
        xmax = base_bbox["xmax"] + deg_pad,
        ymax = base_bbox["ymax"] + deg_pad
      ), crs = sf::st_crs(sites_wgs))
    }
  }, error = function(e) {
    # Ultimate fallback: plain bbox without buffer
    sf::st_bbox(sites_wgs)
  })
  
  # ---- Get tier boundary within bbox (largest polygon selection) ----
  boundary <- tryCatch(get_tier_id(tier_level = boundary_level, bbox = bbox_wgs), error = function(e) NULL)
  if (is.null(boundary)) stop("Failed to retrieve tier boundary via `get_tier_id()`.")
  if (!inherits(boundary, "sf")) stop("`boundary` must be an sf object.")
  
  # Ensure WGS84 and polygonal geometry; select largest polygon
  boundary_wgs <- if (sf::st_crs(boundary)$epsg == 4326) boundary else sf::st_transform(boundary, 4326)
  boundary_wgs <- boundary_wgs |>
    sf::st_cast("POLYGON", warn = FALSE) |>
    dplyr::mutate(.area = as.numeric(sf::st_area(boundary_wgs))) |>
    dplyr::slice_max(order_by = .area, n = 1, with_ties = FALSE) |>
    dplyr::select(-.area)
  
  # ---- Natural Earth: world and country extent in WGS84 ----
  world_wgs <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  if (!sf::st_is_longlat(world_wgs)) world_wgs <- sf::st_transform(world_wgs, 4326)
  
  country <- tryCatch(unique(stats::na.omit(sites_wgs$site_country)), error = function(e) NULL)
  country <- country[1] %||% NA_character_
  if (!is.na(country) && country %in% world_wgs$name) {
    country_wgs <- world_wgs[world_wgs$name == country, ]
  } else {
    # Fallback to world extent
    country_wgs <- world_wgs
  }
  
  # ---- Basemap defaults ----
  basemaps::set_defaults(map_service = basemap_service, map_type = basemap_type)
  
  # ---- Build main map (WGS84) ----
  main_map <- ggplot2::ggplot() +
    basemaps::basemap_gglayer(boundary_wgs) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_sf(data = boundary_wgs, color = boundary_line_color, fill = NA, size = boundary_line_size) +
    ggplot2::geom_sf(data = sites_wgs,    color = site_point_color,  size = site_point_size, alpha = site_point_alpha) +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(boundary_wgs)[c("xmin", "xmax")],
      ylim = sf::st_bbox(boundary_wgs)[c("ymin", "ymax")]
    ) +
    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.5,
      pad_x = grid::unit(0.5, "cm"), pad_y = grid::unit(0.5, "cm"),
      unit_category = "metric"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_blank()
    )
  
  # ---- Build inset map (country extent, WGS84) ----
  inset_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_wgs,   fill = NA, color = "grey70", size = 0.2) +
    ggplot2::geom_sf(data = country_wgs, fill = inset_fill, color = NA) +
    ggplot2::geom_sf(data = boundary_wgs, fill = boundary_fill, color = "#290101f1", alpha = boundary_alpha) +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(country_wgs)[c("xmin", "xmax")],
      ylim = sf::st_bbox(country_wgs)[c("ymin", "ymax")]
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.3)
    )
  
  # ---- Build region map (country + boundary, WGS84) ----
  region_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_wgs,   fill = NA, color = NA) +
    ggplot2::geom_sf(data = country_wgs, fill = "#616161", color = NA) +
    ggplot2::geom_sf(data = boundary_wgs, fill = boundary_fill, color = "#290101f1", alpha = boundary_alpha) +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(country_wgs)[c("xmin", "xmax")],
      ylim = sf::st_bbox(country_wgs)[c("ymin", "ymax")]
    ) +
    ggplot2::theme_void()
  
  # ---- Compose combined map with inset ----
  combined_map <- cowplot::ggdraw() +
    cowplot::draw_plot(main_map) +
    cowplot::draw_plot(
      inset_map,
      x      = inset_pos$x, y = inset_pos$y,
      width  = inset_pos$width, height = inset_pos$height,
      vjust  = 1, halign = 1, valign = 1, hjust = 1
    )
  
  # ---- Save outputs ----
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggplot2::ggsave(file.path(out_dir, "Site_Map.png"),   combined_map, width = 5, height = 4, dpi = 300)
  ggplot2::ggsave(file.path(out_dir, "Region_Map.png"), region_map,   width = 3, height = 4, dpi = 300)
  
  invisible(list(
    main_map     = main_map,
    inset_map    = inset_map,
    region_map   = region_map,
    combined_map = combined_map
  ))
}

# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x
