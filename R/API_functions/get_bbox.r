# =============================================================
# File: get_bbox.R
# Description: Retrieve sites for a tier, clean attributes, and compute a WGS84 bbox with buffer.
# Author: Samuel Chan
# Date: 2026-02-03
# Dependencies: sf, dplyr
# Requires: get_site_summary()
# =============================================================

#' Get Cleaned Sites and a Buffered WGS84 Bounding Box for a Tier
#'
#' Retrieves sites via [`get_site_summary()`], extracts longitude/latitude from geometry,
#' arranges and de-duplicates records, optionally drops organisation fields, and computes
#' a WGS84 (\code{EPSG:4326}) bounding box using a geodesic buffer.
#'
#' @param tier_id Character (or coercible). ReefCloud Tier ID.
#' @param buffer_m Numeric, non-negative. Buffer distance in meters around the sites'
#'   convex hull before computing the bbox. Default `10` (meters).
#' @param drop_org_cols Logical. Drop `org_name` and `org_id` if present. Default `TRUE`.
#' @param return_bbox_as Character. One of:
#'   \itemize{
#'     \item `"bbox"` (default): returns an \code{sf} bbox object.
#'     \item `"list"`: named list with \code{xmin,ymin,xmax,ymax}.
#'     \item `"polygon"`: buffered hull as an `sf` polygon (WGS84).
#'   }
#'
#' @return A list with:
#' \itemize{
#'   \item \code{sites}: an `sf` object of cleaned sites including `lon`, `lat`
#'   \item \code{bbox}: a bbox/list/polygon depending on \code{return_bbox_as}
#' }
#'
#' @details
#' - If sites are not in WGS84, they are transformed to EPSG:4326 before any processing.
#' - The function expands the bbox by \eqn{buffer_m / 111000} degrees.
#' - To avoid zero-area bboxes for identical/near-identical site coordinates, a tiny
#'   minimal expansion is applied when the hull area is zero.
#'
#' @examples
#' \dontrun{
#' # Basic: fetch, clean, and get a small buffered bbox
#' out <- get_clean_sites_and_bbox(tier_id = 1705, buffer_m = 10)
#' out$sites
#' out$bbox
#'
#' # Supply a year to the underlying API:
#' out <- get_clean_sites_and_bbox(tier_id = "1705", buffer_m = 10,
#'                                 return_bbox_as = "list")
#' out$bbox  # named list xmin/ymin/xmax/ymax
#' }
#'
#' @export
get_bbox <- function(
    tier_id,
    buffer_m = 10,
    drop_org_cols = TRUE,
    return_bbox_as = c("bbox", "list", "polygon")
) {
  return_bbox_as <- match.arg(return_bbox_as)
  
  # ---- Validate inputs ----
  if (missing(tier_id) || length(tier_id) != 1 || is.na(tier_id)) {
    stop("`tier_id` must be a single, non-NA value.", call. = FALSE)
  }
  tier_id <- as.character(tier_id)
  if (!is.numeric(buffer_m) || !is.finite(buffer_m) || buffer_m < 0) {
    stop("`buffer_m` must be a non-negative, finite numeric (meters).", call. = FALSE)
  }
  
  # ---- Retrieve sites (sf expected) ----
  sites <- tryCatch(
    get_site_summary(tier_id = tier_id),
    error = function(e) stop("get_site_summary() failed: ", conditionMessage(e), call. = FALSE)
  )
  if (!inherits(sites, "sf")) {
    stop("get_site_summary() did not return an `sf` object.", call. = FALSE)
  }
  if (nrow(sites) == 0) {
    stop("No sites returned for the specified `tier_id`.", call. = FALSE)
  }
  
  # ---- Ensure WGS84 and add lon/lat ----
  sites_wgs <- if (sf::st_is_longlat(sites)) sites else sf::st_transform(sites, 4326)
  
  coords <- sf::st_coordinates(sf::st_geometry(sites_wgs))
  # coords[,1] = X (lon), coords[,2] = Y (lat)
  sites_wgs <- dplyr::mutate(
    sites_wgs,
    lon = coords[, 1],
    lat = coords[, 2]
  )
  
  # ---- Arrange, drop org cols ----
  if (drop_org_cols) {
    drop_cols <- intersect(c("org_name", "org_id"), names(sites_wgs))
    if (length(drop_cols)) {
      sites_wgs <- dplyr::select(sites_wgs, -dplyr::all_of(drop_cols))
    }
  }
  # Remove duplicates
    sites_wgs <- dplyr::distinct(sites_wgs)
  
  # ---- Build buffered hull and bbox (WGS84) ----
  # Convex hull of all points
  hull <- tryCatch(
    sf::st_convex_hull(sf::st_union(sf::st_geometry(sites_wgs))),
    error = function(e) sf::st_as_sfc(sf::st_bbox(sites_wgs), crs = sf::st_crs(4326))
  )
  
  # If hull has zero area (e.g., identical points), mildly expand to avoid degenerate bbox
  if (isTRUE(as.numeric(sf::st_area(hull)) == 0)) {
  # degree-based tiny nudge
      bb0 <- sf::st_bbox(hull)
      eps <- 0.5 / 111000
      hull <- sf::st_as_sfc(sf::st_bbox(c(
        xmin = bb0["xmin"] - eps,
        ymin = bb0["ymin"] - eps,
        xmax = bb0["xmax"] + eps,
        ymax = bb0["ymax"] + eps
      ), crs = sf::st_crs(4326)))
    }
  
  # Apply requested buffer
  buffered <- tryCatch({
    if (buffer_m > 0) {
      if (requireNamespace("lwgeom", quietly = TRUE)) {
        lwgeom::st_geod_buffer(hull, dist = buffer_m)
      } else {
        # Approximate: meters -> degrees (~111 km per degree)
        deg_pad <- buffer_m / 111000
        bb <- sf::st_bbox(hull)
        sf::st_as_sfc(sf::st_bbox(c(
          xmin = bb["xmin"] - deg_pad,
          ymin = bb["ymin"] - deg_pad,
          xmax = bb["xmax"] + deg_pad,
          ymax = bb["ymax"] + deg_pad
        ), crs = sf::st_crs(4326)))
      }
    } else {
      hull
    }
  }, error = function(e) hull)
  
  # ---- Prepare bbox return ----
  if (return_bbox_as == "polygon") {
    bbox_out <- sf::st_as_sf(buffered)
  } else {
    bb <- sf::st_bbox(buffered)
    if (return_bbox_as == "bbox") {
      bbox_out <- bb
    } else {
      bbox_out <- as.list(bb)[c("xmin", "ymin", "xmax", "ymax")]
    }
  }
  
  # ---- Return ----
  list(
    sites = sites_wgs,
    bbox  = bbox_out
  )
}
