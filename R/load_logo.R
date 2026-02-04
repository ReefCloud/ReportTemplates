# =============================================================
# File: load_logo.R
# Description: Helper function to generate ReefCloud logo
# Author: Manuel Gonzalez-Rivero
# Date: 2025-11-13
# Dependencies: base R
# =============================================================

#' Load ReefCloud Logo as a Raster Graphic
#'
#' This function retrieves the ReefCloud logo from the specified URL, converts it from SVG format to a raster image,
#' and returns it as a `rasterGrob` object suitable for use in plots or graphical layouts.
#'
#' @return A `grid::rasterGrob` object containing the ReefCloud logo.
#'
#' @details
#' The function uses the `rsvg` package to render the SVG logo and the `grid` package to create a raster graphic object.
#' 
#' @examples
#' # Example usage:
#' logo_grob <- load_logo()
#' grid::grid.draw(logo_grob)
#' 
#' @export

load_logo <- function() {
  logo_url <- "https://reefcloud.ai/dashboard/logo.fdd3d79b2ab59591.svg"
  logo <- rsvg::rsvg(logo_url)
  grid::rasterGrob(logo, interpolate = TRUE)
}