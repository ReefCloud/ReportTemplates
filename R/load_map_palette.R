# =============================================================
# File: load_map_palette.R
# Description: Load map palettes for categorical hard coral and macroalgae data for use with leaflet maps.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: leaflet
# =============================================================

#' Color Palettes for Benthic Cover Categories 
#'
#' These objects define color palettes for mapping benthic cover categories in visualizations.
#' They use `leaflet::colorFactor()` to create color functions based on predefined domains.
#'
#' @format Two objects:
#' \describe{
#'   \item{map_palette_hc}{A color function for hard coral cover categories, using a pink-purple palette.}
#'   \item{map_palette_ma}{A color function for macroalgae cover categories, using a green palette.}
#' }
#'
#' @details
#' Both palettes map the following domain values:
#' \itemize{
#'   \item `"50 - 100%"`
#'   \item `"30 - 50%"`
#'   \item `"10 - 30%"`
#'   \item `"0 - 10%"`
#' }
#' Colors are reversed so that higher cover percentages correspond to darker colors.
#'
#' @examples
#' # Example usage in a leaflet map:
#' leaflet() %>%
#'   addTiles() %>%
#'   addCircleMarkers(
#'     data = some_data,
#'     color = ~map_palette_hc(hard_coral_category)
#'   )
#'
#' @seealso [leaflet::colorFactor()]
#' @export map_palette_hc
#' @export map_palette_ma

map_palette_hc <- leaflet::colorFactor(
  palette = c("#ae017e", "#f768a1", "#fbb4b9", "#feebe2"),
  domain = c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"),
  reverse = TRUE
)

map_palette_ma <- leaflet::colorFactor(
  palette = c("#238443", "#78c679", "#c2e699", "#ffffcc"),
  domain = c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"),
  reverse = TRUE
)
