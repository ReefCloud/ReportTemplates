# =============================================================
# File: load_plot_palette.R
# Description: Load map palettes for categorical hard coral and macroalgae data for use with leaflet maps.
# Author: Samuel Chan
# Date: 2025-11-27
# Dependencies: ggplot2
# =============================================================
# 
#' Color Palettes for Benthic Cover Categories
#'
#' These named vectors define color palettes for mapping benthic cover categories in visualizations.
#' They are designed for use with `ggplot2` scales such as `scale_fill_manual()` or `scale_color_manual()`.
#'
#' @format Two named character vectors:
#' \describe{
#'   \item{hc.pal}{A palette for hard coral cover categories, using a pink gradient.}
#'   \item{ma.pal}{A palette for macroalgae cover categories, using a green gradient.}   
#'   \item{sc.pal}{A palette for soft coral cover categories, using a purple gradient.}
#' }
#'
#' @details
#' All palettes map the following domain values to colors:
#' \itemize{
#'   \item `"50 - 100%"` (darkest color)
#'   \item `"30 - 50%"`
#'   \item `"10 - 30%"`
#'   \item `"0 - 10%"` (lightest color)
#' }
#'
#' These palettes are useful for consistent color coding of benthic cover categories in charts and maps.
#'
#' @examples
#' # Example usage in ggplot2:
#' library(ggplot2)
#' df <- data.frame(
#'   category = factor(c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%")),
#'   value = c(80, 40, 20, 5)
#' )
#'
#' ggplot(df, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   scale_fill_manual(values = hc.pal) +
#'   theme_minimal()
#'
#' @seealso [ggplot2::scale_fill_manual()], [ggplot2::scale_color_manual()]
#' @export hc.pal
#' @export ma.pal
#' @export sc.pal

hc.pal <- c(
  "50 - 100%" = "#ae017e",
  "30 - 50%"  = "#f768a1",
  "10 - 30%"  = "#fbb4b9",
  "0 - 10%"   = "#feebe2"
)

ma.pal <- c(
  "50 - 100%" = "#238443",
  "30 - 50%"  = "#78c679",
  "10 - 30%"  = "#c2e699",
  "0 - 10%"   = "#ffffcc"
)

sc.pal <- c(
  "50 - 100%" = "#6a51a3",
  "30 - 50%"  = "#9e9ac8",
  "10 - 30%"  = "#cbc9e2",
  "0 - 10%"   = "#f2f0f7"
)

