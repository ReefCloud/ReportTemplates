# =============================================================
# File: load_plot_palette.R
# Description: Defines color palettes for benthic cover ranges (hard coral, macroalgae, soft coral)
#              compatible with either `cover_prop` (range labels) or `cover_cat` (A–D codes).
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: ggplot2
# =============================================================

#' Color Palettes for Benthic Cover (Prop Ranges or Category Codes)
#'
#' Named character vectors defining color palettes keyed to either:
#' \itemize{
#'   \item \code{cover_prop}: \code{"50 - 100%"}, \code{"30 - 50%"}, \code{"10 - 30%"}, \code{"0 - 10%"}
#'   \item \code{cover_cat}:  \code{"A"}, \code{"B"}, \code{"C"}, \code{"D"}
#' }
#'
#' Use with [ggplot2::scale_fill_manual()] or [ggplot2::scale_color_manual()].
#'
#' @format Six named character vectors:
#' \describe{
#'   \item{hc.pal_prop}{Hard coral palette keyed to \code{cover_prop} (pink gradient).}
#'   \item{ma.pal_prop}{Macroalgae palette keyed to \code{cover_prop} (green gradient).}
#'   \item{sc.pal_prop}{Soft coral palette keyed to \code{cover_prop} (purple gradient).}
#'   \item{hc.pal_cat}{Hard coral palette keyed to \code{cover_cat} A–D (pink gradient).}
#'   \item{ma.pal_cat}{Macroalgae palette keyed to \code{cover_cat} A–D (green gradient).}
#'   \item{sc.pal_cat}{Soft coral palette keyed to \code{cover_cat} A–D (purple gradient).}
#'   \item{group.pal}{Benthic groups palette keyed to \code{type} hard coral, soft coral, macroalgae, turf algae, crustose coralline algae, other (discrete colours).}
#'   \item{groupcode.pal}{Benthic groups palette keyed to \code{type_code} hc, sc, ma, ta, ca, ot (discrete colours).}
#' }
#'
#' @details
#' Palettes are defined in **descending** order (highest first) to align with factor levels
#' you set via \code{rev(...)} for plotting convenience:
#' \itemize{
#'   \item \code{cover_prop}: \code{"50 - 100%"} > \code{"30 - 50%"} > \code{"10 - 30%"} > \code{"0 - 10%"}
#'   \item \code{cover_cat}:  \code{A > B > C > D}
#' }
#'
#' @examples
#' library(ggplot2)
#' # Example with cover_prop
#' df_prop <- data.frame(
#'   cover_prop = factor(
#'     c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"),
#'     levels = c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"),
#'     ordered = TRUE
#'   ),
#'   value = c(80, 40, 20, 5)
#' )
#'
#' ggplot(df_prop, aes(x = cover_prop, y = value, fill = cover_prop)) +
#'   geom_col() +
#'   scale_fill_manual(values = hc.pal_prop) +
#'   theme_minimal()
#'
#' # Example with cover_cat
#' df_cat <- data.frame(
#'   cover_cat = factor(
#'     c("A", "B", "C", "D"),
#'     levels = c("A", "B", "C", "D"),
#'     ordered = TRUE
#'   ),
#'   value = c(80, 40, 20, 5)
#' )
#'
#' ggplot(df_cat, aes(x = cover_cat, y = value, fill = cover_cat)) +
#'   geom_col() +
#'   scale_fill_manual(values = ma.pal_cat) +
#'   theme_minimal()
#'
#'
#' @seealso [ggplot2::scale_fill_manual()], [ggplot2::scale_color_manual()]
#' @export hc.pal_prop
#' @export ma.pal_prop
#' @export sc.pal_prop
#' @export hc.pal_cat
#' @export ma.pal_cat
#' @export sc.pal_cat
#' @export group.pal
#' @export groupcode.pal

# ---- cover_prop palettes (labels) ----
hc.pal_prop <- c(
  "50 - 100%" = "#ae017e",
  "30 - 50%"  = "#f768a1",
  "10 - 30%"  = "#fbb4b9",
  "0 - 10%"   = "#feebe2"
)

ma.pal_prop <- c(
  "50 - 100%" = "#238443",
  "30 - 50%"  = "#78c679",
  "10 - 30%"  = "#c2e699",
  "0 - 10%"   = "#ffffcc"
)

sc.pal_prop <- c(
  "50 - 100%" = "#6a51a3",
  "30 - 50%"  = "#9e9ac8",
  "10 - 30%"  = "#cbc9e2",
  "0 - 10%"   = "#f2f0f7"
)

# ---- cover_cat palettes (A–D) ----
# Mapping: A (highest), B, C, D (lowest)
hc.pal_cat <- c(
  "A" = "#ae017e",
  "B" = "#f768a1",
  "C" = "#fbb4b9",
  "D" = "#feebe2"
)

ma.pal_cat <- c(
  "A" = "#238443",
  "B" = "#78c679",
  "C" = "#c2e699",
  "D" = "#ffffcc"
)

sc.pal_cat <- c(
  "A" = "#6a51a3",
  "B" = "#9e9ac8",
  "C" = "#cbc9e2",
  "D" = "#f2f0f7"
)

group.pal <- c(
  "hard coral" = "#f768a1",
  "soft coral" = "#9e9ac8",
  "macroalgae" = "#78c679",
  "turf algae" = "#80b1d3",
  "crustose coralline algae" = "#fdb462",
  "other" = "#ffffb3"
)

groupcode.pal <- c(
  "hc" = "#f768a1",
  "sc" = "#9e9ac8",
  "ma" = "#78c679",
  "ta" = "#80b1d3",
  "ca" = "#fdb462",
  "ot" = "#ffffb3"
)