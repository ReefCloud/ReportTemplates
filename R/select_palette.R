# =============================================================
# File: select_palette.R
# Description: Helper function to select palettes based on input
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: ggplot2
# =============================================================
#' Select a palette by benthic group and domain
#'
#' Convenience helper to retrieve a named palette vector for use with
#' \code{cover_prop} or \code{cover_cat}.
#'
#' @param group Character. One of \code{"hc"}, \code{"ma"}, \code{"sc"}.
#' @param use Character. Domain: \code{"prop"} for \code{cover_prop} labels,
#'   or \code{"cat"} for \code{cover_cat} A–D codes.
#'
#' @return A named character vector of HEX colors suitable for
#'   [ggplot2::scale_fill_manual()] or [ggplot2::scale_color_manual()].
#'
#' @examples
#' select_palette("hc", use = "prop")
#' 
#' # Helper to pick palette by domain ('prop' or 'cat') and group ('hc', 'ma', 'sc')
#' ggplot(df_prop, aes(x = cover_prop, y = value, fill = cover_prop)) +
#'   geom_col() +
#'   scale_fill_manual(values = select_palette("hc", use = "prop")) +
#' @export select_palette

select_palette <- function(group = c("hc", "ma", "sc"), use = c("prop", "cat")) {
  
  source("R/load_plot_palette.R")
  
  group <- match.arg(group)
  use   <- match.arg(use)
  switch(
    paste(group, use, sep = "_"),
    "hc_prop" = hc.pal_prop,
    "ma_prop" = ma.pal_prop,
    "sc_prop" = sc.pal_prop,
    "hc_cat"  = hc.pal_cat,
    "ma_cat"  = ma.pal_cat,
    "sc_cat"  = sc.pal_cat
  )
}