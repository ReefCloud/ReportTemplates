# =============================================================
# File: integer_breaks.R
# Description: Helper function to generate integer axis breaks for ggplot2 plots.
# Author: [Your Name]
# Date: 2025-11-13
# Dependencies: base R
# =============================================================

#' Generate Integer Breaks for ggplot2 Axis
#'
#' This helper function returns a function that generates integer breaks for ggplot2 axes using `pretty()` and `floor()`.
#'
#' @param n Integer. Desired number of breaks (default is 5).
#' @param ... Additional arguments passed to `pretty()`.
#'
#' @return A function that takes a numeric vector and returns integer axis breaks.
#'
#' @examples
#' scale_x_continuous(breaks = integer_breaks())

integer_breaks <- function(n = 5, ...) {
	fxn <- function(x) {
		breaks <- floor(pretty(x, n, ...))
		names(breaks) <- attr(breaks, "labels")
		breaks
	}
	return(fxn)
}
