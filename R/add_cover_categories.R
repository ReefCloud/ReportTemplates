# =============================================================
# File: add_cover_categories.R
# Description: Adds range-based cover categories to benthic cover data frames for plotting.
# Author: Samuel Chan
# Date: 2026-01-07
# Dependencies: dplyr, rlang
# =============================================================

#' Add Range-Based Cover Proportion and Category Columns (Ordered Factors)
#'
#' Classifies benthic cover values from a specified numeric column into:
#' \itemize{
#'   \item A human-readable proportion range column: \code{cover_prop}
#'   \item An ordinal category column: \code{cover_cat} with levels \code{A} (highest) to \code{D} (lowest)
#' }
#'
#' Default ranges and labels:
#' \itemize{
#'   \item \code{"0 - 10%"}  → \code{D}
#'   \item \code{"10 - 30%"} → \code{C}
#'   \item \code{"30 - 50%"} → \code{B}
#'   \item \code{"50 - 100%"} → \code{A}
#' }
#'
#' @param df A data frame containing a numeric column to classify (e.g., \code{low}, \code{mean}, \code{median}, \code{high}).
#' @param column Character. The name of the column to classify. Default \code{"median"}.
#' @param breaks Numeric vector of length 3 giving the lower bounds for the 2nd, 3rd, and 4th ranges respectively,
#'   in ascending order. Default \code{c(10, 30, 50)} results in:
#'   \code{"0 - 10%"}, \code{"10 - 30%"}, \code{"30 - 50%"}, \code{"50 - 100%"}.
#' @param prop_labels Character vector of length 4 defining the labels for the ranges, in ascending order
#'   (lowest to highest). Default \code{c("0 - 10%", "10 - 30%", "30 - 50%", "50 - 100%")}.
#' @param cat_labels Character vector of length 4 defining category codes for the ranges, in ascending order
#'   (lowest to highest). Default \code{c("D", "C", "B", "A")}.
#'
#' @return The input data frame with two additional ordered factor columns:
#' \itemize{
#'   \item \code{cover_prop}: One of \code{prop_labels}, ordered descending for plotting convenience.
#'   \item \code{cover_cat}: One of \code{cat_labels}, ordered descending (e.g., \code{A > B > C > D}).
#' }
#'
#' @details
#' - Missing values in the metric column propagate as \code{NA} in both \code{cover_prop} and \code{cover_cat}.
#' - Boundary handling:
#'   \itemize{
#'     \item First bin: \code{[0, b1)} — values strictly less than \code{b1} (e.g., \code{< 10}).
#'     \item Middle bins: \code{[b1, b2)} and \code{[b2, b3)} — lower bound inclusive, upper bound exclusive.
#'     \item Final bin: \code{[b3, 100]} — lower bound inclusive; upper bound inclusive to capture \code{100}.
#'   }
#' - Factor levels are set to descending order (\code{rev(prop_labels)} and \code{rev(cat_labels)}) for plotting convenience.
#'
#' @examples
#' df <- tibble::tibble(median = c(NA, 5, 10, 15, 30, 35, 50, 55, 100))
#' add_cover_categories(df, column = "median")
#'
#' # Custom breaks and labels
#' add_cover_categories(
#'   df, column = "median",
#'   breaks = c(10, 25, 40),
#'   prop_labels = c("0 - 10%", "10 - 25%", "25 - 40%", "40 - 100%"),
#'   cat_labels  = c("D", "C", "B", "A")
#' )
#'
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
add_cover_categories <- function(
    df,
    column = "median",
    breaks = c(10, 30, 50),
    prop_labels = c("0 - 10%", "10 - 30%", "30 - 50%", "50 - 100%"),
    cat_labels  = c("D", "C", "B", "A")
) {
  # ---- Validate inputs ----
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  if (!is.character(column) || length(column) != 1) {
    stop("`column` must be a single string.")
  }
  if (!column %in% names(df)) {
    stop(sprintf("Column '%s' not found in data frame.", column))
  }
  if (!is.numeric(df[[column]])) {
    stop(sprintf("Column '%s' must be numeric.", column))
  }
  if (!is.numeric(breaks) || length(breaks) != 3 || any(!is.finite(breaks))) {
    stop("`breaks` must be a numeric vector of length 3 with finite values (e.g., c(10, 30, 50)).")
  }
  if (!is.character(prop_labels) || length(prop_labels) != 4) {
    stop("`prop_labels` must be a character vector of length 4, ascending from lowest to highest.")
  }
  if (!is.character(cat_labels) || length(cat_labels) != 4) {
    stop("`cat_labels` must be a character vector of length 4, ascending from lowest to highest (lowest first).")
  }
  
  # Sort breaks to ensure ascending thresholds (lowest -> highest)
  b  <- sort(breaks)
  b1 <- b[1]  # lower bound for 2nd range
  b2 <- b[2]  # lower bound for 3rd range
  b3 <- b[3]  # lower bound for 4th (top) range
  
  # ---- Compute range-based categories; preserve NAs ----
  df <- dplyr::mutate(
    df,
    cover_prop = dplyr::case_when(
      is.na(.data[[column]])                       ~ NA_character_,
      .data[[column]] <  b1                        ~ prop_labels[1], # 0 - b1 (upper exclusive)
      .data[[column]] >= b1 & .data[[column]] < b2 ~ prop_labels[2], # b1 - b2 (upper exclusive)
      .data[[column]] >= b2 & .data[[column]] < b3 ~ prop_labels[3], # b2 - b3 (upper exclusive)
      .data[[column]] >= b3                        ~ prop_labels[4]  # b3 - 100 (upper inclusive by label)
    ),
    cover_cat = dplyr::case_when(
      is.na(.data[[column]])                       ~ NA_character_,
      .data[[column]] <  b1                        ~ cat_labels[1],  # D (lowest)
      .data[[column]] >= b1 & .data[[column]] < b2 ~ cat_labels[2],  # C
      .data[[column]] >= b2 & .data[[column]] < b3 ~ cat_labels[3],  # B
      .data[[column]] >= b3                        ~ cat_labels[4]   # A (highest)
    )
  )
  
  # ---- Convert to ordered factors (descending for plotting convenience) ----
  df$cover_prop <- factor(df$cover_prop, levels = rev(prop_labels), ordered = TRUE)
  df$cover_cat  <- factor(df$cover_cat,  levels = rev(cat_labels),  ordered = TRUE)
  
  return(df)
}
