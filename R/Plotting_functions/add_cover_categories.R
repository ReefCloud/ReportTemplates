# =============================================================
# File: add_cover_categories.R
# Description: Adds cover categories to benthic cover data frames for use in plotting.
# Author: Samuel Chan
# Date: 2025-11-13
# Dependencies: ggimage, ggplot2, tidyverse, scales, hrbrthemes
# =============================================================
# 

#' Add Cover Proportion and Category Columns as Ordered Factors
#'
#' This function adds two new columns to a benthic cover data frame:
#' \itemize{
#'   \item \code{cover_prop}: Factor representing cover ranges with levels:
#'     \item "50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"
#'   \item \code{cover_cat}: Factor representing category labels with levels:
#'     \item "A", "B", "C", "D"
#' }
#'
#' @param df A data frame containing numeric columns such as \code{low}, \code{mean}, \code{median}, or \code{high}.
#' @param column Character. The name of the column to classify (default = "median").
#'
#' @return The input data frame with two additional factor columns: \code{cover_prop} and \code{cover_cat}.
#'
#' @examples
#' df <- tibble::tibble(low = c(5, 15), mean = c(12, 40), median = c(8, 35), high = c(20, 60))
#' add_cover_categories(df, column = "mean")
#'
#' @export

add_cover_categories <- function(df, column = "median") {
  # Validate column
  if (!column %in% names(df)) {
    stop(sprintf("Column '%s' not found in data frame.", column))
  }
  
  df |> 
    dplyr::mutate(
      cover_prop = dplyr::case_when(
        .data[[column]] < 10 ~ "0 - 10%",
        .data[[column]] >= 10 & .data[[column]] < 30 ~ "10 - 30%",
        .data[[column]] >= 30 & .data[[column]] < 50 ~ "30 - 50%",
        .data[[column]] >= 50 ~ "50 - 100%"
      ),
      cover_cat = dplyr::case_when(
        .data[[column]] < 10 ~ "D",
        .data[[column]] >= 10 & .data[[column]] < 30 ~ "C",
        .data[[column]] >= 30 & .data[[column]] < 50 ~ "B",
        .data[[column]] >= 50 ~ "A"
      ),
      # Convert to factors with fixed levels
      cover_prop = factor(cover_prop, levels = c("50 - 100%", "30 - 50%", "10 - 30%", "0 - 10%"), ordered = TRUE),
      cover_cat = factor(cover_cat, levels = c("A", "B", "C", "D"), ordered = TRUE)
    )
}
