# =============================================================
# File: export_cpce_csv.R
# Description: Retrieves coordinates from CPCe files
# Author: Samuel Chan
# Date: 2026-01-12
# Dependencies: readr, stringr, purrr, tibble
# =============================================================

#' Export CPCe points into a CSV in the same folder
#'
#' @description
#' Scans the provided directory (non-recursive) for `.cpc` files, parses each using
#' [read_cpce()], row-binds results, and writes a CSV *into the same folder*.
#' The CSV filename encodes the **last four folder levels** of the provided path.
#'
#' @param dir Character scalar. Directory to search for `.cpc` files.
#' @param scale_factor Numeric scalar. CPC units per pixel; **15** (default) or **12** for legacy data.
#' @param round_to_pixel Logical; round to nearest pixel. Default `TRUE`.
#' @param fail_on_error Logical; if `TRUE`, stop on the first parse error.
#'   If `FALSE`, skip files that error and continue (with a warning). Default `TRUE`.
#' @param overwrite Logical; if `TRUE`, overwrite the output CSV if it exists.
#'   If `FALSE`, error when the target CSV already exists. Default `TRUE`.
#'
#' @return Invisibly returns the combined tibble. Also writes the CSV to disk in `dir`.
#'
#' @examples
#' \dontrun{
#' # Writes to the same folder, filename based on last 4 levels:
#' export_cpce_csv("C:/reef/surveys")
#'
#' # Legacy data:
#' export_cpce_csv("D:/legacy_cpc/reef/surveys", scale_factor = 12)
#' }
#' @seealso [read_cpce()]
#' @export
<- function(dir,
            scale_factor = 15,
            round_to_pixel = TRUE,
            fail_on_error = TRUE,
            overwrite = TRUE) {
  # ---- validation ----
  if (!is.character(dir) || length(dir) != 1L) {
    stop("`dir` must be a single character directory path.", call. = FALSE)
  }
  if (!dir.exists(dir)) stop("Directory does not exist: ", dir, call. = FALSE)
  if (!is.numeric(scale_factor) || length(scale_factor) != 1L || !scale_factor %in% c(12, 15)) {
    stop("`scale_factor` must be 12 (legacy) or 15 (default).", call. = FALSE)
  }
  if (!is.logical(round_to_pixel) || length(round_to_pixel) != 1L) {
    stop("`round_to_pixel` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(fail_on_error) || length(fail_on_error) != 1L) {
    stop("`fail_on_error` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("`overwrite` must be a single logical value.", call. = FALSE)
  }
  
  # ---- derive "last 4 levels" name ----
  # Normalize to forward slashes, split into components
  norm <- gsub("\\\\", "/", dir)
  parts <- strsplit(norm, "/", fixed = TRUE)[[1]]
  # Drop empty parts that can occur with leading drive/leading slash
  parts <- parts[nzchar(parts)]
  # Take the last up-to-4 components
  last4 <- utils::tail(parts, n = min(4L, length(parts)))
  # Sanitize each component to be filename-safe, then join with "__"
  sanitize <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)
  last4_safe <- vapply(last4, sanitize, character(1))
  suffix <- paste(last4_safe, collapse = "__")
  # Ensure not overly long; trim conservatively if needed
  if (nchar(suffix) > 180) suffix <- paste0(substr(suffix, 1, 180), "_trim")
  out_csv <- file.path(dir, paste0("cpce_points_", suffix, ".csv"))
  
  # ---- discover files in the folder (non-recursive) ----
  files <- list.files(
    path = dir,
    pattern = "\\.cpc$",
    recursive = FALSE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(files)) {
    stop("No `.cpc` files found in: ", dir, call. = FALSE)
  }
  
  # ---- parse each file ----
  parse_one <- function(fp) {
    read_cpce(fp,
              scale_factor = scale_factor,
              round_to_pixel = round_to_pixel) |>
      dplyr::mutate(.path = fp)
  }
  
  if (fail_on_error) {
    dfl <- purrr::map(files, parse_one)
  } else {
    dfl <- purrr::map(
      files,
      ~ tryCatch(parse_one(.x),
                 error = function(e) {
                   warning("Skipped '", .x, "' due to error: ", conditionMessage(e))
                   NULL
                 })
    )
    dfl <- purrr::compact(dfl)
  }
  
  if (!length(dfl)) {
    stop("No `.cpc` files could be parsed successfully.", call. = FALSE)
  }
  
  combined <- dplyr::bind_rows(dfl)
  
  # ---- write CSV ----
  if (file.exists(out_csv) && !overwrite) {
    stop("Output file already exists and `overwrite = FALSE`: ", out_csv, call. = FALSE)
  }
  readr::write_csv(combined, out_csv)
  message("Wrote combined CPCe dataset to: ", out_csv)
  
  invisible(combined)
}
