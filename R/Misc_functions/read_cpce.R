# =============================================================
# File: read_cpce.R
# Description: Retrieves data from CPCe (.cpc) files
# Author: Samuel Chan
# Date: 2026-01-12
# Dependencies: readr, stringr, purrr, tibble
# =============================================================

#' Read a CPCe (.cpc) file and extract X/Y coordinates and annotations
#'
#' @description
#' Parses a CPCe `.cpc` data file into a tidy tibble containing raw CPC-unit
#' coordinates, converted **1‑based** pixel positions, and per-point label short codes.
#' Defaults to `scale_factor = 15` (CPCe >= 4.0). For legacy data, set `scale_factor = 12`.
#'
#' @param path Character scalar. Path to a `.cpc` file.
#' @param scale_factor Numeric scalar. CPC units per pixel. Default `15`.
#'   Set `12` for legacy/non‑96 DPI cases.
#' @param round_to_pixel Logical; round to nearest whole pixel. Default `TRUE`.
#'
#' @return A tibble with:
#' \itemize{
#'   \item `file`        — base file name
#'   \item `image_name`  — image name
#'   \item `point_id`    — sequential point index
#'   \item `x_cpc`, `y_cpc` — raw CPC coordinates from the file
#'   \item `x_px`,  `y_px`  — converted pixel coordinates (1‑based)
#'   \item `label_code`     — short code per point; may be `NA` if absent
#' }
#'
#' @details
#' **Annotations parsing strategy:**
#' * First tries to capture a trailing short code on the **same line** as each X,Y pair.
#' * If missing, looks for a **contiguous block** of `n` label lines immediately
#'   following the coordinate block. Supports quoted CSV rows like
#'   `"A","CAR","Notes",""` and extracts the **second field** (`CAR`).
#' * If neither pattern is found, `label_code` is `NA`.
#'
#' @examples
#' \dontrun{
#' pts  <- read_cpce("data/IMG_0001.cpc")              # default scale_factor = 15
#' pts12 <- read_cpce("data/legacy.cpc", scale_factor = 12)  # legacy
#' }
#' @export
read_cpce <- function(path,
                      scale_factor = 15,
                      round_to_pixel = TRUE) {
  
  # Validation
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string to a .cpc file.", call. = FALSE)
  }
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (!is.numeric(scale_factor) || length(scale_factor) != 1L) {
    stop("`scale_factor` must be a single numeric value (15 default, or 12 for legacy).", call. = FALSE)
  }
  
  # Base file name and image_name without .cpc
  base_file   <- basename(path)
  image_name  <- sub("\\.cpc$", "", base_file, ignore.case = TRUE)
  
  lines <- readr::read_lines(path)
  
  # Numeric pair extractor
  num_pair <- function(s) {
    m <- stringr::str_match(s, "(-?\\d+(?:\\.\\d+)?)[,\\s]+(-?\\d+(?:\\.\\d+)?)")
    if (is.na(m[1, 1])) return(NULL)
    as.numeric(m[1, 2:3])
  }
  
  # Trailing label after numeric pair, e.g. "23563,31588  HC"
  trailing_label <- function(s) {
    m <- stringr::str_match(s, "(-?\\d+(?:\\.\\d+)?)[,\\s]+(-?\\d+(?:\\.\\d+)?)\\s*(.*)$")
    if (is.na(m[1, 1])) return(NA_character_)
    tail <- stringr::str_trim(m[1, 4])
    if (tail == "") return(NA_character_)
    lab <- stringr::str_match(tail, "^([A-Za-z0-9_\\-./]+)")
    if (is.na(lab[1, 1])) NA_character_ else lab[1, 1]
  }
  
  # Parse quoted CSV label line like: "A","CAR","Notes",""
  parse_label_csv_line <- function(s) {
    m <- stringr::str_match(s, '^\\s*"[^"]*"\\s*,\\s*"([^"]+)"')
    if (is.na(m[1, 2])) NA_character_ else m[1, 2]
  }
  
  # Find numeric lines
  pairs    <- purrr::map(seq_along(lines), ~ num_pair(lines[.x]))
  pair_idx <- which(!purrr::map_lgl(pairs, is.null))
  if (length(pair_idx) < 6) {
    stop("Could not locate expected boundary + point blocks in '", path,
         "'. File may be corrupted or in an unexpected format.", call. = FALSE)
  }
  
  # Point count line (usually after bounds)
  count_line_idx <- pair_idx[5] + 1L
  if (count_line_idx > length(lines)) {
    stop("Unexpected end of file while reading point count in '", path, "'.", call. = FALSE)
  }
  count_line <- lines[count_line_idx]
  pts_n <- suppressWarnings(as.integer(stringr::str_trim(count_line)))
  if (is.na(pts_n) || pts_n <= 0) {
    int_only_idx <- which(stringr::str_detect(lines, "^\\s*\\d+\\s*$"))
    if (length(int_only_idx)) pts_n <- suppressWarnings(as.integer(lines[int_only_idx[1]]))
  }
  if (is.na(pts_n) || pts_n <= 0) {
    stop("Could not determine number of points in '", path, "'.", call. = FALSE)
  }
  
  # Coordinate pairs after the count
  candidate_idx <- seq.int(count_line_idx + 1L, length(lines))
  cand_pairs    <- purrr::compact(purrr::map(lines[candidate_idx], num_pair))
  if (length(cand_pairs) < pts_n) {
    stop("Found fewer coordinate pairs (", length(cand_pairs),
         ") than expected (", pts_n, ") in '", path, "'.", call. = FALSE)
  }
  coord_pairs <- cand_pairs[seq_len(pts_n)]
  coord_lines <- lines[candidate_idx][seq_len(pts_n)]
  
  # Try labels on the same lines
  label_same_line <- vapply(coord_lines, trailing_label, FUN.VALUE = character(1))
  have_all <- !any(is.na(label_same_line))
  
  # Fallback: look for a block of `pts_n` labels right after the coord block
  label_block <- rep(NA_character_, pts_n)
  if (!have_all) {
    after_idx <- candidate_idx[pts_n] + 1L
    rem <- if (after_idx <= length(lines)) lines[after_idx:length(lines)] else character(0)
    
    # First attempt: quoted CSV lines
    csv_labels <- purrr::map_chr(rem, parse_label_csv_line)
    csv_labels <- csv_labels[!is.na(csv_labels)]
    if (length(csv_labels) >= pts_n) {
      label_block <- csv_labels[seq_len(pts_n)]
    } else {
      # Second attempt: plain short-code lines (no numbers and a single token)
      is_code_line <- function(s) {
        s_trim <- stringr::str_trim(s)
        if (s_trim == "") return(FALSE)
        !stringr::str_detect(s_trim, "(-?\\d+(?:\\.\\d+)?)[,\\s]+(-?\\d+(?:\\.\\d+)?)") &&
          stringr::str_detect(s_trim, "^[A-Za-z0-9_\\-./]+$")
      }
      codes <- rem[purrr::map_lgl(rem, is_code_line)]
      if (length(codes) >= pts_n) {
        label_block <- stringr::str_trim(codes[seq_len(pts_n)])
      }
    }
  }
  
  # Final labels
  label_code <- if (have_all) label_same_line else label_block
  if (all(is.na(label_code))) {
    message("No per-point labels found in '", base_file, "'. `label_code` is NA.")
  }
  
  # Get numeric vectors 
  x_cpc <- vapply(coord_pairs, function(v) v[1], numeric(1))
  y_cpc <- vapply(coord_pairs, function(v) v[2], numeric(1))
  
  
  # If scale_factor is NA, default to 15; else enforce 12 or 15
  if (is.na(scale_factor)) {
    scale_factor <- 15
  }
  if (!(scale_factor %in% c(12, 15))) {
    stop("`scale_factor` must be exactly 12 or 15.", call. = FALSE)
  }
  
  # CPC -> pixel converter (1‑based)
  to_px <- function(v) {
    px <- v / scale_factor
    if (round_to_pixel) px <- round(px)
    px <- px + 1L
    px
  }
  x_px <- to_px(x_cpc)
  y_px <- to_px(y_cpc)
  
  tibble::tibble(
    file       = base_file,
    image_name = image_name,          # <- base name without .cpc
    point_id   = seq_len(pts_n),
    x_cpc      = x_cpc,
    y_cpc      = y_cpc,
    x_px       = x_px,
    y_px       = y_px,
    label_code = label_code
  )
}
