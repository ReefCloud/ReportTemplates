# =============================================================
# File: export_cpce_csv.R
# Description: Exports data from CPCe (.cpc) files into csv
# Author: Samuel Chan
# Date: 2026-01-23
# Dependencies: readr, stringr, purrr, tibble, utils
# =============================================================

#' Export CPCe points CSVs for a list of folders
#'
#' @description
#' For each directory in `dirs`, this function reads all `.cpc` files that are
#' **directly inside that folder** (non-recursive), parses them with [read_cpce()],
#' and writes a combined CSV into the same folder. The CSV filename is auto-
#' generated from the **last four folder levels** (sanitized) to make it unique
#' and traceable.
#'
#' @param dirs Either:
#'   * a **character vector** of directory paths (e.g., from `list.dirs()`), or
#'   * a **tibble/data.frame** with a column named **`path`** (e.g., from
#'     `list_directory_tibble()`).
#' @param scale_factor Numeric scalar. CPC units per pixel; must be **15** (default) or **12** (legacy).
#' @param round_to_pixel Logical; round to nearest pixel (default `TRUE`).
#' @param fail_on_error Logical; if `TRUE`, any error in a folder stops the whole run.
#'   If `FALSE`, errors are recorded in the summary and processing continues. Default `TRUE`.
#' @param overwrite Logical; if `TRUE`, overwrite existing CSVs. Default `TRUE`.
#' @param progress Logical; if `TRUE`, show a progress bar (default `TRUE`).
#'
#' @return A tibble with one row per input folder containing:
#' \itemize{
#'   \item `.dir` — folder path processed
#'   \item `n_cpc_files` — number of `.cpc` files found in that folder
#'   \item `n_points` — total number of point rows parsed (sum across files)
#'   \item `out_csv` — CSV file path written (or `NA` on failure / no files)
#'   \item `status` — "ok", "no_cpc_files", or an error message
#' }
#'
#' @details
#' - **Non-recursive** per folder: only `.cpc` files directly within each `dir` are read.
#' - CSV name uses the **last 4 folder levels** joined by `__`, sanitized for file safety.
#' - Uses your 1-based pixel convention and enforces `scale_factor` ∈ {12, 15}.
#'
#' @examples
#' \dontrun{
#' # 1) With a character vector of directories:
#' all_dirs <- list.dirs("C:/Users/HP/Desktop/CRM 2018_to do sites", recursive = TRUE, full.names = TRUE)
#' summary1 <- export_cpce_csv(all_dirs, scale_factor = 15)
#'
#' # 2) With a tibble from list_directory_tibble():
#' leaf_tbl <- list_directory_tibble("C:/Users/HP/Desktop/CRM 2018_to do sites")
#' summary2 <- export_cpce_csv(leaf_tbl, scale_factor = 15)
#' }
#' @seealso [read_cpce()]
#' @export
export_cpce_csv <- function(dirs,
                            scale_factor = 15,
                            round_to_pixel = TRUE,
                            fail_on_error = TRUE,
                            overwrite = TRUE,
                            progress = TRUE) {
  # ---- normalize dirs input ----
  if (is.data.frame(dirs)) {
    if (!("path" %in% names(dirs))) {
      stop("When `dirs` is a tibble/data.frame, it must contain a column named `path`.", call. = FALSE)
    }
    dirs <- dirs$path
  }
  
  # ---- validation ----
  if (!is.character(dirs) || !length(dirs)) {
    stop("`dirs` must be a non-empty character vector of directory paths (or a tibble with `path`).", call. = FALSE)
  }
  if (!is.numeric(scale_factor) || length(scale_factor) != 1L || !(scale_factor %in% c(12, 15))) {
    stop("`scale_factor` must be 12 (legacy) or 15 (default).", call. = FALSE)
  }
  if (!is.logical(round_to_pixel) || length(round_to_pixel) != 1L) {
    stop("`round_to_pixel` must be a single logical.", call. = FALSE)
  }
  if (!is.logical(fail_on_error) || length(fail_on_error) != 1L) {
    stop("`fail_on_error` must be a single logical.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("`overwrite` must be a single logical.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L) {
    stop("`progress` must be a single logical.", call. = FALSE)
  } 
  
  # ---- Helper: build output CSV path from last 4 folder levels ----
  make_out_csv <- function(dir) {
    norm  <- gsub("\\\\", "/", dir)
    parts <- strsplit(norm, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    last4 <- utils::tail(parts, n = min(4L, length(parts)))
    sanitize <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)
    suffix <- paste(vapply(last4, sanitize, character(1)), collapse = "__")
    if (nchar(suffix) > 180) suffix <- paste0(substr(suffix, 1, 180), "_trim")
    file.path(dir, paste0("cpce_points_", suffix, ".csv"))
  }
  
  # ---- Process a single folder ----
  handle_one <- function(dir) {
    if (!dir.exists(dir)) {
      return(tibble::tibble(
        .dir = dir, n_cpc_files = NA_integer_, n_points = NA_integer_,
        out_csv = NA_character_, status = "dir_not_found"
      ))
    }
    
    files <- list.files(
      path = dir,
      pattern = "\\.cpc$",
      recursive = FALSE,    # non-recursive per your requirement
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (!length(files)) {
      return(tibble::tibble(
        .dir = dir, n_cpc_files = 0L, n_points = 0L,
        out_csv = NA_character_, status = "no_cpc_files"
      ))
    }
    
    out_csv <- make_out_csv(dir)
    if (file.exists(out_csv) && !overwrite) {
      return(tibble::tibble(
        .dir = dir, n_cpc_files = length(files), n_points = NA_integer_,
        out_csv = out_csv, status = "exists_skip_overwrite_false"
      ))
    }
    
    # Parse all .cpc files in this folder
    parse_one <- function(fp) {
      read_cpce(fp, scale_factor = scale_factor, round_to_pixel = round_to_pixel) |>
        dplyr::mutate(.path = fp)
    }
    
    # Fail-fast vs continue-on-error
    if (fail_on_error) {
      dfl <- purrr::map(files, parse_one)
    } else {
      dfl <- purrr::map(
        files,
        ~ tryCatch(parse_one(.x),
                   error = function(e) {
                     warning("Folder '", dir, "': skipped '", .x, "' due to error: ", conditionMessage(e))
                     NULL
                   })
      )
      dfl <- purrr::compact(dfl)
      if (!length(dfl)) {
        return(tibble::tibble(
          .dir = dir, n_cpc_files = length(files), n_points = 0L,
          out_csv = out_csv, status = "all_files_failed"
        ))
      }
    }
    
    combined <- dplyr::bind_rows(dfl)
    
    # Write CSV
    readr::write_csv(combined, out_csv)
    
    tibble::tibble(
      .dir = dir,
      n_cpc_files = length(files),
      n_points = nrow(combined),
      out_csv = out_csv,
      status = "ok"
    )
  }
  
  # ---- Iterate with optional progress bar ----
  n <- length(dirs)
  results <- vector("list", n)
  
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  for (i in seq_len(n)) {
    dir_i <- dirs[i]
    if (fail_on_error) {
      results[[i]] <- handle_one(dir_i)
    } else {
      results[[i]] <- tryCatch(
        handle_one(dir_i),
        error = function(e) {
          tibble::tibble(
            .dir = dir_i, n_cpc_files = NA_integer_, n_points = NA_integer_,
            out_csv = NA_character_, status = paste0("error: ", conditionMessage(e))
          )
        }
      )
    }
    if (progress) utils::setTxtProgressBar(pb, i)   # CHANGED
  }
  
  dplyr::bind_rows(results)                         # CHANGED (bind results list)
}