# =============================================================
# File: list_directory_tibble.R
# Description: Lists lowest-level directories as a tibble
# Author: Samuel Chan
# Date: 2026-01-23
# Dependencies: fs, tibble, dplyr 
# =============================================================

#' List leaf (lowest-level) directories under a root as a tibble
#'
#' @param root Character of root directory.
#' @param include_root Logical. Include `root` if it has no subdirs. Default TRUE.
#' @return A tibble with columns: path, name, parent, depth
#' @examples
#' # leaf_tbl <- list_directory_tibble("C:/Users/Surveys/Reef Sites")
list_directory_tibble <- function(root, include_root = TRUE) {
  stopifnot(dir_exists(root))
  
  # All subdirectories (recursive). Include root to evaluate leaf-ness.
  subdirs <- dir_ls(root, type = "directory", recurse = TRUE, fail = FALSE)
  root_abs <- path_abs(root)
  all_dirs <- c(root_abs, subdirs) |> 
    unique() |> 
    path_norm()
  
  # A directory is a parent if any other directory has it as path_dir()
  parents <- unique(path_dir(all_dirs))
  
  # A leaf is a directory that is not a parent of any other directory
  leaves <- setdiff(all_dirs, parents)
  
  # Optionally include root if it has no subdirectories
  if (include_root && !(root_abs %in% parents)) {
    leaves <- unique(c(leaves, root_abs))
  }
  
  # Build tibble
  out <- tibble(
    path   = path_abs(leaves) |> path_norm(),
    parent = path_dir(leaves) |> path_norm(),
    name   = path_file(leaves),
    # depth: number of path components beyond root
    depth  = {
      # split both root and leaf into components to compute relative depth
      split_depth <- function(p) length(strsplit(path_norm(p), .Platform$file.sep, fixed = TRUE)[[1]])
      d_root  <- split_depth(root_abs)
      vapply(leaves, function(p) split_depth(p) - d_root, integer(1))
    }
  ) |>
    arrange(depth, parent, name)
  
  out
}
