# =============================================================
# File: validate_bbox.R
# Description: Validates bounding box input for ReefCloud API calls.
# Author: Samuel Chan
# Date: 2025-11-20
# Dependencies: None
# =============================================================

#' Validate bounding box input
#'
#' Ensures that the bounding box contains valid numeric coordinates and correct names.
#'
#' @param bbox A bounding box object (`sf::bbox`), named vector, or list.
#'
#' @return TRUE if valid, otherwise throws an error.
#' 
#' @examples
#' # Example 1: Valid bbox
#' valid_bbox <- c(xmin = 142.0, ymin = -10.0, xmax = 145.0, ymax = -8.0)
#' validate_bbox(valid_bbox)  # Returns TRUE
#'
#' # Example 2: Invalid bbox
#' invalid_bbox <- c(xmin = 145.0, ymin = -10.0, xmax = 142.0, ymax = -8.0)
#' \dontrun{
#' validate_bbox(invalid_bbox)  # Throws an error
#' }
#' 
#' @export

validate_bbox <- function(bbox) {
  # Convert sf bbox to list if needed
  if (inherits(bbox, "bbox")) {
    bbox <- as.list(bbox)
  } else if (is.vector(bbox) || is.list(bbox)) {
    bbox <- as.list(bbox)
  } else {
    stop("`bbox` must be an sf bbox object, named vector, or list.")
  }
  
  # Check names
  required_names <- c("xmin", "ymin", "xmax", "ymax")
  if (!all(required_names %in% names(bbox))) {
    stop("`bbox` must have names: xmin, ymin, xmax, ymax.")
  }
  
  # Check numeric values
  if (!all(sapply(bbox[required_names], is.numeric))) {
    stop("All bbox values must be numeric.")
  }
  
  # Check logical bounds
  if (bbox$xmin >= bbox$xmax || bbox$ymin >= bbox$ymax) {
    stop("Invalid bbox: xmin must be < xmax and ymin must be < ymax.")
  }
  
  return(TRUE)
}
