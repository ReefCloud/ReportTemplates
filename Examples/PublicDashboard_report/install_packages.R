# List of required packages
required_packages <- c(
  "waffle",
  "tidyverse",
  "hrbrthemes",
  "grid",
  "RCurl",
  "rsvg",
  "scales",
  "ggmap",
  "sf",
  "cowplot",
  "gt",
  "devtools"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  installed_packages <- rownames(installed.packages())
  for (pkg in packages) {
    if (!(pkg %in% installed_packages)) {
      install.packages(pkg)
    }
  }
}

devtools::install_github("ropensci/rnaturalearth")
devtools::install_github("ropensci/rnaturalearthdata")
devtools::install_github("ropensci/rnaturalearthhires")

# Install required packages
install_if_missing(required_packages)

# Load required packages
lapply(required_packages, library, character.only = TRUE)