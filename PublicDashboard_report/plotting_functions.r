# Description: This script generates waffle plots from ReefCloud Dashboard exports
# Author: Manuel Gonzalez-Rivero
# Date: 2023-10-10

# Load required libraries
library(waffle)
library(tidyverse)
library(hrbrthemes)
library(grid)
library(RCurl)
library(rsvg)
library(scales)
library(ggmap)

# Helper function to load logo
load_logo <- function() {
  logo_url <- "https://reefcloud.ai/dashboard/logo.fdd3d79b2ab59591.svg"
  logo <- rsvg::rsvg(logo_url)
  grid::rasterGrob(logo, interpolate=TRUE)
}

# Function to generate waffle plots
generate_waffle_plots <- function(site_file, cred) {
  xdf <- read_csv(site_file) %>%
    mutate(Proportion = Site_No / sum(Site_No),
           Proportion = round(Proportion * 100, 0))
  name <- str_split(site_file, "_")[[1]][3] %>% str_remove(".csv")
  
  logo_grob <- load_logo()
  
  p <- xdf %>%
    ggplot(aes(fill=Class, values=Site_No)) +
    geom_waffle(color = "white", size=1.125, n_rows = 10, make_proportional = TRUE) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_manual(values =c(A='#00734D', B='#F0C918', C='#F47721', D='#ED1C24'), name=NULL) +
    coord_equal() +
    labs(
      title = paste("Coral Reef Habitat Condition", name, "Region"),
      subtitle = "Distribution of sites by coral cover categories",
      caption = sprintf("Source: ReefCloud.ai, Data Credits: %s", cred)
    ) +
    theme_ipsum(grid="") +
    theme_enhance_waffle() +
    theme(
      legend.position="bottom",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(size=14, face="bold"),
      plot.subtitle = element_text(size=12),
      plot.caption = element_text(size=10)
    ) +
    annotate("text", 
             x=Inf, 
             y=-Inf, 
             label="Source: ReefCloud.ai", 
             hjust=1.1, 
             vjust=-1.1, 
             size=3, 
             color="black")
  
  ggsave(p, 
         filename = paste0('figures/', "SiteCondition_", name, '.png'),
         bg = "transparent", dpi = 600)
}

# Function to generate temporal cover plot
Temporal_cover_plot <- function(cover_file, cred) {
  xdf <- read_csv(cover_file)
  name <- str_split(cover_file, "_")[[1]][2] %>% str_remove(".csv")
  
  logo_grob <- load_logo()
  
  plot <- ggplot(xdf) +
    geom_point(aes(x=year, y=median), size=2, color="black") +
    geom_line(aes(x=year, y=median), size=1.5) +
    geom_ribbon(aes(x=year, ymin=lower, ymax=upper), alpha=0.3) +
    labs(
      title = "Temporal Cover Plot",
      subtitle = "Median Cover with 95% Credible Intervals",
      x = "Year",
      y = "Median Cover",
      caption = "Source: ReefCloud.ai"
    ) +
    scale_x_continuous(breaks = breaks_pretty()) +
    theme_ipsum(grid="") +
    theme(
      legend.position="bottom",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(size=14, face="bold"),
      plot.subtitle = element_text(size=12),
      plot.caption = element_text(size=10)
    ) +
    annotate("text", 
             x=Inf, 
             y=-Inf, 
             label=sprintf("Data Credits: %s", cred), 
             hjust=1.1, 
             vjust=-1.1, 
             size=3, 
             color="black")
  
  ggsave(plot, 
         filename = sprintf("figures/temporal_cover_%s.png", name),
         bg = "transparent", dpi = 600)
}

# Function to generate map
generate_map <- function(data_file = NULL, api = TRUE, sites = NULL, tier_id = NULL) {
  require(sf)
  require(ggplot2)
  require(naturalearthr)
  require(cowplot)
  
  if (isFALSE(api) && !is.null(data_file)) {
    sites <- read_csv(data_file)
    sites <- st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326)
  }
  
  if (is.null(sites) && is.null(tier_id)) {
    stop("Sites data must be provided either through 'sites' parameter or 'data_file' parameter.")
  }
  
  if (is.null(sites) && !is.null(tier_id)) {
    info <- getRegionalSummary(tier_id)  # regional summary based on a Spatial Tier ID. 
    sites <- get_sites_info(info) # Extract sites from ReefCloud API
  }
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  country <- unique(sites$site_country)
  country_shape <- world[world$name == country, ]
  
  main_map <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sites, aes(geometry = geometry), color = "red", size = 3) +
    labs(
      title = "Map of Locations",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  inset_map <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = country_shape, fill = "lightblue") +
    coord_sf(xlim = st_bbox(country_shape)[c("xmin", "xmax")], 
             ylim = st_bbox(country_shape)[c("ymin", "ymax")]) +
    theme_void()
  
  combined_map <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_map, x = 0.7, y = 0.7, width = 0.25, height = 0.25)
  
  ggsave(combined_map, file = file.path("figures", sprintf("SurveySite_Map_%s_TierID_%s.png", country, tier_id)))
}

# Function to generate donut chart
generate_donut_chart <- function(TS_file, y=2024) {
  xdf <- read_csv(TS_file) %>%
    filter(year == y) %>%
    mutate("No Stress (<4 DHW)" = 100 - sum(severe, moderate)) %>%
    rename("Severe (>8 DHW)" = severe, "Moderate (4-8 DHW)" = moderate) %>%
    pivot_longer(
      cols = !year, 
      names_to = "Stress_cat", 
      values_to = "Prop")
  
  name <- str_split(TS_file, "_")[[1]][2] %>% str_remove(".csv")
  
  p <- ggplot(xdf, aes(x = 2, y = Prop, fill = factor(Stress_cat))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    theme_void() +
    scale_fill_manual(values = c("No Stress (<4 DHW)" = '#03d843', "Moderate (4-8 DHW)"= '#F0C918', "Severe (>8 DHW)" = '#ED1C24'), name = NULL) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 32, face = "bold"),
      legend.text = element_text(size = 24)
    ) +
    labs(
      title = "Proportion of Sites Affected by Thermal Stress",
      fill = "Thermal Stress",
      caption = sprintf("Source: ReefCloud.ai, Data Credits: %s", cred)
    )
  
  ggsave(p, 
         filename = sprintf("figures/ThermalStress_%s.png", name),
         bg = "transparent", dpi = 600)
}

