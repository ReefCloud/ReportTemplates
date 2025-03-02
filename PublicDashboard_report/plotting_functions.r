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
source("ReefCloud_API.r")

# Helper function to load logo
load_logo <- function() {
  logo_url <- "https://reefcloud.ai/dashboard/logo.fdd3d79b2ab59591.svg"
  logo <- rsvg::rsvg(logo_url)
  grid::rasterGrob(logo, interpolate=TRUE)
}

# Function to generate waffle plots
generate_waffle_plots <- function(tier_id, info, cover_type) {

  xdf<- get_site_cover_cat(tier_id=tier_id, cover_type=cover_type) %>%
        mutate(Proportion = Site_No / sum(Site_No),
           Proportion = round(Proportion * 100, 0))
  # logo_grob <- load_logo()
  possible_classes<- c("A", "B", "C", "D")
  p <- xdf %>% 
    ggplot(aes(fill=Class, values=Site_No)) +
    geom_waffle(color = "white", size=1.125, n_rows = 10, make_proportional = TRUE) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_manual(values =c(A='#00734D', B='#F0C918', C='#F47721', D='#ED1C24'), name=NULL)+
    coord_equal() +
    labs(
      title = paste("Coral Reef Habitat Condition", info$region_name, "Region"),
      subtitle = sprintf("Distribution of sites by %s categories", str_to_title(cover_type)),
      caption = str_wrap(sprintf("Data Credits: %s", paste(info$data_contributors, collapse=". ")), 70)
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
         filename = paste0('figures/', "SiteCondition_",cover_type, '.png'),
         bg = "transparent", width = 8, height = 8)

  return(p)
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
  require(rnaturalearth)
  require(cowplot)
  require(ggspatial) # Add ggspatial for scale bar
  require(basemaps)
  
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
  
  bbox <- st_bbox(st_buffer(sites, 10))
  boundary <- getTiers(tier_level = 4, bbox = bbox)
  boundary <- boundary %>% st_cast("POLYGON") %>% mutate(Area = st_area(.)) %>% filter(Area == max(Area))
  world <- ne_countries(scale = "large", returnclass = "sf")
  country <- unique(sites$site_country)
  country_shape <- world[world$name == country, ]
  
  ##Configure map
  # set defaults for the basemap
  set_defaults(map_service = "carto", map_type = "light")

  #transform CRS to Web Mercators (EPSG 3857)
  boundary <- boundary %>% st_transform(crs = 3857)
  sites <- sites %>% st_transform(crs = 3857)

  main_map <- ggplot() +
    basemap_gglayer(boundary) +
    scale_fill_identity() + 
    geom_sf(data = boundary, aes(geometry = geometry), color = "black", fill = NA, size = 3) +
    geom_sf(data = sites, aes(geometry = geometry), color = "red", size = 3) +
    labs(
      title = "Map of Locations",
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(xlim = st_bbox(boundary)[c("xmin", "xmax")], 
             ylim = st_bbox(boundary)[c("ymin", "ymax")]) +
    annotation_scale(location = "bl", width_hint = 0.5) + # Add scale bar
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  inset_map <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = country_shape, fill = "lightblue") +
    geom_sf(data = sites, fill = NA, color = "red") +
    coord_sf(xlim = st_bbox(country_shape)[c("xmin", "xmax")], 
             ylim = st_bbox(country_shape)[c("ymin", "ymax")]) +
    theme_void() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1) # Add border around inset map
    )
  
  combined_map <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_map, x = 0.68, y = 0.1, width = 0.25, height = 0.25) # Position inset map in bottom right corner
  
  ggsave("figures/Site_Map.png", combined_map, width = 5, height = 4)
}

# Function to generate donut chart
generate_donut_chart <- function(tier_id, y, info) {
  require(cowplot)
  require(tidyverse)
  require(ggplot2)
  require(gridGraphics)

  ### Generate Thermal Stress Plot
  dhw.df <- get_disturbance_ext(tier_id, e_type="thermal_stress") %>% 
    mutate(Stress_Cat=case_when(
      severity==1 ~ "Moderate",
      severity==2 ~ "Severe",
      .default= "No Major Stress"
    )
    ) %>%
    select(tier_id, Year, severity, Stress_Cat, percentage_total_reef) %>%
    filter(Year==2024) %>%
    reframe(add_row(cur_data(), severity=0, Stress_Cat="No Major Stress", percentage_total_reef= 1-sum(cur_data()$percentage_total_reef)))
    
  dhw.df <- dhw.df %>%
    arrange(factor(Stress_Cat, levels=c("No Major Stress","Moderate","Severe"))) %>%
    mutate(lab.ypos=cumsum(percentage_total_reef))

  dhw.p <- ggplot(dhw.df, aes(x = 2, y = percentage_total_reef, 
                              fill = factor(Stress_Cat, levels=c("No Major Stress","Moderate","Severe")))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label=paste0(percentage_total_reef*100, " %")), position = position_stack(vjust = 0.5), size=10, 
              color="white") +
    xlim(0.5, 2.5) +
    theme_void() +
    scale_fill_manual(values = c("No Major Stress" = '#C0C2C0', "Moderate"= '#F13030', "Severe" = '#5A0001'), name = NULL) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle=element_text(size = 14),
      legend.text = element_text(size = 20)
    ) +
    labs(
      title = "Thermal Stress",
      subtitle = sprintf("Region: %s\nYear: %s", str_to_title(info$region_name), y),
      fill = "Thermal Stress",
      caption = str_wrap("Source: ReefCloud.ai. Data Credits: Australian Institute of Marine Science, NOAA Coral Reef Watch, Allen Coral Atlas", 70)
    )

  ### Generate Cyclone Plot
  tc.df <- get_disturbance_ext(tier_id, e_type="storm_exposure_year") %>% 
    mutate(Stress_Cat=case_when(
      severity==1 ~ "Moderate",
      severity==2 ~ "Severe",
      .default= "No Major Stress"
    )
    ) %>%
    select(tier_id, Year, severity, Stress_Cat, percentage_total_reef) %>%
    filter(Year==2024) %>%
    reframe(add_row(cur_data(), severity=0, Stress_Cat="No Major Stress", percentage_total_reef= 1-sum(cur_data()$percentage_total_reef)))
  
  tc.df <- tc.df %>%
    arrange(factor(Stress_Cat, levels=c("No Major Stress","Moderate","Severe"))) %>%
    mutate(lab.ypos=cumsum(percentage_total_reef))

  tc.p <- ggplot(tc.df, aes(x = 2, y = percentage_total_reef, fill = factor(Stress_Cat))) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label=paste0(percentage_total_reef*100, " %")), position = position_stack(vjust = 0.5), size=10, 
          color="white") +
    xlim(0.5, 2.5) +
    theme_void() +
    scale_fill_manual(values = c("No Major Stress" = '#C0C2C0', "Moderate"= '#5EEB5B', "Severe" = '#62AB37'), name = NULL) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle=element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 20)
    ) +
    labs(
      title = "Tropical Cyclones",
      fill = "Tropical Cyclones",
      caption = str_wrap("Source: ReefCloud.ai. Data Credits: Australian Institute of Marine Science, Australian Bureau of Meteorology, International Best Track Archive for Climate Stewardship, Allen Coral Atlas", 70)
    )

  ### Extract the legend from one of the plots
get_legend_35 <- function(plot) {
  # return all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}

legend <- get_legend_35(
  dhw.p + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
  ### Remove legends from individual plots
  dhw.p <- dhw.p + theme(legend.position = "none")
  tc.p <- tc.p + theme(legend.position = "none")

  ### Combine the plots and the legend
  combined_plot <- plot_grid(
    plot_grid(dhw.p, tc.p, labels = c("A)", "B)"), ncol = 2),
    legend,
    nrow=2,
    #ncol = 1,
    rel_widths = c(0.1, 1),
    rel_heights = c(7,1)
  )

  ggsave("figures/Disturbance_impact.png", combined_plot, bg = "transparent", width = 16, height = 8)
  
  dist.df<-dhw.df %>%
  mutate(Disturbance="Thermal Stress") %>%
  bind_rows(tc.df %>%
    mutate(Disturbance="Tropical Cyclones") )
  return(dist.df)
}

theme_Publication <- function(base_size=14, base_family="sans") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(0.8), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}