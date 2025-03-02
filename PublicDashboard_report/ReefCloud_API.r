library(sf)
library(httr)
library(jsonlite)
library(gt)

# Miscellaneous Functions to use ReefCloud API and fetch data from the public dashboard
#API DOCUMENTATION: https://api.reefcloud.ai/reefcloud/dashboard-api/docs#/

##Get regional summary ######
#' Get Regional Summary
#'
#' This function retrieves a summary of regional data from the ReefCloud API based on the provided tier ID.
#'
#' @param tierID A character string representing the tier ID for which the regional summary is to be retrieved.
#' @return A list containing the following elements:
#' \item{region_name}{The name of the region.}
#' \item{site_count}{The number of sites in the region.}
#' \item{photo_quadrats}{The number of photo quadrats in the region.}
#' \item{data_contributors}{The number of data contributors in the region.}
#' \item{site_id}{The site ID associated with the region.}
#' \item{source}{The source of the data, which is "www.reefcloud.ai".}
#' @examples
#' \dontrun{
#'   summary <- getRegionalSummary("exampleTierID")
#'   print(summary)
#' }
#' @export
getRegionalSummary <- function(tierID){
url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers/", tierID)
response <- GET(url)
data <- fromJSON(content(response, "text"))
info <- list(region_name=data$data$name, site_count=data$data$site_count, photo_quadrats=data$data$image_count, data_contributors=data$data$contributors, site_id=data$data$site_id, source="www.reefcloud.ai")
return(info)
}


## Get Tiers 
getTiers<-function(tier_level=4, bbox){

url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/tiers?tier_level=%s&xmin=%s&ymin=%s&xmax=%s&ymax=%s", 
tier_level,bbox$xmin, bbox$ymin,bbox$xmax, bbox$ymax)
boundary<-st_read(url)
return(boundary)
}

## Get Sites Information #######
#'
#' This function retrieves information about monitoring sites within a region (tier) from the ReefCloud API.
#'
#' @param info A list containing information about the tier from getRegionalSummary function.
#' @return A spatial dataframe containing the longitude and latitude of the sites.
#' @examples
#' \dontrun{
#'   sites_info <- get_sites_info(tier_info)
#'   print(sites_info)
#' }
#' @export
get_sites_info <- function(info) {
  sites <- data.frame(site_longitude = numeric(), site_latitude = numeric(), stringsAsFactors = FALSE)
  for (i in info$site_id){
    url <- paste0("https://api.reefcloud.ai/reefcloud/dashboard-api/sites/", i)
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    site <- as.data.frame(data$data)
    sites <- bind_rows(sites, site)
    }
  sites <- st_as_sf(sites, coords = c("site_longitude", "site_latitude"), crs = 4326)
  # Return a spatial dataframe containing the longitude and latitude of the sites
  sites
}


##Get reef condition
get_site_cover_cat <- function(tier_id, cover_type) {
  cover_type<-str_replace_all(cover_type, " ", "%20")
  url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/temporal-distribution/%s?benthic_type=%s", 
  tier_id, cover_type)
  response <- GET(url)
  data <- fromJSON(content(response, "text"))

  last_year_pos<-which(data$year==max(data$year))
  df<-data.frame(Class=c("D","D", "B", "C", "A"),  site_num=data$values[[last_year_pos]] %>% pull(magnitude)) %>%
  mutate(Year=data$year[[last_year_pos]])%>%
  group_by(Year, Class)%>%
  summarise(Site_No=sum(site_num))%>%
  mutate(Range= case_when(Class =="D" ~ "0 - 10 %",
  Class =="C" ~ "10 - 30 %",
  Class =="B" ~ "30 - 50 %",
  .default= ">50%"
   ))

  return(df)
}

### Environmental pressures
get_disturbance_ext <- function(tier_id, e_type) {
  #e_type {thermal_stress, storm_exposure_year}
  url <- sprintf("https://api.reefcloud.ai/reefcloud/dashboard-api/environmental/%s?env_type=%s", 
  tier_id, e_type)
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding="UTF-8"))
  data<-data$data %>%
    mutate(Year=year(start_date))
  return(data)
}

