# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stars)
library(forcats)


# Declare constants -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"
boundaries_path <- "../data/boundaries/"
fmmp_path <- "../data/land_use/farmland_mapping_and_monitoring/"
images_path <- "../images/"
#world <- ne_countries(returnclass = "sf")
#states <- ne_states(returnclass = "sf")

california_sf <- st_read(paste0(boundaries_path, "ca-state-boundary/CA_State_TIGER2016.shp")) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

ca_counties_sf <- st_read(paste0(boundaries_path, "ca-county-boundaries/CA_Counties/CA_Counties_TIGER2016.shp")) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

sd_bbox <- ca_counties_sf %>% filter(NAME == "San Diego") %>% st_bbox()

ca_bbox <- c(-124.409591, -114.131211,
             32.534156, 42.009518)

# Load station data -------------------------------------------------------
ghcnd_stations_ca_pre_1984_df <- read_csv("~/Documents/research-projects/trivariate-temperature/data/ghcnd/ghcnd-stations-pre-1984.csv")


# Filter stations by number of missing records ----------------------------
ghcnd_stations_ca_pre_1984_df <- ghcnd_stations_ca_pre_1984_df %>%
  filter(LATITUDE > 0) %>% # remove obviously incorrect station
  rename_with(.fn = tolower) %>% # make names lower case
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(warm_half_year = case_when(month %in% c(5,6,7,8,9,10) ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(missing_min_temp = case_when(is.na(tmin) ~ 1,
                                      TRUE ~ 0),
         missing_max_temp = case_when(is.na(tmax) ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(missing_min_temp = missing_min_temp * warm_half_year,
         missing_max_temp = missing_max_temp * warm_half_year)

ghcnd_stations_ca_pre_1984_missing_data_summary <- ghcnd_stations_ca_pre_1984_df %>%
  filter(year > 1983) %>%
  group_by(station) %>%
  summarize(name = min(name),
            proportion_of_missing_tmin = sum(missing_min_temp)/sum(warm_half_year),
            proportion_of_missing_tmax = sum(missing_max_temp)/sum(warm_half_year),
            lat = min(latitude),
            lon = min(longitude)) %>%
  ungroup() %>%
  filter(proportion_of_missing_tmin <= 0.15) # get rid of stations with more than 15% of records missing

ghcnd_stations_ca_pre_1984_sf <- st_as_sf(x = ghcnd_stations_ca_pre_1984_missing_data_summary,
                                          coords = c("lon", "lat"),
                                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")



# Loops to generate plots -------------------------------------------------
# ORANGE
for(cur_year in seq(from = 1984, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "orange",
                                 county = "Orange",
                                 resx = 2000,
                                 resy = 2000)
}

# SAN DIEGO
for(cur_year in seq(from = 2002, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sandiego",
                                 county = "San Diego",
                                 resx = 2000,
                                 resy = 1300)
}

# LOS ANGELES
for(cur_year in seq(from = 1984, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "losangeles",
                                 county = "Los Angeles",
                                 resx = 2000,
                                 resy = 1300,
                                 override_y_min = 33.6)
}

# RIVERSIDE
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "riverside",
                                 county = "Riverside",
                                 resx = 2400,
                                 resy = 800)
}

# VENTURA
for(cur_year in seq(from = 1984, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "ventura",
                                 county = "Ventura",
                                 resx = 2000,
                                 resy = 1800,
                                 override_y_min = 34)
}

# MONTERREY
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "monterey",
                                 county = "Monterey",
                                 resx = 2000,
                                 resy = 1400)
}

# SAN MATEO
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sanmateo",
                                 county = "San Mateo",
                                 resx = 2000,
                                 resy = 1400,
                                 override_y_min = 37.1)
}

# CONTRA COSTA
for(cur_year in seq(from = 1984, to = 1998, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "contracosta",
                                 county = "Contra Costa",
                                 resx = 2000,
                                 resy = 1000)
}

# MARIN
for(cur_year in seq(from = 1984, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "marin",
                                 county = "Marin",
                                 resx = 2000,
                                 resy = 1200)
}

# SANTA CLARA
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "santaclara",
                                 county = "Santa Clara",
                                 resx = 2000,
                                 resy = 1400)
}

# IMPERIAL
for(cur_year in seq(from = 1984, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "imperial",
                                 county = "Imperial",
                                 resx = 2000,
                                 resy = 1100)
}

# ALAMEDA
for(cur_year in seq(from = 2002, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "alameda",
                                 county = "Alameda",
                                 resx = 2000,
                                 resy = 1100)
}

# PLACER
for(cur_year in seq(from = 1984, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "placer",
                                 county = "Placer",
                                 resx = 2000,
                                 resy = 1400, override_x_max = -120.6)
}

# KERN
for(cur_year in seq(from = 1988, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "kern",
                                 county = "Kern",
                                 resx = 2000,
                                 resy = 1000)
}

# SAN BERNARDINO
for(cur_year in seq(from = 1984, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sanbernardino",
                                 county = "San Bernardino",
                                 resx = 1600,
                                 resy = 2000,
                                 override_x_max = -116.5)
}

# SAN JOAQUIN
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sanjoaquin",
                                 county = "San Joaquin",
                                 resx = 1800,
                                 resy = 2000)
}

# SACRAMENTO
for(cur_year in seq(from = 1988, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sacramento",
                                 county = "Sacramento",
                                 resx = 2000,
                                 resy = 1600)
}

# MERCED
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "merced",
                                 county = "Merced",
                                 resx = 2000,
                                 resy = 1600)
}

# SOLANO
for(cur_year in seq(from = 2000, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "solano",
                                 county = "Solano",
                                 resx = 2000,
                                 resy = 1400)
}

# SONOMA
for(cur_year in seq(from = 1984, to = 2000, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "sonoma",
                                 county = "Sonoma",
                                 resx = 2000,
                                 resy = 1300)
}

# NAPA
for(cur_year in seq(from = 2002, to = 2016, by = 2)) {
  land_use_station_map_generator(year = cur_year,
                                 county_file_name = "napa",
                                 county = "Napa",
                                 resx = 2000,
                                 resy = 2000)
}

# Image factory function --------------------------------------------------
land_use_station_map_generator <- function(year = 1984,
                                           county_file_name = "sandiego",
                                           county = "San Diego",
                                           resx = NULL,
                                           resy = NULL,
                                           override_x_max = NULL,
                                           override_y_min = NULL) {
  land_use_sf <- st_read(paste0(fmmp_path, paste0(year,
                                                  "_FMMP_shape_files/",
                                                  county_file_name, year, ".shp"))) %>%
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  land_use_r <- land_use_sf["polygon_ty"] %>%
    mutate_if(is.character, as.factor) %>%
    transmute(Use = fct_collapse(polygon_ty,
                                 Farmland = c("P","S", "U", "L"),
                                 Pasture = c("G"),
                                 Urban = c("D"),
                                 Other = c("X"),
                                 Water = c("W"),
                                 Unmapped = c("Z"))) %>%
    st_rasterize()
  
  county_bbox <- ca_counties_sf %>% filter(NAME == county) %>% st_bbox()
  
  if(!is.null(override_y_min)) {
    county_bbox[2] = override_y_min
  }
  if(!is.null(override_x_max)) {
    county_bbox[3] = override_x_max
  }
  
  #print("Made it here")
  
  stations_to_plot <-(st_filter(ghcnd_stations_ca_pre_1984_sf,
                                y = ca_counties_sf %>% 
                                  filter(NAME == county)) %>%
                        as.data.frame())$station
  
  #print("Made it there")
  #print(stations_to_plot)
  
  plot_to_display <-  ggplot() +
    # geom_sf(data = ca_counties_sf %>%
    #           filter(NAME %in% c("San Diego")),
    #         inherit.aes = FALSE,
    #         fill = NA) +
    coord_sf(#xlim = ca_bbox[1:2],
      xlim = c(county_bbox[1], county_bbox[3]),
      #ylim = ca_bbox[3:4],
      ylim = c(county_bbox[2], county_bbox[4]),
      expand = TRUE) +
    geom_stars(data = land_use_r,
               inherit.aes = FALSE) +
    scale_fill_manual(values = c(Farmland = "#567462",
                                 Pasture = "#847361",
                                 #Urban = "#bbbdb2",
                                 Urban = "#5e5f61",
                                 Other = "#ded6b3",
                                 Water = "#16333b",
                                 Unmapped = "beige"),
                      na.value = NA) +
    #scale_fill_viridis_d(option = "C") +
    #scale_fill_discrete() +
    geom_point(data = ghcnd_stations_ca_pre_1984_missing_data_summary %>%
                 filter(station %in% stations_to_plot),
               aes(x = lon,
                   y = lat),
               color = "#FF5733"
               # color = proportion_of_missing_tmax)
    ) +
    geom_text(data = ghcnd_stations_ca_pre_1984_missing_data_summary %>%
                filter(station %in% stations_to_plot),
              aes(x = lon,
                  y = lat,
                  label = station),
              color = "#FF5733",
              angle = 30,
              size  = 2,
              nudge_y = 0.03) +
    # scale_color_viridis_b(breaks = seq(from = 0.15,
    #                                    to = 1,
    #                                    by = 0.15)) +
    labs(title = "Land Use and Weather Station Locations,",
         subtitle = paste0(county," c.", year)) +
    theme_bw()
  
  if(is.null(resx)) {
    plot_to_display
  }
  else {
    ggsave(plot = plot_to_display,
           file = paste0(images_path,
                         "ghcnd/station_inventory/",
                         county_file_name,
                         "/",
                         county_file_name,"-",year,".png"),
           height = resy,
           width = resx,
           units = "px")
  }
}

