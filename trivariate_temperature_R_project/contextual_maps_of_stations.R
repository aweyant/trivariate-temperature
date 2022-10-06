
# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stars)
library(forcats)


# Set default color palette -----------------------------------------------
# Define a collection of palettes to alter the default based on number of levels to encode
# discrete_palettes <- list(
#   c("skyblue", "orange"),
#   RColorBrewer::brewer.pal(3, "Set2"),
#   RColorBrewer::brewer.pal(6, "Accent")
# )
# withr::with_options(
#   list(ggplot2.discrete.fill = discrete_palettes), {
#     # 1st palette is used when there 1-2 levels (e.g., year)
#     print(cty_by_var(year))
#     # 2nd palette is used when there are 3 levels
#     print(cty_by_var(drv))
#     # 3rd palette is used when there are 4-6 levels
#     print(cty_by_var(fl))
#   })


# Declare constants -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"
boundaries_path <- "../data/boundaries/"
fmmp_path <- "../data/land_use/farmland_mapping_and_monitoring/"
images_path <- "../images/"
world <- ne_countries(returnclass = "sf")
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


# Load land coverage data -------------------------------------------------
sd_1984_sf <- st_read(paste0(fmmp_path, "1984_FMMP_shape_files/sandiego1984.shp")) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
sd_1984_r <- sd_1984_sf["polygon_ty"] %>%
  mutate_if(is.character, as.factor) %>%
  transmute(Use = fct_collapse(polygon_ty,
                            Farmland = c("P","S", "U", "L"),
                            Pasture = c("G"),
                            Urban = c("D"),
                            Other = c("X"),
                            Water = c("W"))) %>%
  st_rasterize()


sd_2016_sf <- st_read(paste0(fmmp_path, "2016_FMMP_shape_files/sandiego2016.shp")) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
sd_2016_r <- sd_2016_sf["polygon_ty"] %>%
  mutate_if(is.character, as.factor) %>%
  transmute(Use = fct_collapse(polygon_ty,
                               Farmland = c("P","S", "U", "L"),
                               Pasture = c("G"),
                               Urban = c("D"),
                               Other = c("X"),
                               Water = c("W"))) %>%
  st_rasterize()



# Filter stations by the number of missing records ------------------------
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
  ungroup() #%>%
  #filter(proportion_of_missing_records <= 0.15) # get rid of stations with more than 15% of records missing


# Map of our station inventory --------------------------------------------
ggplot(data = ghcnd_stations_ca_pre_1984_missing_data_summary,
       aes(x = lon,
           y = lat,
           color = proportion_of_missing_tmax)) +
  scale_color_viridis_b(breaks = seq(from = 0.15,
                                     to = 1,
                                     by = 0.15)) +
  geom_point() +
  geom_sf(data = ca_counties_sf %>%
            filter(
              # NAME %in% c("San Diego",
              #                  "Los Angeles",
              #                  "Riverside",
              #                  "San Bernardino",
              #                  "Orange",
              #                  "Imperial",
              #                  "Ventura",
              #                  "Santa Barbara")
              ),
          inherit.aes = FALSE,
          fill = NA) +
  coord_sf(#xlim = ca_bbox[1:2],
           xlim = c(-120, ca_bbox[2]),
           #ylim = ca_bbox[3:4],
           ylim = c(ca_bbox[3],36),
           expand = TRUE) +
  theme_bw()

# San Diego County 2016
ggplot() +
  # geom_sf(data = ca_counties_sf %>%
  #           filter(NAME %in% c("San Diego")),
  #         inherit.aes = FALSE,
  #         fill = NA) +
  coord_sf(#xlim = ca_bbox[1:2],
    xlim = c(sd_bbox[1], sd_bbox[3]),
    #ylim = ca_bbox[3:4],
    ylim = c(sd_bbox[2], sd_bbox[4]),
    expand = TRUE) +
  geom_stars(data = sd_2016_r,
             inherit.aes = FALSE) +
  scale_fill_manual(values = c(Farmland = "#567462",
                               Pasture = "#847361",
                               #Urban = "#bbbdb2",
                               Urban = "#5e5f61",
                               Other = "#ded6b3",
                               Water = "#16333b",
                               Z = "beige")) +
  #scale_fill_viridis_d(option = "C") +
  #scale_fill_discrete() +
  geom_point(data = ghcnd_stations_ca_pre_1984_missing_data_summary,
             aes(x = lon,
                 y = lat),
             color = "#39FF14"
                # color = proportion_of_missing_tmax)
             ) +
  # scale_color_viridis_b(breaks = seq(from = 0.15,
  #                                    to = 1,
  #                                    by = 0.15)) +
  labs(title = "Land Use and Weather Station Locations, c.2016") +
  theme_bw()

# San Diego County 1984
ggplot() +
  # geom_sf(data = ca_counties_sf %>%
  #           filter(NAME %in% c("San Diego")),
  #         inherit.aes = FALSE,
  #         fill = NA) +
  coord_sf(#xlim = ca_bbox[1:2],
    xlim = c(sd_bbox[1], sd_bbox[3]),
    #ylim = ca_bbox[3:4],
    ylim = c(sd_bbox[2], sd_bbox[4]),
    expand = TRUE) +
  geom_stars(data = sd_1984_r,
             inherit.aes = FALSE) +
  #scale_fill_viridis_d(option = "C") +
  scale_fill_manual(values = c(Farmland = "#567462",
                               Pasture = "#847361",
                               Urban = "#a4a5ac",
                               #Urban = "#bbbdb2",
                               #Urban = "#5e5f61",
                               Other = "#ded6b3",
                               Water = "#16333b",
                               Z = "beige")) +
  #scale_fill_discrete() +
  geom_point(data = ghcnd_stations_ca_pre_1984_missing_data_summary,
             aes(x = lon,
                 y = lat),
             color = "#39FF14"
             # color = proportion_of_missing_tmax)
  ) +
  # scale_color_viridis_b(breaks = seq(from = 0.15,
  #                                    to = 1,
  #                                    by = 0.15)) +
  labs(title = "Land Use and Weather Station Locations, c.1984") +
  theme_bw()


