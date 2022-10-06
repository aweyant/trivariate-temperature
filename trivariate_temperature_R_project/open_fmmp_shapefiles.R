# Load packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(stars)

# Declare constants -------------------------------------------------------
path_to_fmmp <- "~/Documents/research-projects/trivariate-temperature/data/land_use/farmland_mapping_and_monitoring/"

#Sys.glob(paste0(path_to_fmmp, "1984_FMMP_shape_files/alameda*"))


# Open shapefile as simple feature collection -----------------------------
alameda_1984_sf <- st_read(paste0(path_to_fmmp, "1984_FMMP_shape_files/alameda1984.shp"))
alameda_2018_sf <- st_read(paste0(path_to_fmmp, "2018_in_progress_FMMP_shape_files/Important_Farmland_2018.gdb"))


# Test plot ---------------------------------------------------------------
plot(alameda_1984_sf["polygon_ty"], axes = TRUE, graticule = TRUE)

ggplot() +
  geom_sf(data = alameda_1984_sf["polygon_ty"],
          aes(fill = polygon_ty)) +
  scale_fill_discrete() +
  theme_bw()

#graticule test
# g = alameda_1984_sf %>% st_graticule()
# plot(st_geometry(g), axes = TRUE)

# Rasterizing simple feature ----------------------------------------------

# most basic method
st_rasterize(alameda_1984_sf["polygon_ty"]) %>% plot()

alameda_1984_sf["polygon_ty"] %>%
  mutate_if(is.character, as.factor) %>%
  st_rasterize() %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  plot(graticule = TRUE)

# ggplot method
ggplot() +
  geom_stars(data = alameda_1984_sf["polygon_ty"] %>%
               #filter(polygon_ty == "D") %>% # subset to developed area
               mutate_if(is.character, as.factor) %>%
               st_rasterize() %>%
               st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) +
  scale_fill_viridis_d(option = "C") +
  theme_bw()

ggplot() +
  geom_stars(data = alameda_2018_sf["polygon_ty"] %>%
               #filter(polygon_ty == "D") %>% # subset to developed area
               mutate_if(is.character, as.factor) %>%
               st_rasterize() %>%
               st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) +
  scale_fill_viridis_d(option = "C") +
  theme_bw()


# Combine simple features for each timeslice ------------------------------



