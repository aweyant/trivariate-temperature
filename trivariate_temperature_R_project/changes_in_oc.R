
# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
# Declare constants -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"
boundaries_path <- "../data/boundaries/"
fmmp_path <- "../data/land_use/farmland_mapping_and_monitoring/"
images_path <- "../images/"

# Load data ---------------------------------------------------------------
ghcnd_stations_ca_pre_1984_df <- read_csv("~/Documents/research-projects/trivariate-temperature/data/ghcnd/ghcnd-stations-pre-1984.csv")

difference_summary <- ghcnd_stations_ca_pre_1984_df %>%
  filter(STATION %in% c("USC00049087","USC00047888")) %>%
  select(DATE, STATION, TMIN) %>%
  pivot_wider(names_from = c("STATION"),
              values_from = TMIN) %>%
  mutate(#diff = USC00049087 - USC00047888,
    diff = USC00047888,
         year = year(DATE),
         month = month(DATE)) %>%
  group_by(year) %>%
  summarize(mean_diff = mean(diff, na.rm = TRUE),
            date = min(DATE)) %>% 
  na.omit()

ggplot(data = difference_summary %>%
         filter(year >= 1960),
       aes(x = date,
           y = mean_diff)) +
  geom_line() +
  theme_bw()


