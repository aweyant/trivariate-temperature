# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)

source("./fun_create_events.R")
# Declare constants -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"

# Import data -------------------------------------------------------------
ghcnd_stations_ca_pre_1984_df <- read_csv(paste0(ghcnd_path,
                                                 "ghcnd-stations-pre-1984.csv")) %>%
  rename_with(.fn = tolower) %>%
  mutate(year = year(date),
         month = month(date))


# Create events -----------------------------------------------------------
stations_of_interest <- (ghcnd_stations_ca_pre_1984_df$station %>% unique())[1:10]

ghcnd_stations_tmin_90p_hhy_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.90,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989 & month %in% 5:10",
                event_var_threshold_type = "percentile")

write_csv(x = ghcnd_stations_tmin_90p_hhy_events_df,
          file = "../data/ghcnd_tmin_90p_hhy_events.csv")

ghcnd_stations_tmin_95p_hhy_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.95,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989 & month %in% 5:10",
                event_var_threshold_type = "percentile")

write_csv(x = ghcnd_stations_tmin_95p_hhy_events_df,
          file = "../data/ghcnd_tmin_95p_hhy_events.csv")
