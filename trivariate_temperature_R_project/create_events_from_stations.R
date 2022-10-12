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



# Running window for threshold --------------------------------------------
# ghcnd_stations_ctn95_threshold_df <- ghcnd_stations_ca_pre_1984_df %>% 
#   mutate(year_day = yday(date)) %>%
#   group_by(station) %>%
#   mutate(tmin_lag1 = lag(tmin), tmin_lag2 = lag(tmin, 2), tmin_lag3 = lag(tmin, 3),
#          tmin_lag4 = lag(tmin, 4), tmin_lag5 = lag(tmin, 5), tmin_lag6 = lag(tmin,6), tmin_lag7 = lag(tmin, 7),
#          tmin_lead1 = lead(tmin), tmin_lead2 = lead(tmin, 2), tmin_lead3 = lead(tmin, 3),
#          tmin_lead4 = lead(tmin, 4), tmin_lead5 = lead(tmin, 5), tmin_lead6 = lead(tmin,6), tmin_lead7 = lead(tmin,7)) %>%
#   ungroup() %>%
#   group_by(station, year_day) %>%
#   summarize(ctn95_theshold = quantile(x = c(tmin,
#                                             tmin_lag1, tmin_lag2, tmin_lag3,
#                                             tmin_lag4, tmin_lag5, tmin_lag6, tmin_lag7,
#                                             tmin_lead1, tmin_lead2, tmin_lead3,
#                                             tmin_lead4, tmin_lead5, tmin_lead6, tmin_lead7),
#                                       probs = 0.95,
#                                       na.rm = TRUE)) %>%
#   ungroup()
# 
# write_csv(x = ghcnd_stations_ctn95_threshold_df,
#           paste0(ghcnd_path, "ctn95_threshold.csv"))

ghcnd_stations_ctn95_threshold_df <- read_csv(file = paste0(ghcnd_path, "ctn95_threshold.csv"))

ghcnd_stations_ca_pre_1984_df <- ghcnd_stations_ca_pre_1984_df %>%
  mutate(year_day = yday(date)) %>%
  left_join(ghcnd_stations_ctn95_threshold_df,
            by = c("station", "year_day"))

# A quick look at Lindbergh Field
ghcnd_stations_ctn95_threshold_df %>% 
  filter(station == "USW00023188") %>% 
  ggplot(aes(x = year_day, y = ctn95_theshold)) +
  geom_point() +
  theme_bw()

# A quick look at Tahoe
ghcnd_stations_ctn95_threshold_df %>% 
  filter(station == "USC00048758") %>% 
  ggplot(aes(x = year_day, y = ctn95_theshold)) +
  geom_point() +
  theme_bw()

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


ghcnd_stations_tmin_95p_hhy_events_disagg_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.95,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989 & month %in% 5:10",
                event_var_threshold_type = "percentile",
                aggregate_event = FALSE)

write_csv(x = ghcnd_stations_tmin_95p_hhy_events_disagg_df,
          file = "../data/ghcnd_tmin_95p_hhy_events_disagg.csv")

ghcnd_stations_tmin_97p_hhy_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.975,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989 & month %in% 5:10",
                event_var_threshold_type = "percentile")

write_csv(x = ghcnd_stations_tmin_97p_hhy_events_df,
          file = "../data/ghcnd_tmin_97p_hhy_events.csv")


ghcnd_stations_tmin_97p_hhy_events_disagg_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.975,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989 & month %in% 5:10",
                event_var_threshold_type = "percentile",
                aggregate_event = FALSE)

write_csv(x = ghcnd_stations_tmin_97p_hhy_events_disagg_df,
          file = "../data/ghcnd_tmin_97p_hhy_events_disagg.csv")

ghcnd_stations_tmin_97p_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.975,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989",
                event_var_threshold_type = "percentile")

write_csv(x = ghcnd_stations_tmin_97p_events_df,
          file = "../data/ghcnd_tmin_97p_events.csv")

ghcnd_stations_tmin_99p_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  group_by(station) %>%
  mutate(first_year = min(year)) %>%
  ungroup() %>%
  filter(first_year <= 1960) %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0.99,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989",
                event_var_threshold_type = "percentile")

write_csv(x = ghcnd_stations_tmin_99p_events_df,
          file = "../data/ghcnd_tmin_99p_events.csv")

ghcnd_stations_ctn95pct_events_df <- ghcnd_stations_ca_pre_1984_df %>%
  mutate(tmin = tmin - ctn95_theshold) %>%
  group_by(station) %>%
  mutate(first_year = min(year)) %>%
  ungroup() %>%
  filter(first_year <= 1960) %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989",
                event_var_threshold_type = "absolute")

write_csv(x = ghcnd_stations_ctn95pct_events_df,
          file = "../data/ghcnd_ctn95pct_events.csv")

ghcnd_stations_ctn95pct_events_disagg_df <- ghcnd_stations_ca_pre_1984_df %>%
  mutate(tmin = tmin - ctn95_theshold) %>%
  group_by(station) %>%
  mutate(first_year = min(year)) %>%
  ungroup() %>%
  filter(first_year <= 1960) %>%
  #filter(station %in% stations_of_interest) %>%
  create_events(unique_id_coords = c("station"),
                metadata_coords = c("longitude", "latitude", "date"),
                event_var = "tmin",
                event_func = "sum",
                event_var_threshold = 0,
                inequality_direction = "greater",
                quantile_filter = "year %in% 1960:1989",
                event_var_threshold_type = "absolute",
                aggregate_event = FALSE)

write_csv(x = ghcnd_stations_ctn95pct_events_disagg_df,
          file = "../data/ghcnd_ctn95pct_events_disagg.csv")


# Explore ctn95 events ----------------------------------------------------
ghcnd_stations_ctn95pct_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x=year,y=n)) +
  geom_point() +
  theme_bw()

ghcnd_stations_ctn95pct_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(len = sum(length)) %>%
  ggplot(aes(x=year,y=len)) +
  geom_point() +
  theme_bw()


ghcnd_stations_ca_pre_1984_df %>% 
  filter(name == "SAN DIEGO INTERNATIONAL AIRPORT, CA US") %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(year) %>%
  summarise(mean_tmin = mean(tmin, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_tmin)) +
  geom_point() +
  theme_bw()

# Explore 99p events ------------------------------------------------------
ghcnd_stations_tmin_99p_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x=year,y=n)) +
  geom_point() +
  theme_bw()

ghcnd_stations_tmin_99p_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(len = sum(length)) %>%
  ggplot(aes(x=year,y=len)) +
  geom_point() +
  theme_bw()


ghcnd_stations_ca_pre_1984_df %>% 
  filter(name == "SAN DIEGO INTERNATIONAL AIRPORT, CA US") %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(year) %>%
  summarise(mean_tmin = mean(tmin, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_tmin)) +
  geom_point() +
  theme_bw()



# Plots of event summaries ------------------------------------------------
ghcnd_stations_tmin_97p_events_df %>%
  filter(unique_id == "USC00047767") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n_events = n()) %>%
  ggplot(aes(x=year,y=n_events)) +
  geom_point() +
  theme_bw()

ghcnd_stations_tmin_97p_events_df %>%
  filter(unique_id == "USC00047767") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(total = sum(total)) %>%
  ggplot(aes(x=year,y=total)) +
  geom_path() +
  theme_bw()

ghcnd_stations_ca_pre_1984_df %>% 
  filter(station == "USC00047767") %>%
  group_by(year) %>%
  summarise(mean_tmin = mean(tmin, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_tmin)) +
  geom_point() +
  theme_bw()

# LINDBERGH FIELD
ghcnd_stations_tmin_97p_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x=year,y=n)) +
  geom_point() +
  theme_bw()

ghcnd_stations_tmin_97p_events_df %>%
  filter(unique_id == "USW00023188") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(len = median(length)) %>%
  ggplot(aes(x=year,y=len)) +
  geom_point() +
  theme_bw()


ghcnd_stations_ca_pre_1984_df %>% 
  filter(name == "SAN DIEGO INTERNATIONAL AIRPORT, CA US") %>%
     group_by(year) %>%
     summarise(mean_tmin = mean(tmin, na.rm = TRUE)) %>%
     ggplot(aes(x = year, y = mean_tmin)) +
     geom_point() +
     theme_bw()
