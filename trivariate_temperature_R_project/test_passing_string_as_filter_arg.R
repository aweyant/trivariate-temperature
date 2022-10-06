
# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)


# Declare constatns -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"

# Import data -------------------------------------------------------------
ghcnd_stations_ca_pre_1984_df <- read_csv(paste0(ghcnd_path,
                                                 "ghcnd-stations-pre-1984.csv")) %>%
  rename_with(.fn = tolower) %>%
  mutate(year = year(date),
         month = month(date))

stations_of_interest <- (ghcnd_stations_ca_pre_1984_df$station %>% unique())[1:10]


# Quantile over selected period by regular means --------------------------
ghcnd_stations_ca_pre_1984_df %>%
  filter(station %in% stations_of_interest) %>%
  filter(year %in% (1990:2019), month %in% (5:10)) %>%
  group_by(station) %>%
  summarize(calc_threshold = quantile(x = tmin,
                                      probs = 0.98,
                                      na.rm = TRUE))

# Quantile over selected period with string as arg ------------------------
quantile_filter <- "year %in% (1990:2019) & month %in% (5:10)"

calc_theshold_df <- ghcnd_stations_ca_pre_1984_df %>%
  #filter(station %in% stations_of_interest) %>%
  filter(eval(rlang::parse_expr(quantile_filter))) %>%
  group_by(station) %>%
  summarize(calc_threshold = quantile(x = tmin,
                                      probs = 0.98,
                                      na.rm = TRUE))


# Result ------------------------------------------------------------------
# SUCCESS - RESULTS ARE THE SAME

# Left joining calculated quantiles with rest of the data -----------------
ghcnd_stations_ca_pre_1984_df %>%
  left_join(calc_theshold_df, by = "station") %>%
  select(station, date, tmin, calc_threshold) %>%
  na.omit()


# Result ------------------------------------------------------------------
# SUCCESS - LEFT JOIN DID NOT FAIL OR CULL ROWS


