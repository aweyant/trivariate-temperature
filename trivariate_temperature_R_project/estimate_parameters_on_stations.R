#' ---
#' title: "A First Look of GTETLG Applied to Minimum Temperature"
#' output: github_document
#' ---
#'
#'

# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)

source("./gtetlg_utils.R")

# Declare constants -------------------------------------------------------
world <- ne_countries(returnclass = "sf")
ca_bbox <- c(-124.409591, -114.131211,
             32.534156, 42.009518)

# Load data ---------------------------------------------------------------
ghcnd_stations_tmin_97p_hhy_events_df <- read_csv("../data/ghcnd_tmin_97p_hhy_events.csv")

# Estimate parameters -----------------------------------------------------
ghcnd_stations_tmin_97p_hhy_params_df <- ghcnd_stations_tmin_97p_hhy_events_df %>%
  na.omit() %>%
  group_by(unique_id) %>%
  summarize(latitude = min(latitude),
            longitude = min(longitude),
            event_var_threshold = min(event_var_threshold),
            n_events = max(event_number),
            q_hat = q_est(length),
            p_hat = p_est(length),
            b_hat = b_est(length, total)) %>%
  na.omit() %>%
  filter(n_events >= 100)

write_csv(x = ghcnd_stations_tmin_97p_hhy_params_df,
          file = "../data/ghcnd_stations_tmin_97p_hhy_params.csv")

# Number of events by station ---------------------------------------------

#+ ghcnd-stations-tmin-97p-hhy-params-n-events.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
          ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = n_events)) +
  scale_color_viridis_b(breaks = seq(100,800,by = 100)) +
  labs(title = "Number of Events",
       color = "n") +
  theme_bw()


# Temperature threshold by station ----------------------------------------
#+ ghcnd-stations-tmin-97p-hhy-params-threshold.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = event_var_threshold)) +
  scale_color_viridis_b(breaks = seq(50,300,by = 50)) +
  labs(title = "Temperature Threshold\nFor Defining Heatwaves",
       color = paste0("\U00B0","C","*10")) +
  theme_bw()


# qhat by station ---------------------------------------------------------
#+ ghcnd-stations-tmin-97p-hhy-params-q-hat.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = q_hat)) +
  scale_color_viridis_b() +
  labs(title = "Probability that a Heatwave\nLasts Only One Day",
       color = expression(hat(q))) +
  theme_bw()

# qhat complement by station ---------------------------------------------------------
#+ ghcnd-stations-tmin-97p-hhy-params-q-hat-complement.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = 1-q_hat)) +
  scale_color_viridis_b() +
  labs(title = "Probability that a Heatwave\nLasts Beyond One Day",
       #color = paste0("1-", expression(hat(q)))
       color = "1-q_hat"
       ) +
  theme_bw()

# phat by station ---------------------------------------------------------

#+ ghcnd-stations-tmin-97p-hhy-params-p-hat.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = p_hat)) +
  scale_color_viridis_b() +
  labs(title = "Probability that a Heatwave\nEnds on Any Day After the First",
       #color = "p_hat"
       color = expression(hat(p))
       ) +
  theme_bw()


# Reciprocal of phat by station -------------------------------------------
#+ghcnd-stations-tmin-97p-hhy-params-p-hat-inv.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = 1/p_hat)) +
  scale_color_viridis_b(limits = c(0,8), breaks = seq(2,6,by =2)) +
  labs(title = "\'Persistance\' of Heatwaves\nAfter First Day",
      # color = "1/p_hat",
      color = paste0("p_hat", "\U207B", "\U00B9")) +
  theme_bw()


# bhat by station ---------------------------------------------------------
#+ ghcnd-stations-tmin-97p-hhy-params-b-hat.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = b_hat)) +
  scale_color_viridis_b() +
  labs(title = "Reciprocal of\nMean Temperature Exceedance",
       # color = "1/p_hat",
       color = expression(hat(beta))) +
  theme_bw()

# Reciprocal of bhat by station -------------------------------------------
#+ ghcnd-stations-tmin-97p-hhy-params-b-hat-inv.png
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_params_df,
             aes(x = longitude, y = latitude, color = 1/b_hat)) +
  scale_color_viridis_b() +
  labs(title = "Mean Temperature Exceedance\nOn Heatwave Days",
       # color = "1/p_hat",
       color = "1/\U03B2") +
  theme_bw()





