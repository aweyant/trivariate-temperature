# Load packages -----------------------------------------------------------
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(twosamples)

source("./gtetlg_utils.R")


# Declare constants -------------------------------------------------------
dts_image_path <- "../images/ghcnd/goodness-of-fit/dts-results/"
world <- ne_countries(returnclass = "sf")
ca_bbox <- c(-124.409591, -114.131211,
             32.534156, 42.009518)


# Load files --------------------------------------------------------------
ghcnd_stations_tmin_97p_hhy_events_df <- read_csv("../data/ghcnd_tmin_97p_hhy_events.csv")
ghcnd_stations_tmin_97p_hhy_params_df <- read_csv("../data/ghcnd_stations_tmin_97p_hhy_params.csv") %>%
  rename(n = n_events,
         q = q_hat,
         p = p_hat,
         b = b_hat) %>%
  filter(n_events >= 100)

ghcnd_stations_tmin_97p_hhy_gen_events_df <- lapply(X = ghcnd_stations_tmin_97p_hhy_params_df$unique_id,
                                                    FUN = function(cur_id) {
                                                      params = (ghcnd_stations_tmin_97p_hhy_params_df %>%
                                                                  na.omit() %>%
                                                                  filter(unique_id == cur_id))[1,5:8] %>%
                                                        as_vector() %>% unlist()
                                                      #print(params)
                                                      rgtetlg(n = params[1],
                                                              q = params[2],
                                                              p = params[3],
                                                              b = params[4]) %>%
                                                        mutate(unique_id = cur_id)
                                                    }) %>%
  bind_rows()

ghcnd_stations_tmin_97p_hhy_events_df <- ghcnd_stations_tmin_97p_hhy_events_df %>%
  filter(unique_id %in% (ghcnd_stations_tmin_97p_hhy_gen_events_df$unique_id %>% unique()))


# Run tests ---------------------------------------------------------------
N = 50
stations_of_interest <- ghcnd_stations_tmin_97p_hhy_gen_events_df$unique_id %>%
  unique() #%>%
  #sample(size = N)

ghcnd_stations_tmin_97p_hhy_dts_result_df <- lapply(X = stations_of_interest,
                                                    FUN = function(id) {
                                                      data.frame(station = id,
                                                                 latitude = (ghcnd_stations_tmin_97p_hhy_params_df %>%
                                                                               filter(unique_id == id))$latitude,
                                                                 longitude = (ghcnd_stations_tmin_97p_hhy_params_df %>%
                                                                                filter(unique_id == id))$longitude,
                                                                 p_val = dts_test(a = (ghcnd_stations_tmin_97p_hhy_events_df %>%
                                                                                         filter(unique_id == id))$total,
                                                                                  b = (ghcnd_stations_tmin_97p_hhy_gen_events_df %>%
                                                                                         filter(unique_id == id))$event_magnitude)[[2]])
                                                    }) %>%
  bind_rows()


# Some plots and maps about results ---------------------------------------

# Histogram of p_vals
png(filename = paste0(dts_image_path, "97p_hhy_total_dts_p_val_hist.png"))
ghcnd_stations_tmin_97p_hhy_dts_result_df$p_val %>% hist %>%
  plot(main = "DTS Test Results: Tmin, HHY, CA", xlab = "p-Value")
dev.off()

# Map of p_vals
ggplot(data = world) +
  geom_sf(fill = NA) +
  coord_sf(xlim = ca_bbox[1:2],
           ylim = ca_bbox[3:4]) +
  geom_point(data = ghcnd_stations_tmin_97p_hhy_dts_result_df,
             aes(x = longitude, y = latitude, color = p_val)) +
  scale_color_viridis_b(limits = c(0,0.4),
                        breaks = seq(0.1,0.4,by = 0.1)) +
  labs(title = "Event Magnitude\nDTS Test Results",
       color = paste0("p-Val.")) +
  theme_bw()

ggsave(filename = paste0(dts_image_path, "97p_hhy_total_dts_p_val_map.png"),
       width = 14,
       height = 14,
       units = "cm",
       dpi = 400)











