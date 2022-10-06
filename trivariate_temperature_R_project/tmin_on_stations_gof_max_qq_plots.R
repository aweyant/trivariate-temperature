
# Load packages -----------------------------------------------------------
library(tidyverse)
library(foreach)

source("./gtetlg_utils.R")

# Load files --------------------------------------------------------------
ghcnd_stations_tmin_97p_hhy_events_df <- read_csv("../data/ghcnd_tmin_97p_hhy_events.csv")
ghcnd_stations_tmin_97p_hhy_params_df <- read_csv("../data/ghcnd_stations_tmin_97p_hhy_params.csv") %>%
  rename(n = n_events,
         q = q_hat,
         p = p_hat,
         b = b_hat)

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


# Cookie-cutter histogram function ----------------------------------------
source("./fun_overlapping_histograms.R")





# Test function -----------------------------------------------------------
N = 50
stations_of_interest <- ghcnd_stations_tmin_97p_hhy_gen_events_df$unique_id %>%
  unique() %>%
  sample(size = N)
for(i in seq_along(stations_of_interest)){
  id = stations_of_interest[i]
  png(filename = paste0("../images/ghcnd/goodness-of-fit/max-qq-plots/",
                        id,
                        "_",
                        "qq_plot.png"),
      height = 12,
      width = 12,
      unit = "cm",
      res = 200)
  qqplot(y = (ghcnd_stations_tmin_97p_hhy_events_df %>%
                filter(unique_id == id))$max_rate,
         x = (ghcnd_stations_tmin_97p_hhy_gen_events_df %>%
                filter(unique_id == id))$event_max,
         asp = 1,
         xlab = "Generated",
         ylab = "Observed",
         main = paste0("Magnitude Emperical QQ: ",
                       id),
         sub = paste0("Lat: = ",
                      round((ghcnd_stations_tmin_97p_hhy_params_df %>%
                               filter(unique_id == id))$latitude, 3),
                      "Lon: = ",
                      round((ghcnd_stations_tmin_97p_hhy_params_df %>%
                               filter(unique_id == id))$longitude, 3))
  )
  abline(a = 0, b = 1, col = "steelblue", lwd = 2)
  dev.off()
}

#parallel::stopCluster(my.cluster)





