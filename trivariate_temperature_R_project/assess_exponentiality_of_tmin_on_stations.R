# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(lattice)
#library(rnaturalearth)
#library(rnaturalearthdata)

# Declare constants -------------------------------------------------------
world <- ne_countries(returnclass = "sf")
ca_bbox <- c(-124.409591, -114.131211,
             32.534156, 42.009518)

# Load data ---------------------------------------------------------------
ghcnd_stations_ctn95pct_events_disagg_df <- read_csv("../data/ghcnd_ctn95pct_events_disagg.csv")
ghcnd_stations_ctn95pct_params_df <- read_csv("../data/ghcnd_stations_ctn95pct_params.csv")


# Create qq-plots of random stations --------------------------------------
N = 50
stations_of_interest <- (ghcnd_stations_ctn95pct_params_df %>%
                           filter(n_events >= 100) %>%
                           slice_sample(n = N))$unique_id
for(i in seq_along(stations_of_interest)) {
  cur_station = stations_of_interest[i]
  png(filename = paste0("../images/ghcnd/ctn95_pct/goodness-of-fit/exponentiality-qq-plots/",
                        cur_station,
                        "qq_plot.png"))
  qqmath(~exceedance,
         data = ghcnd_stations_ctn95pct_events_disagg_df %>%
           filter(station == cur_station),
         distribution = function(p) {qexp(p, rate = (ghcnd_stations_ctn95pct_params_df %>%
                                                       filter(unique_id == cur_station))$b_hat) %>%
             round()},
         panel = function(x, ...) {
           panel.qqmathline(x, ...)
           panel.qqmath(x, ...)
         },
         #f.value = seq(0,1,by=0.04),
         aspect = 1,#"xy",
         xlab = "Theoretical",
         ylab = "Observed",
         sub = paste0("Lon: ",
                      round((ghcnd_stations_ctn95pct_params_df %>%
                               filter(unique_id == cur_station))$longitude, 2),
                      "Lat: ",
                      round((ghcnd_stations_ctn95pct_params_df %>%
                               filter(unique_id == cur_station))$latitude, 2))
  )
  dev.off()
}

