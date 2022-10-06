# Load packages -----------------------------------------------------------
library(tidyverse)

# Declare constants -------------------------------------------------------
ghcnd_pre_84_stations_list <- read_csv("~/Documents/research-projects/trivariate-temperature/data/ghcnd/ghcnd-pre-84-stations-list.txt",
                                       col_names = FALSE) %>% as.vector() %>% unlist()
ghcnd_path <- "../data/ghcnd/"


# Concatenation -----------------------------------------------------------
for(i in 1:length(ghcnd_pre_84_stations_list)) {
  if(i == 1) {
    ghcnd_stations_ca_pre_1984_df <- read_csv(paste0(ghcnd_path,
                                                     ghcnd_pre_84_stations_list[i]))
  }
  else{
    ghcnd_stations_ca_pre_1984_df <- bind_rows(ghcnd_stations_ca_pre_1984_df,
                                               read_csv(paste0(ghcnd_path,
                                                               ghcnd_pre_84_stations_list[i])))
  }
}

write_csv(x = ghcnd_stations_ca_pre_1984_df %>%
            select(c("STATION",
                     "DATE",
                     "LATITUDE",
                     "LONGITUDE",
                     "ELEVATION",
                     "NAME",
                     "PRCP",
                     "PRCP_ATTRIBUTES",
                     "TMAX",
                     "TMAX_ATTRIBUTES",
                     "TMIN",
                     "TMIN_ATTRIBUTES")),
          file = paste0(ghcnd_path, "ghcnd-stations-pre-1984.csv"))
