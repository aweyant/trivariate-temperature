# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)


# Declare constants -------------------------------------------------------
ghcnd_path <- "../data/ghcnd/"
images_path <- "../images/ghcnd/station_inventory/"


# Load in station list ----------------------------------------------------
ghcnd_stations_df <- read_table(paste0(ghcnd_path, "ghcnd-stations-ca-identifying-columns.txt"),
                                col_names = c("station_id",
                                              "lat",
                                              "lon",
                                              "elevation")) %>%
  left_join(read_csv(paste0(ghcnd_path, "ghcnd-stations-ca-first-record.txt"),
                     col_names = c("station_id", "first_record_date")),
            by = "station_id") %>%
  mutate(first_record_year = year(first_record_date))


# Count stations by start date --------------------------------------------
png(file = paste0(images_path, "histogram_record_start_times.png"),
    width = 381,
    height = 356,
    units = "px")
ghcnd_stations_df$first_record_year %>%
  hist %>%
  plot(main = "Histogram of\nStation Record Start Times",
       sub = paste0("n = ", nrow(ghcnd_stations_df), " (",
                    nrow(ghcnd_stations_df %>% filter(first_record_date < 1984)),
                    " before 1984)"),
       xlab = "Year")
abline(v = 1984, col = "red")
dev.off()
                              


# Save list of files with pre-1984 stations -------------------------------

# column name was subsequently deleted manually in text editor
write_csv(x = ghcnd_stations_df %>%
            filter(first_record_year < 1984) %>%
            select(station_id) %>%
            mutate(station_id = paste0(station_id, ".csv")),
          file = paste0(ghcnd_path,
                        "ghcnd-pre-84-stations-list.txt"))


