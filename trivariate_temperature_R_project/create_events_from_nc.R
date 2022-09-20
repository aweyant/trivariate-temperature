# Load packages -----------------------------------------------------------
library(tidync)

args = commandArgs(trailingOnly = TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
print(args)

source_file_path <- args[1]
print(source_file_path)
threshold_value <- args[2]
print(threshold_value)
percentile_threshold_choice <- args[3]
print(percentile_threshold_choice)
event_var_name <- args[4]
print(event_var_name)

dest_file_path <- paste0("../data/",
                         event_var_name,
                         "_",
                         threshold_value,
                         "_",
                         percentile_threshold_choice,
                         "_events.csv")
print(dest_file_path)

threshold_value <- as.numeric(threshold_value)/100

# Load needed functions ---------------------------------------------------
source(file = "fun_create_events.R")
source(file = "create_parallel_backend.R")

# Suppress Unnecessary Warnings -------------------------------------------
options(dplyr.summarise.inform = FALSE)

# List lats and lons ------------------------------------------------------
complete_nc <- tidync(x = source_file_path)
lat_v <- (complete_nc %>% activate("D0") %>% hyper_array())$lat
lon_v <- (complete_nc %>% activate("D1") %>% hyper_array())$lon





# Apply create_events() function ------------------------------------------
N = 20
foreach(i = (length(lon_v):1)) %dopar% {
  #for(i in 1:1) {
  start_time <- Sys.time()
  #for(j in seq(from = 1, to = length(lat_v) - (N-1), by = N)) {
  #print(lat_v[j:(j+(N-1))])
  tidync(x = source_file_path) %>%
    hyper_filter(lon = (lon == lon_v[i])) %>%
    #hyper_filter(lat = (lat %in% lat_v[j:j+(N-1)])) %>%
    hyper_tibble() %>%
    create_events(unique_id_coords = c("lon", "lat"),
                  metadata_coords = c("lon", "lat", "time"),
                  event_var = event_var_name,
                  event_func = "sum",
                  event_var_threshold = threshold_value,
                  inequality_direction = "greater",
                  event_var_threshold_type = percentile_threshold_choice) %>%
    write_csv(file = dest_file_path, append = TRUE)
  gc()
  end_time <- Sys.time()
  print(end_time-start_time)
  #}
  print(paste0("Progress: ", signif(100 * i/length(lon_v), digits = 2), "%"))
}

parallel::stopCluster(cl = my.cluster)


# Rename columns ----------------------------------------------------------
complete_events_df <- read_csv(dest_file_path,
                                    col_names = c("unique_id",
                                                  "event_var_threshold",
                                                  "total",
                                                  "max_rate",
                                                  "length",
                                                  "event_number",
                                                  "lon", "lat", "time")) %>%
  na.omit() %>%
  # filter(str_detect(unique_id, regex("[0-9]{3}\\.[0-9]*[_][0-9]+\\.[0-9]*"))) %>%
  filter(lat > 27.0, lat < 52.0, lon > 235.0, lon < 248.0) %>%
  unite("unique_id", lon, lat, sep = "_", remove = FALSE)

write_csv(x = complete_events_df,
          file = dest_file_path)
