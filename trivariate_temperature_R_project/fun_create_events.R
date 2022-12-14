# Load packages -----------------------------------------------------------
library(tidyverse)

# create_events Desc ------------------------------------------------------
#' INPUT
#' df: a tidy data.frame
#' metadata_coords: vector of coordinate variables which will be preserved 
#' (e.g. time, lat, lon, plev)
#' unique_id: the subset of metadata coordinates which uniquely determine
#' a location. For the above example, this would be lat, lon, and plev. In
#' the case of station-level data, this might merely be station_id.
#' event_var: the variable by which our "event" is defined (e.g. hourly
#' precipitation, if precipitation events are being studied or temperature,
#' if heatwaves are being studied)
#' event_func: the function to apply to the event_variable when events are
#' summarized (e.g. sum() would be applied to a precipitation rate over an
#' event to create a precipitation total)
#' event_var_threshold: the threshold above which a time period is
#' considered part of an event (for example, 0.1mm might be used as a
#' threshold for 6-hourly precipitation to filter out trace precip)


# create_events Code ------------------------------------------------------
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater") {
  if(inequality_direction == "greater") {
    df <- df %>%
      # filter out instances of is.na(event_var)
      filter(!is.na(.data[[event_var]])) %>%
      # mark time increments as during an event "active" or not; specify a
      #' column of the unique id
      mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
      # uniquely identify geographic points by the specified combination of metadata_coords
      unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
      group_by(unique_id) %>%
      # number events at each geographic point
      mutate(event_number = {r <- rle(active)
      r$values <- cumsum(r$values) * r$values
      inverse.rle(r)
      }
      ) %>%
      ungroup() %>%
      # summarize events at each point
      filter(active == 1) %>%
      group_by(interaction(unique_id, event_number)) %>%
      mutate(event_length = sum(active, na.rm = TRUE)) %>%
      summarize(
        unique_id = unique_id,
        total = sum(.data[[event_var]] - event_var_threshold),
        max_rate = max(.data[[event_var]] - event_var_threshold),
        length = min(event_length),
        event_number = min(event_number),
        #event_number = min(event_number, na.rm = TRUE),
        across({{metadata_coords}}, ~min(., na.rm = TRUE))
      ) %>%
      ungroup() %>%
      unique()
    return(df[,-1]) # return data.frame without ugly grouping variable
  }
  else {
    df <- df %>%
      # filter out instances of is.na(event_var)
      filter(!is.na(.data[[event_var]])) %>%
      # mark time increments as during an event "active" or not; specify a
      #' column of the unique id
      mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
      # uniquely identify geographic points by the specified combination of metadata_coords
      unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
      group_by(unique_id) %>%
      # number events at each geographic point
      mutate(event_number = {r <- rle(active)
      r$values <- cumsum(r$values) * r$values
      inverse.rle(r)
      }
      ) %>%
      ungroup() %>%
      # summarize events at each point
      filter(active == 1) %>%
      group_by(interaction(unique_id, event_number)) %>%
      mutate(event_length = sum(active, na.rm = TRUE)) %>%
      summarize(
        unique_id = unique_id,
        total = sum(.data[[event_var]] - event_var_threshold),
        max_rate = min(.data[[event_var]]) - event_var_threshold,
        length = min(event_length),
        event_number = min(event_number),
        #event_number = min(event_number, na.rm = TRUE),
        across({{metadata_coords}}, ~min(., na.rm = TRUE))
      ) %>%
      ungroup() %>%
      unique()
    return(df[,-1]) # return data.frame without ugly grouping variable
  }
}


# Experimental create_events code -----------------------------------------
# added the option of quantile thresholds
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater",
                         event_var_threshold_type = "absolute") {
  if(event_var_threshold_type == "absolute") {
    if(inequality_direction == "greater") {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", all_of(unique_id_coords), remove = FALSE) %>%
        group_by(unique_id) %>%
        #number events at each geographic point
        mutate(event_number = {r <- rle(active);
        r$values <- cumsum(r$values) * r$values;
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = max(.data[[event_var]] - event_var_threshold),
          length = min(event_length),
          event_number = min(event_number),
          event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
      #return(df)
    }
    else {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = min(.data[[event_var]]) - event_var_threshold,
          length = min(event_length),
          event_number = min(event_number),
          #event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
  }
  else {
    if(inequality_direction == "greater") {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # calculate "active" threhold for each unique point
        mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                   probs = event_var_threshold)) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] > calc_event_var_threshold)) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          event_var_threshold = min(calc_event_var_threshold),
          total = sum(.data[[event_var]] - calc_event_var_threshold),
          max_rate = max(.data[[event_var]] - calc_event_var_threshold),
          length = min(event_length),
          #event_number = min(event_number),
          event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
    else {
      df <- df %>%
        # filter out instances of is.na(event_var)
        filter(!is.na(.data[[event_var]])) %>%
        # uniquely identify geographic points by the specified combination of metadata_coords
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        group_by(unique_id) %>%
        # calculate "active" threhold for each unique point
        mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                   probs = event_var_threshold)) %>%
        # mark time increments as during an event "active" or not; specify a
        #' column of the unique id
        mutate(active = as.numeric(.data[[event_var]] < calc_event_var_threshold)) %>%
        # number events at each geographic point
        mutate(event_number = {r <- rle(active)
        r$values <- cumsum(r$values) * r$values
        inverse.rle(r)
        }
        ) %>%
        ungroup() %>%
        # summarize events at each point
        filter(active == 1) %>%
        group_by(interaction(unique_id, event_number)) %>%
        mutate(event_length = sum(active, na.rm = TRUE)) %>%
        summarize(
          unique_id = unique_id,
          event_var_threshold = min(calc_event_var_threshold),
          total = sum(.data[[event_var]] - event_var_threshold),
          max_rate = min(.data[[event_var]]) - event_var_threshold,
          length = min(event_length),
          #event_number = min(event_number),
          event_number = min(event_number, na.rm = TRUE),
          across({{metadata_coords}}, ~min(., na.rm = TRUE))
        ) %>%
        ungroup() %>%
        unique()
      return(df[,-1]) # return data.frame without ugly grouping variable
    }
  }
}


# Experimental create_events code ii --------------------------------------
# added the option of disaggregated events
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater",
                         event_var_threshold_type = "absolute",
                         aggregate_event = TRUE) {
  if(aggregate_event) {
    if(event_var_threshold_type == "absolute") {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", all_of(unique_id_coords), remove = FALSE) %>%
          group_by(unique_id) %>%
          #number events at each geographic point
          mutate(event_number = {r <- rle(active);
          r$values <- cumsum(r$values) * r$values;
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = max(.data[[event_var]] - event_var_threshold),
            length = min(event_length),
            event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
        #return(df)
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = min(.data[[event_var]]) - event_var_threshold,
            length = min(event_length),
            event_number = min(event_number),
            #event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    else {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            event_var_threshold = min(calc_event_var_threshold),
            total = sum(.data[[event_var]] - calc_event_var_threshold),
            max_rate = max(.data[[event_var]] - calc_event_var_threshold),
            length = min(event_length),
            #event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            event_var_threshold = min(calc_event_var_threshold),
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = min(.data[[event_var]]) - event_var_threshold,
            length = min(event_length),
            #event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    
  }
  
  else{
    if(event_var_threshold_type == "absolute") {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", all_of(unique_id_coords), remove = FALSE) %>%
          group_by(unique_id) %>%
          #number events at each geographic point
          mutate(event_number = {r <- rle(active);
          r$values <- cumsum(r$values) * r$values;
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 across(all_of(metadata_coords), min)) 
        return(df[,-1]) # return data.frame without ugly grouping variable
        #return(df)
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 across(all_of(metadata_coords), min)) 
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    else {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 exceedance = .data[[event_var]] - calc_event_var_threshold,
                 across(all_of(metadata_coords), min)
          ) 
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 across(all_of(metadata_coords), min)) %>%
          ungroup()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
  }
}

# Experimental create_events code ii --------------------------------------
# Added the option to take quantiles over specific times; for example,
#' we may want to calculate a quantile on years [1960, 1989] in the months
#' [May, October]. This would be a "quantile filter" of "year %in% (1960:1989)
#' & month %in% (5:10)".
create_events<- function(df,
                         metadata_coords,
                         unique_id_coords,
                         event_var,
                         event_func,
                         event_var_threshold,
                         inequality_direction = "greater",
                         event_var_threshold_type = "absolute",
                         quantile_filter = NULL,
                         aggregate_event = TRUE) {
  if(aggregate_event) {
    if(event_var_threshold_type == "absolute") {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", all_of(unique_id_coords), remove = FALSE) %>%
          group_by(unique_id) %>%
          #number events at each geographic point
          mutate(event_number = {r <- rle(active);
          r$values <- cumsum(r$values) * r$values;
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = max(.data[[event_var]] - event_var_threshold),
            length = min(event_length),
            event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
        #return(df)
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = min(.data[[event_var]]) - event_var_threshold,
            length = min(event_length),
            event_number = min(event_number),
            #event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    else { # QUANTILE THRESHOLD
      # calculate "active" threhold for each unique point
      df <- df %>%
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        left_join(df %>%
                    unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
                    filter(eval(rlang::parse_expr(quantile_filter))) %>%
                    group_by(unique_id) %>%
                    summarize(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                                 probs = event_var_threshold,
                                                                 na.rm = TRUE)),
                  by = "unique_id")
      #print(head(df))
      
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          filter(!is.na(calc_event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          # unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            event_var_threshold = min(calc_event_var_threshold),
            total = sum(.data[[event_var]] - calc_event_var_threshold),
            max_rate = max(.data[[event_var]] - calc_event_var_threshold),
            length = min(event_length),
            #event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          filter(!is.na(calc_event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          # unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE)) %>%
          summarize(
            unique_id = unique_id,
            event_var_threshold = min(calc_event_var_threshold),
            total = sum(.data[[event_var]] - event_var_threshold),
            max_rate = min(.data[[event_var]]) - event_var_threshold,
            length = min(event_length),
            #event_number = min(event_number),
            event_number = min(event_number, na.rm = TRUE),
            across({{metadata_coords}}, ~min(., na.rm = TRUE))
          ) %>%
          ungroup() %>%
          unique()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    
  }
  
  else{
    if(event_var_threshold_type == "absolute") {
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", all_of(unique_id_coords), remove = FALSE) %>%
          group_by(unique_id) %>%
          #number events at each geographic point
          mutate(event_number = {r <- rle(active);
          r$values <- cumsum(r$values) * r$values;
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 exceedance = .data[[event_var]] - event_var_threshold,
                 across(all_of(metadata_coords), min)) 
        return(df[,-1]) # return data.frame without ugly grouping variable
        #return(df)
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 exceedance = .data[[event_var]] - event_var_threshold,
                 across(all_of(metadata_coords), min)) 
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
    else { # QUANTILE THRESHOLD
      # calculate "active" threhold for each unique point
      df <- df %>%
        unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
        left_join(df %>%
                    unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
                    filter(eval(rlang::parse_expr(quantile_filter))) %>%
                    group_by(unique_id) %>%
                    summarize(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                                 probs = event_var_threshold,
                                                                 na.rm = TRUE)),
                  by = "unique_id")
      if(inequality_direction == "greater") {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          filter(!is.na(calc_event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          # unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] > calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 exceedance = .data[[event_var]] - calc_event_var_threshold,
                 across(all_of(metadata_coords), min)
          ) 
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
      else {
        df <- df %>%
          # filter out instances of is.na(event_var)
          filter(!is.na(.data[[event_var]])) %>%
          filter(!is.na(calc_event_var_threshold)) %>%
          # uniquely identify geographic points by the specified combination of metadata_coords
          # unite("unique_id", {{ unique_id_coords }}, remove = FALSE) %>%
          group_by(unique_id) %>%
          # calculate "active" threhold for each unique point
          mutate(calc_event_var_threshold = quantile(x = .data[[event_var]],
                                                     probs = event_var_threshold)) %>%
          # mark time increments as during an event "active" or not; specify a
          #' column of the unique id
          mutate(active = as.numeric(.data[[event_var]] < calc_event_var_threshold)) %>%
          # number events at each geographic point
          mutate(event_number = {r <- rle(active)
          r$values <- cumsum(r$values) * r$values
          inverse.rle(r)
          }
          ) %>%
          ungroup() %>%
          # summarize events at each point
          filter(active == 1) %>%
          group_by(interaction(unique_id, event_number)) %>%
          mutate(event_length = sum(active, na.rm = TRUE),
                 across(all_of(metadata_coords), min)) %>%
          ungroup()
        return(df[,-1]) # return data.frame without ugly grouping variable
      }
    }
  }
}
