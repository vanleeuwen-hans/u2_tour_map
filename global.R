# Load required packages
library(tidyverse)
library(tidygeocoder)
library(ggplot2)
library(viridis)
library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(maps)

# Function to safely read data with error handling
safe_read_data <- function(file_path) {
  tryCatch({
    read_csv(file_path, show_col_types = FALSE)
  }, error = function(e) {
    stop(paste("Error reading data file:", e$message))
  })
}

# Function to safely process cities
safe_process_cities <- function(data) {
  tryCatch({
    data %>%
      select(city, country) %>%
      distinct() %>%
      mutate(
        location = paste(city, country, sep = ", "),
        city = if_else(city == "", NA_character_, city),
        country = if_else(country == "", NA_character_, country)
      ) %>%
      filter(!is.na(city) & !is.na(country) & city != "" & country != "")
  }, error = function(e) {
    stop(paste("Error processing cities:", e$message))
  })
}

# Main processing
tryCatch({
  # Load data
  u2data <- safe_read_data('u2data_all_shows_clean_final.csv')
  
  # Process unique shows
  unique_shows <- u2data %>%
    select(showID, venue, city, country, date, tour) %>%
    distinct() %>%
    arrange(date)
  
  # Process unique cities
  unique_cities <- safe_process_cities(unique_shows)
  
  # Load or create geocoded_cities
  geocoded_file_path <- "geocoded_cities.csv"
  if (file.exists(geocoded_file_path)) {
    geocoded_cities <- safe_read_data(geocoded_file_path)
  } else {
    geocoded_cities <- unique_cities %>%
      geocode(
        address = location,
        method = 'osm',
        limit = 1,
        min_time = 0.5
      )
    write_csv(geocoded_cities, geocoded_file_path)
  }
  
  # Create frequency counts for cities
  city_frequency <- unique_shows %>%
    group_by(city, country) %>%
    summarise(
      show_count = n_distinct(showID),
      first_show = min(date, na.rm = TRUE),
      last_show = max(date, na.rm = TRUE),
      venues = list(unique(na.omit(venue))),
      tours = list(unique(na.omit(tour))),
      .groups = 'drop'
    ) %>%
    left_join(geocoded_cities, by = c("city", "country"))
  
  # Transform show counts
  city_frequency_transformed <- city_frequency %>%
    mutate(
      log_shows = log(show_count + 1),
      show_category = cut(
        show_count, 
        breaks = c(0, 1, 5, 10, 25, 50, 100, Inf),
        labels = c("1", "2-5", "6-10", "11-25", "26-50", "51-100", "100+"),
        include.lowest = TRUE
      )
    )
  
  # Create decade-based frequency
  city_by_decade <- unique_shows %>%
    mutate(
      decade = ifelse(
        !is.na(date),
        paste0(substr(as.character(date), 1, 3), "0s"),
        NA_character_
      )
    ) %>%
    filter(!is.na(decade)) %>%
    group_by(city, country, decade) %>%
    summarise(
      show_count = n_distinct(showID),
      .groups = 'drop'
    ) %>%
    left_join(geocoded_cities, by = c("city", "country"))
  
  # Create validation report
  validation <- list(
    total_shows = n_distinct(u2data$showID),
    processed_shows = nrow(unique_shows),
    unique_cities = n_distinct(u2data$city),
    unique_countries = n_distinct(u2data$country),
    unique_tours = n_distinct(u2data$tour),
    cities_with_coords = sum(!is.na(geocoded_cities$lat)),
    total_song_performances = nrow(u2data),
    shows_missing_setlist = sum(is.na(u2data$song_title)),
    total_shows_in_frequency = sum(city_frequency$show_count)
  )
  
  # Print validation report
  print("Data Validation Report:")
  print(validation)
  
}, error = function(e) {
  stop(paste("Error in main processing:", e$message))
})


# Create a validation function for coordinates
validate_coords <- function(data) {
  has_coords <- !is.na(data$lat) & !is.na(data$long)
  valid_lat <- data$lat >= -90 & data$lat <= 90
  valid_long <- data$long >= -180 & data$long <= 180
  valid_coords <- has_coords & valid_lat & valid_long
  data$valid_coords <- valid_coords
  invalid_rows <- data[!valid_coords, ]
  if (nrow(invalid_rows) > 0) {
    warning(sprintf("Found %d invalid coordinates:\n%s", 
                    nrow(invalid_rows),
                    paste(sprintf("City: %s, Country: %s, Lat: %s, Long: %s", 
                                  invalid_rows$city, 
                                  invalid_rows$country, 
                                  invalid_rows$lat, 
                                  invalid_rows$long), 
                          collapse = "\n")))
  }
  return(data)
}