# Create server logic with improved view state management
server <- function(input, output, session) {
  # Create reactive values to store map state with European default center
  map_state <- reactiveValues(
    zoom = 4,
    lat = 50,    # Centered on Europe
    lng = 10,    # Centered on Europe
    bounds = NULL
  )
  
  # Initialize available songs once when the app starts
  observeEvent(u2data, {
    req(u2data)
    songs <- c("All", sort(unique(na.omit(u2data$song_title))))
    updateSelectizeInput(session, "song_filter",
                         choices = songs,
                         selected = "All")
  }, once = TRUE)  # Only run once when app starts
  
  # Observer to capture map bounds when they change
  observeEvent(input$city_map_bounds, {
    map_state$bounds <- input$city_map_bounds
  })
  
  # Observer to capture zoom level when it changes
  observeEvent(input$city_map_zoom, {
    map_state$zoom <- input$city_map_zoom
  })
  
  # Observer to capture center when it changes
  observeEvent(input$city_map_center, {
    map_state$lat <- input$city_map_center$lat
    map_state$lng <- input$city_map_center$lng
  })
  
  # Reset view button handler with European center
  observeEvent(input$reset_view, {
    map_state$zoom <- 4
    map_state$lat <- 50
    map_state$lng <- 10
  })
  
  # Update tour filter choices with counts, sorted chronologically
  observe({
    req(unique_shows)
    
    tour_counts <- tryCatch({
      unique_shows %>%
        group_by(tour) %>%
        summarise(
          show_count = n_distinct(showID),
          first_date = min(date, na.rm = TRUE),
          last_date = max(date, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(desc(last_date)) %>%
        mutate(tour_label = sprintf("%s (%d shows, %s-%s)", 
                                    tour, 
                                    show_count,
                                    format(first_date, "%b %Y"),
                                    format(last_date, "%b %Y"))) %>%
        pull(tour_label)
    }, error = function(e) {
      return(character(0))
    })
    
    updateSelectInput(session, "tour_filter", 
                      choices = c("All", tour_counts),
                      selected = input$tour_filter)
  })
  
  # Improved popup handling with HTML formatting
  showPopup <- function(lat, lng, content) {
    req(lat, lng, content)
    leafletProxy("city_map") %>%
      clearPopups() %>%
      addPopups(lng, lat, content,
                options = popupOptions(
                  closeButton = TRUE,
                  className = "custom-popup",
                  maxWidth = 300
                ))
  }
  
  # Enhanced marker click handling
  observeEvent(input$city_map_marker_click, {
    req(input$city_map_marker_click)
    event <- input$city_map_marker_click
    
    if (!is.null(event$lat) && !is.null(event$lng)) {
      filtered <- filtered_data()
      
      popup_content <- filtered %>%
        filter(abs(lat - event$lat) < 1e-10 & abs(long - event$lng) < 1e-10) %>%
        pull(popup_content)
      
      if (length(popup_content) > 0) {
        showPopup(event$lat, event$lng, popup_content[1])
      }
    }
  })
  
  # Reactive expression for filtered shows based on both tour and song
  filtered_shows <- reactive({
    req(unique_shows, u2data, input$song_filter, input$tour_filter)
    
    # Start with all shows
    shows <- unique_shows
    
    # Filter by tour if selected
    if (input$tour_filter != "All") {
      selected_tour <- sub(" \\(.*\\)$", "", input$tour_filter)
      shows <- shows %>% filter(tour == selected_tour)
    }
    
    # Filter by song if selected
    if (input$song_filter != "All") {
      # Get showIDs where the selected song was played
      song_shows <- u2data %>%
        filter(!is.na(song_title), 
               song_title == input$song_filter) %>%
        distinct(showID) %>%
        pull(showID)
      
      shows <- shows %>% filter(showID %in% song_shows)
      
      # Check if we have any shows after filtering
      if (nrow(shows) == 0) {
        # Reset song filter to "All"
        updateSelectizeInput(session, "song_filter", selected = "All")
        # Use unfiltered shows (only tour filter)
        shows <- unique_shows
        if (input$tour_filter != "All") {
          selected_tour <- sub(" \\(.*\\)$", "", input$tour_filter)
          shows <- shows %>% filter(tour == selected_tour)
        }
      }
    }
    
    shows
  })
  
  # Output for filter warning message
  output$filter_warning <- renderText({
    req(input$song_filter, input$tour_filter)
    if (input$song_filter != "All" && input$tour_filter != "All") {
      selected_tour <- sub(" \\(.*\\)$", "", input$tour_filter)
      shows <- unique_shows %>% 
        filter(tour == selected_tour) %>%
        inner_join(u2data %>% 
                     filter(song_title == input$song_filter) %>% 
                     distinct(showID))
      
      if (nrow(shows) == 0) {
        sprintf("'%s' was not played during the %s tour. Showing all songs for this tour instead.", 
                input$song_filter, selected_tour)
      }
    }
  })
  
  # Improved filtered data with validation
  filtered_data <- reactive({
    req(filtered_shows())
    
    tryCatch({
      # Group shows by city and create summary statistics
      data <- filtered_shows() %>%
        group_by(city, country) %>%
        summarise(
          show_count = n_distinct(showID),
          first_show = min(date, na.rm = TRUE),
          last_show = max(date, na.rm = TRUE),
          venues = list(unique(na.omit(venue))),
          tours = list(unique(na.omit(tour))),
          .groups = 'drop'
        ) %>%
        left_join(geocoded_cities, by = c("city", "country")) %>%
        mutate(
          log_shows = log(show_count + 1),
          popup_content = sprintf(
            '<div class="popup-content">
              <h3>%s, %s</h3>
              <table>
                <tr><td><b>Shows:</b></td><td>%d</td></tr>
                <tr><td><b>First show:</b></td><td>%s</td></tr>
                <tr><td><b>Last show:</b></td><td>%s</td></tr>
                <tr><td><b>Venues:</b></td><td>%s</td></tr>
                <tr><td><b>Tours:</b></td><td>%s</td></tr>
              </table>
            </div>',
            city, country, show_count, first_show, last_show,
            sapply(venues, function(x) paste(x, collapse = ", ")),
            sapply(tours, function(x) paste(x, collapse = ", "))
          )
        )
      
      # Validate coordinates
      validated_data <- validate_coords(data)
      
      # Filter out invalid coordinates
      validated_data %>% filter(valid_coords)
      
    }, error = function(e) {
      validate(need(FALSE, paste("Error filtering data:", e$message)))
      return(NULL)
    })
  })
  
  # Add validation status output
  output$validation_status <- renderText({
    data <- filtered_data()
    if (is.null(data)) return("")
    
    total_rows <- nrow(data) + sum(!data$valid_coords)
    invalid_rows <- sum(!data$valid_coords)
    
    if (invalid_rows > 0) {
      sprintf("Warning: %d out of %d locations have invalid or missing coordinates and were excluded from the map.", 
              invalid_rows, total_rows)
    } else {
      "All locations have valid coordinates."
    }
  })
  
  # Improved color scale with more distinct bins
  color_scale <- reactive({
    req(filtered_data())
    bins <- c(1, 3, 5, 10, 20, 50, Inf)
    colorBin("viridis", domain = filtered_data()$show_count, bins = bins, right = FALSE)
  })
  
  # Enhanced map rendering with view state preservation
  output$city_map <- renderLeaflet({
    req(filtered_data())
    
    tryCatch({
      map <- leaflet(filtered_data()) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addCircleMarkers(
          ~long, 
          ~lat,
          radius = ~pmax(5, log_shows * 3),
          popup = ~popup_content,
          label = ~sprintf("%s: %d shows", city, show_count),
          labelOptions = labelOptions(textsize = "12px"),
          color = ~color_scale()(show_count),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          opacity = 0.8,
          clusterOptions = markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            showCoverageOnHover = TRUE,
            zoomToBoundsOnClick = TRUE,
            animate = TRUE,
            maxClusterRadius = 50
          )
        ) %>%
        addLayersControl(
          baseGroups = c("Light", "Dark", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = color_scale(),
          values = ~show_count,
          title = "Number of Shows",
          opacity = 0.7,
          labFormat = labelFormat(digits = 0)
        )
      
      # Use stored view state
      map %>% setView(
        lng = map_state$lng,
        lat = map_state$lat,
        zoom = map_state$zoom
      )
    }, error = function(e) {
      validate(need(FALSE, paste("Error rendering map:", e$message)))
      return(NULL)
    })
  })
}