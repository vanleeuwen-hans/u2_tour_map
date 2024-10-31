# Create a Shiny UI with improved layout
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Add a title
  titlePanel("Tour Map Visualization"),
  
  # Move filters to the top
  fluidRow(
    column(
      width = 4,
      selectInput("tour_filter", "Filter by Tour:", 
                  choices = c("All"),  # We'll update this dynamically
                  selected = "All",
                  width = "100%")
    ),
    column(
      width = 4,
      selectizeInput("song_filter", "Filter by Song:",
                     choices = c("All"),  # Initialize with just "All"
                     selected = "All",
                     options = list(
                       placeholder = 'Select a song...',
                       maxItems = 1,
                       closeAfterSelect = TRUE
                     ),
                     width = "100%")
    ),
    column(
      width = 2,
      actionButton("reset_view", "Reset Map View", 
                   class = "btn btn-primary",
                   style = "margin-top: 25px;")
    )
  ),
  
  # Add validation status message and filter warning
  fluidRow(
    column(
      width = 12,
      textOutput("validation_status")
    ),
    column(
      width = 12,
      textOutput("filter_warning"),
      style = "color: red;"
    )
  ),
  
  # Main map panel
  fluidRow(
    column(
      width = 12,
      leafletOutput("city_map", height = "725px")
    )
  )
)