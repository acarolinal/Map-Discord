library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(uuid)
library(shinythemes)

# ========== CONFIGURATION ==========
apps_script_url <- Sys.getenv("APPS_SCRIPT_URL")
discord_webhook_url <- Sys.getenv("DISCORD_WEBHOOK_URL")
# ========== HELPER FUNCTIONS ==========
load_points <- function() {
  tryCatch({
    resp <- GET(paste0(apps_script_url, "?action=get"), timeout(10))
    if (status_code(resp) != 200) return(data.frame())
    text <- content(resp, "text", encoding = "UTF-8")
    data <- fromJSON(text)
    if (is.null(data$points)) return(data.frame())
    df <- as.data.frame(data$points, stringsAsFactors = FALSE)
    if (nrow(df) == 0) return(data.frame())
    names(df) <- tolower(names(df))
    lat_col <- grep("^lat|^latitude|^y$", names(df), value = TRUE)[1]
    lng_col <- grep("^lng|^lon|^long|^longitude|^x$", names(df), value = TRUE)[1]
    if (is.na(lat_col) || is.na(lng_col)) return(data.frame())
    data.frame(name = df$name, lat = as.numeric(df[[lat_col]]), lng = as.numeric(df[[lng_col]]), stringsAsFactors = FALSE)
  }, error = function(e) data.frame())
}

save_point <- function(record) {
  resp <- POST(
    apps_script_url,
    body = toJSON(record, auto_unbox = TRUE),
    content_type_json(),
    encode = "json"
  )
  status_code(resp) == 200
}

# ========== UI ==========
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Global Study Community Map"),
  sidebarLayout(
    sidebarPanel(
      helpText("Follow these steps to join:"),
      tags$ol(
        tags$li("Click your location on the map."),
        tags$li("Enter your name or nickname below."),
        tags$li("Click 'Register my Location'.")
      ),
      hr(),
      textInput("userName", "Full Name / Nickname:", placeholder = "e.g. Aurora"),
      actionButton("saveData", "Register my Location", class = "btn-success btn-block"),
      hr(),
      uiOutput("statusFeedback")
    ),
    mainPanel(
      leafletOutput("mainMap", height = "700px")
    )
  )
)

# ========== SERVER ==========
server <- function(input, output, session) {
  
  selected_point <- reactiveVal(NULL)
  existing_points <- reactiveVal(data.frame())
  
  # Function to redraw markers from existing_points()
  update_map_markers <- function() {
    points <- existing_points()
    proxy <- leafletProxy("mainMap")
    proxy %>% clearGroup("existing_points")
    
    if (nrow(points) > 0 && all(c("lat", "lng") %in% names(points))) {
      proxy %>% addCircleMarkers(
        data = points,
        lng = ~lng, lat = ~lat,
        radius = 6, color = "#007bff", fillOpacity = 0.7,
        label = ~name,
        group = "existing_points"
      )
    }
  }
  
  # Initial load of points
  observe({
    points <- load_points()
    existing_points(points)
    update_map_markers()
  })
  
  # Base map (empty, markers added later)
  output$mainMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -63.17, lat = 9.75, zoom = 2)
  })
  
  # Click on map -> temporary marker
  observeEvent(input$mainMap_click, {
    click <- input$mainMap_click
    selected_point(click)
    leafletProxy("mainMap") %>%
      clearGroup("temp_marker") %>%
      addMarkers(lng = click$lng, lat = click$lat, group = "temp_marker")
  })
  
  # Save new point
  observeEvent(input$saveData, {
    req(input$userName, selected_point())
    
    new_record <- list(
      id = UUIDgenerate(),
      name = trimws(input$userName),
      lat = selected_point()$lat,
      lng = selected_point()$lng,
      timestamp = as.character(Sys.time())
    )
    
    tryCatch({
      if (!save_point(new_record)) {
        stop("Failed to write to Google Sheets (HTTP error)")
      }
      
      # Discord notification
      POST(
        discord_webhook_url,
        body = list(content = paste0("🌎 **", new_record$name, "** joined the map from (", 
                                     round(new_record$lat, 2), ", ", round(new_record$lng, 2), ")!")),
        encode = "json"
      )
      
      # Update local points
      current <- existing_points()
      new_df <- data.frame(
        id = new_record$id,
        name = new_record$name,
        lat = new_record$lat,
        lng = new_record$lng,
        timestamp = new_record$timestamp,
        stringsAsFactors = FALSE
      )
      existing_points(rbind(current, new_df))
      update_map_markers()
      
      # Clean UI
      leafletProxy("mainMap") %>% clearGroup("temp_marker")
      selected_point(NULL)
      updateTextInput(session, "userName", value = "")
      
      output$statusFeedback <- renderUI({
        div(class = "alert alert-success", "✅ Location shared successfully!")
      })
      delay(5000, output$statusFeedback <- renderUI({ NULL }))
      
    }, error = function(e) {
      output$statusFeedback <- renderUI({
        div(class = "alert alert-danger", paste("❌ Error:", e$message))
      })
    })
  })
}

shinyApp(ui = ui, server = server)