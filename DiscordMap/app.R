library(shiny)
library(leaflet)
library(httr)
library(googlesheets4)
library(uuid)

# --- CONFIGURATION ---
# 1. Google Sheet URL
options(gargle_oauth_cache = ".secrets") 
sheet_url <- "https://docs.google.com/spreadsheets/d/1LZaRNp98FZlqjkH0sPD3u03J6ZKB2Jh0h67rKsd2eEc/edit?usp=sharing"

googlesheets4::gs4_deauth()

# 2. Discord Webhook URL (Captain Hook)
discord_webhook_url <- "https://discord.com/api/webhooks/1493100444333899937/LJk0bLrEBIOFT7ZJfnRejcVywDnnxxfsaoiYp-8HgTjmCyypSlSkg0pnVIugvuoBHAcZ"

# --- USER INTERFACE ---
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
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

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive value to store the selected point
  selected_point <- reactiveVal(NULL)
  
  # Initialize the base map
  output$mainMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -63.17, lat = 9.75, zoom = 2) # World view centered near you
  })
  
  # Observe map clicks to place a temporary marker
  observeEvent(input$mainMap_click, {
    click <- input$mainMap_click
    selected_point(click)
    
    leafletProxy("mainMap") %>%
      clearGroup("temp_marker") %>%
      addMarkers(lng = click$lng, lat = click$lat, group = "temp_marker")
  })
  
  # Action when 'Register' button is pressed
  observeEvent(input$saveData, {
    # Validation: Ensure name and map point exist
    req(input$userName, selected_point())
    
    # Prepare data frame for storage
    new_record <- data.frame(
      id = UUIDgenerate(),
      name = input$userName,
      lat = selected_point()$lat,
      lng = selected_point()$lng,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      # 1. Save to Google Sheets (Now active!)
      sheet_append(sheet_url, new_record)
      
      # 2. Prepare Discord Notification
      discord_payload <- list(
        content = paste0("🌎 **", input$userName, "** has joined the map from (", 
                         round(new_record$lat, 2), ", ", round(new_record$lng, 2), ")!")
      )
      
      # Send to Discord
      POST(url = discord_webhook_url, body = discord_payload, encode = "json")
      
      # Provide visual feedback
      output$statusFeedback <- renderUI({
        div(class = "alert alert-success", "Location shared successfully!")
      })
      
      # Clean temporary marker
      leafletProxy("mainMap") %>% clearGroup("temp_marker")
      
    }, error = function(e) {
      output$statusFeedback <- renderUI({
        div(class = "alert alert-danger", paste("Error:", e$message))
      })
    })
  })
}

# Run the Application
shinyApp(ui = ui, server = server)