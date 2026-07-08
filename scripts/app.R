# =============================================================================
# Shiny Application for Bivalve Mollusk Visualization
# Dataset: OBIS
# Author: Mònica Rodríguez Gómez
# Date: January 2026
# =============================================================================



# -----------------------------------------------------------------------------
# 0. Load libraries
# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. UserInterface
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Interactive Visualization of Mediterranean Bivalve Mollusks (OBIS)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sp",
        label   = "Select a species",
        choices = sort(unique(datos_moluscos_filtrado$scientificName)),
        selected = sort(unique(datos_moluscos_filtrado$scientificName))[1]
      ),
      sliderInput(
        inputId = "year_range",
        label   = "Year Range",
        min     = min(datos_moluscos_filtrado$year, na.rm = TRUE),
        max     = max(datos_moluscos_filtrado$year, na.rm = TRUE),
        value   = c(
          min(datos_moluscos_filtrado$year, na.rm = TRUE),
          max(datos_moluscos_filtrado$year, na.rm = TRUE)
        ),
        step    = 1,
        sep     = ""
      ),
      sliderInput(
        inputId = "depth_range",
        label   = "Depth range (m)",
        min     = floor(min(datos_moluscos_filtrado$depth, na.rm = TRUE)),
        max     = ceiling(max(datos_moluscos_filtrado$depth, na.rm = TRUE)),
        value   = c(
          floor(min(datos_moluscos_filtrado$depth, na.rm = TRUE)),
          ceiling(max(datos_moluscos_filtrado$depth, na.rm = TRUE))
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Occurrence Map", 
                 plotOutput("map", height = "500px")),
        tabPanel("Depth Histogram", 
                 plotOutput("depth_hist", height = "500px")),
        tabPanel("Data Table", 
                 DT::dataTableOutput("data_table"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 2. Server
# Statistics of filtered data
# Map
# Depth histogram 
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  datos_filtrados <- reactive({
    req(input$sp, input$year_range, input$depth_range)
    datos_moluscos_filtrado %>%
      filter(
        scientificName == input$sp,
        !is.na(year),
        year >= input$year_range[1],
        year <= input$year_range[2],
        !is.na(depth),
        depth >= input$depth_range[1],
        depth <= input$depth_range[2]
      )
  })
  output$mapa <- renderPlot({
    df <- datos_filtrados()
    validate(
      need(nrow(df) > 0, "No records found for selected filters.")
    )
    ggplot(df, aes(x = decimalLongitude, y = decimalLatitude)) +
      borders("world", colour = "grey70", fill = "grey95") +
      geom_point(color = "darkblue", alpha = 0.7, size = 1) +
      coord_quickmap() +
      theme_minimal() +
      labs(
        title = paste("Mapa de ocurrencias de", input$sp),
        x = "Longitude",
        y = "Latitude"
      )
  })
  output$hist_depth <- renderPlot({
    df <- datos_filtrados()
    validate(
      need(nrow(df) > 0, "No records found for selected filters.")
    )
    ggplot(df, aes(x = depth)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      theme_minimal() +
      labs(
        title = paste("Depth distribution for", input$sp),
        x = "Depth (m)",
        y = "Frequency"
      )
  })
}

# -----------------------------------------------------------------------------
# 3. Run the application
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)