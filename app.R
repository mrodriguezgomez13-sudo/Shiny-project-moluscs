# =============================================================================
# Shiny project
# =============================================================================

library(shiny)
library(dplyr)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Visualización interactiva de moluscos bivalvos (OBIS)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sp",
        label   = "Especie",
        choices = sort(unique(datos_moluscos_filtrado$scientificName)),
        selected = sort(unique(datos_moluscos_filtrado$scientificName))[1]
      ),
      sliderInput(
        inputId = "year_range",
        label   = "Rango de años",
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
        label   = "Rango de profundidad (m)",
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
        tabPanel("Mapa de ocurrencias",
                 plotOutput("mapa", height = "400px")),
        tabPanel("Histograma de profundidad",
                 plotOutput("hist_depth", height = "400px"))
      )
    )
  )
)

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
      need(nrow(df) > 0, "No hay registros para los filtros seleccionados.")
    )
    ggplot(df, aes(x = decimalLongitude, y = decimalLatitude)) +
      borders("world", colour = "grey70", fill = "grey95") +
      geom_point(color = "darkblue", alpha = 0.7, size = 1) +
      coord_quickmap() +
      theme_minimal() +
      labs(
        title = paste("Mapa de ocurrencias de", input$sp),
        x = "Longitud",
        y = "Latitud"
      )
  })
  output$hist_depth <- renderPlot({
    df <- datos_filtrados()
    validate(
      need(nrow(df) > 0, "No hay registros para los filtros seleccionados.")
    )
    ggplot(df, aes(x = depth)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      theme_minimal() +
      labs(
        title = paste("Distribución de profundidades para", input$sp),
        x = "Profundidad (m)",
        y = "Frecuencia"
      )
  })
}
shinyApp(ui = ui, server = server)