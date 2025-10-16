# app.R (robust indicator-checking version)
library(shiny)
library(tidyverse)
library(WDI)
library(plotly)
library(DT)


# --- download (may take a few seconds) ---
library(tidyverse)
data_selected <- read_csv('data_raw2.csv')


# Note: above rename maps "NY.GDP.PCAP.KD" -> "GDP_per_capita" if present

# --- pivot to long format (only the present indicator columns) ---

# --- UI and server (same pattern as before) ---
ui <- fluidPage(
  titlePanel("UN SDG Data Explorer — Robust"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Choose SDG Goal:", choices = sdg_map$SDG),
      uiOutput("indicator_ui"),
      selectInput("region", "Region:", choices = c("All", sort(unique(data_clean$region)))),
      helpText("Source: World Bank via WDI. If an indicator is missing, try WDIsearch() to find the correct code.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Plot", plotlyOutput("plot")),
        tabPanel("Summary Table", DTOutput("summary")),
        tabPanel("Regression", plotOutput("regPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$indicator_ui <- renderUI({
    indicators <- sdg_map %>% filter(SDG == input$sdg) %>% pull(indicator)
    selectInput("indicator", "Indicator:", choices = indicators)
  })
  
  filtered_data <- reactive({
    req(input$indicator)
    d <- data_clean %>% filter(indicator == input$indicator)
    if(input$region != "All") d <- d %>% filter(region == input$region)
    d
  })
  
  output$plot <- renderPlotly({
    req(filtered_data())
    plot_ly(filtered_data(), x = ~year, y = ~value, color = ~country,
            type = "scatter", mode = "lines") %>%
      layout(title = paste(input$sdg, ":", input$indicator, "over time"))
  })
  
  output$summary <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      group_by(region) %>%
      summarize(Mean = round(mean(value, na.rm = TRUE), 2),
                SD = round(sd(value, na.rm = TRUE), 2),
                N = n())
  })
  
  output$regPlot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = year, y = value)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression of", input$indicator, "over time"), x = "Year", y = input$indicator)
  })
}

shinyApp(ui, server)
