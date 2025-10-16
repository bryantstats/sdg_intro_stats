# app.R
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

# --- 1. Create mock SDG-style data ----
set.seed(123)
sdg_data <- expand.grid(
  SDG = c("1: No Poverty", "3: Good Health", "4: Quality Education", "13: Climate Action"),
  Indicator = c("GDP per Capita", "Life Expectancy", "Literacy Rate", "CO2 Emissions"),
  Region = c("Africa", "Asia", "Europe", "Americas"),
  Year = 2000:2024
) %>%
  mutate(
    Country = sample(c("Country A", "Country B", "Country C", "Country D"), n(), replace = TRUE),
    Value = case_when(
      Indicator == "GDP per Capita" ~ runif(n(), 1000, 50000),
      Indicator == "Life Expectancy" ~ runif(n(), 50, 85),
      Indicator == "Literacy Rate" ~ runif(n(), 40, 100),
      Indicator == "CO2 Emissions" ~ runif(n(), 1, 15)
    )
  )

# --- 2. UI layout ----
ui <- fluidPage(
  titlePanel("UN SDG Data Explorer — Intro to Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Choose SDG Goal:",
                  choices = unique(sdg_data$SDG)),
      uiOutput("indicator_ui"),
      selectInput("region", "Region:", choices = c("All", unique(sdg_data$Region))),
      selectInput("stat_type", "Statistic:",
                  choices = c("Summary Stats", "Trend Plot", "Regression Sandbox")),
      hr(),
      helpText("Sample data (for demo only)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("plot")),
        tabPanel("Summary Table", DTOutput("summary")),
        tabPanel("Regression Sandbox", plotOutput("regPlot"))
      )
    )
  )
)

# --- 3. Server logic ----
server <- function(input, output, session) {
  # Dynamically update indicator choices
  output$indicator_ui <- renderUI({
    indicator_choices <- sdg_data %>%
      filter(SDG == input$sdg) %>%
      pull(Indicator) %>%
      unique()
    selectInput("indicator", "Choose Indicator:", choices = indicator_choices)
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- sdg_data %>% filter(SDG == input$sdg, Indicator == input$indicator)
    if (input$region != "All") data <- data %>% filter(Region == input$region)
    data
  })
  
  # Trend Plot
  output$plot <- renderPlotly({
    req(filtered_data())
    plot_ly(filtered_data(),
            x = ~Year, y = ~Value, color = ~Country,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = paste("Trends in", input$indicator))
  })
  
  # Summary statistics
  output$summary <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      group_by(Region) %>%
      summarize(
        Mean = mean(Value, na.rm = TRUE),
        SD = sd(Value, na.rm = TRUE),
        N = n()
      )
  })
  
  # Regression sandbox
  output$regPlot <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Value)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Simple Linear Regression for", input$indicator),
           x = "Year", y = input$indicator)
  })
}

# --- 4. Run the app ----
shinyApp(ui, server)
