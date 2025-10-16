# app.R
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(gapminder)

# --- 1. Prepare dataset ---
data_clean <- gapminder %>%
  rename(
    GDP_per_capita = gdpPercap,
    Life_expectancy = lifeExp,
    Population = pop
  )

# --- 2. Map indicators to SDG goals ---
sdg_map <- tibble(
  SDG = c("1: No Poverty",
          "3: Good Health and Well-Being",
          "4: Quality Education",
          "13: Climate Action"),
  indicator_choices = I(list(
    c("GDP_per_capita", "Population"),
    c("Life_expectancy"),
    c("GDP_per_capita"),  # placeholder, could later use literacy/education data
    c("GDP_per_capita")   # placeholder for CO₂ data if added later
  ))
)

# --- 3. Build UI ---
ui <- fluidPage(
  titlePanel("🌍 SDG Data Explorer for Intro to Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Select SDG Goal:",
                  choices = sdg_map$SDG,
                  selected = "3: Good Health and Well-Being"),
      uiOutput("indicator_ui"),  # will update dynamically
      selectInput("continent", "Continent:",
                  choices = c("All", unique(data_clean$continent))),
      hr(),
      helpText("Data source: Gapminder dataset (1952–2007)")
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

# --- 4. Server ---
server <- function(input, output, session) {
  
  # dynamically update indicator options when SDG changes
  output$indicator_ui <- renderUI({
    indicator_choices <- sdg_map %>%
      filter(SDG == input$sdg) %>%
      pull(indicator_choices) %>%
      .[[1]]
    selectInput("indicator", "Choose Indicator:", choices = indicator_choices)
  })
  
  # filter dataset
  filtered_data <- reactive({
    req(input$indicator)
    d <- data_clean %>%
      select(country, continent, year, value = all_of(input$indicator))
    if (input$continent != "All") d <- d %>% filter(continent == input$continent)
    d
  })
  
  # trend plot
  output$plot <- renderPlotly({
    req(filtered_data())
    plot_ly(filtered_data(),
            x = ~year, y = ~value, color = ~country,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = paste(input$sdg, ":", input$indicator, "over time"))
  })
  
  # summary table
  output$summary <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      group_by(continent) %>%
      summarize(
        Mean = round(mean(value, na.rm = TRUE), 2),
        SD = round(sd(value, na.rm = TRUE), 2),
        N = n()
      )
  })
  
  # regression
  output$regPlot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = year, y = value)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        title = paste("Regression of", input$indicator, "over time"),
        x = "Year", y = input$indicator
      )
  })
}

# --- 5. Run app ---
shinyApp(ui, server)
