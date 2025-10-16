# app.R
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)

# Load or download data
data_file <- "sdg_data.csv"

if (!file.exists(data_file)) {
  # Example download from World Bank SDG data
  url <- "https://databankfiles.worldbank.org/public/ddpext_download/SDG_csv.zip"
  temp <- tempfile()
  download.file(url, temp)
  unzip(temp, exdir = tempdir())
  csv_files <- list.files(tempdir(), pattern = "\\.csv$", full.names = TRUE)
  sdg_data <- read_csv(csv_files[1])
  write_csv(sdg_data, data_file)
  unlink(temp)
} else {
  sdg_data <- read_csv(data_file)
}

# Simplify dataset for app use
sdg_data <- sdg_data %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  mutate(across(where(is.character), as.factor))

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("UN SDG Interactive Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Options"),
      selectInput("goal", "Select SDG Goal:",
                  choices = c("Goal 7: Affordable and Clean Energy",
                              "Goal 9: Industry, Innovation and Infrastructure",
                              "Goal 1: No Poverty",
                              "Goal 2: Zero Hunger",
                              "Goal 3: Good Health and Well-being")),
      selectInput("xvar", "Select X Variable:", choices = names(sdg_data)),
      selectInput("yvar", "Select Y Variable:", choices = names(sdg_data)),
      checkboxInput("show_table", "Show/Hide Data Table", value = FALSE),
      hr(),
      helpText("This interactive app visualizes UN SDG data using various statistical tools.
                Use the controls above to filter and analyze data interactively.")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "pills",
        tabPanel("Boxplot",
                 h4("Boxplot Visualization"),
                 p("A boxplot shows the distribution of a variable through its quartiles,
                   highlighting the median, interquartile range, and potential outliers."),
                 plotlyOutput("boxplot"),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   h5("Data Used for Boxplot"),
                   DTOutput("table_boxplot")
                 )
        ),
        tabPanel("Scatter Plot",
                 h4("Scatter Plot Visualization"),
                 p("A scatter plot displays the relationship between two numeric variables,
                   helping visualize trends, clusters, or potential correlations."),
                 plotlyOutput("scatterPlot"),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   h5("Data Used for Scatter Plot"),
                   DTOutput("table_scatter")
                 )
        ),
        tabPanel("Regression",
                 h4("Regression Analysis"),
                 p("A regression plot shows a fitted line summarizing the linear relationship
                   between two numeric variables."),
                 plotlyOutput("regressionPlot"),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   h5("Data Used for Regression Plot"),
                   DTOutput("table_regression")
                 )
        ),
        tabPanel("Correlation",
                 h4("Correlation Matrix"),
                 p("The correlation matrix shows how strongly numeric variables are related.
                   Values close to 1 or -1 indicate strong positive or negative correlation."),
                 plotlyOutput("correlationPlot"),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   h5("Data Used for Correlation Matrix"),
                   DTOutput("table_corr")
                 )
        ),
        tabPanel("Summary Statistics",
                 h4("Summary Statistics"),
                 p("This section provides key descriptive statistics like mean, median,
                   standard deviation, min, and max values for the selected indicators."),
                 DTOutput("summary_table"),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   h5("Underlying Data"),
                   DTOutput("table_summary")
                 )
        ),
        tabPanel("Raw Data",
                 h4("Full Raw Dataset"),
                 p("Below is the complete dataset used in this app. You can search, filter,
                   and download the data for your own analysis."),
                 br(),
                 DTOutput("raw_data_table")
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    sdg_data
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    req(input$yvar)
    p <- ggplot(filtered_data(), aes_string(y = input$yvar)) +
      geom_boxplot(fill = "#1E90FF", alpha = 0.7) +
      theme_minimal() +
      labs(y = input$yvar, title = "Boxplot of Selected Variable")
    ggplotly(p)
  })
  output$table_boxplot <- renderDT({ filtered_data() })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    req(input$xvar, input$yvar)
    p <- ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "#2E8B57", alpha = 0.7) +
      theme_minimal() +
      labs(x = input$xvar, y = input$yvar, title = "Scatter Plot of Selected Variables")
    ggplotly(p)
  })
  output$table_scatter <- renderDT({ filtered_data() })
  
  # Regression plot
  output$regressionPlot <- renderPlotly({
    req(input$xvar, input$yvar)
    data <- filtered_data()
    model <- lm(as.formula(paste(input$yvar, "~", input$xvar)), data = data)
    eq <- paste0("y = ", round(coef(model)[2], 3), "x + ", round(coef(model)[1], 3))
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "#FF7F50", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      annotate("text", x = Inf, y = -Inf, label = eq, hjust = 1.1, vjust = -1, size = 4, color = "blue") +
      theme_minimal() +
      labs(title = "Linear Regression Plot", subtitle = eq)
    ggplotly(p)
  })
  output$table_regression <- renderDT({ filtered_data() })
  
  # Correlation
  output$correlationPlot <- renderPlotly({
    num_data <- filtered_data() %>% select(where(is.numeric))
    if (ncol(num_data) < 2) return(NULL)
    cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
    melt_cor <- reshape2::melt(cor_matrix)
    p <- ggplot(melt_cor, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      labs(title = "Correlation Matrix", fill = "Correlation")
    ggplotly(p)
  })
  output$table_corr <- renderDT({ filtered_data() })
  
  # Summary statistics
  output$summary_table <- renderDT({
    num_data <- filtered_data() %>% select(where(is.numeric))
    summary_df <- num_data %>%
      summarise_all(list(
        Mean = mean,
        Median = median,
        SD = sd,
        Min = min,
        Max = max
      ), na.rm = TRUE)
    datatable(t(summary_df), options = list(scrollX = TRUE))
  })
  output$table_summary <- renderDT({ filtered_data() })
  
  # Full raw data tab
  output$raw_data_table <- renderDT({
    datatable(
      sdg_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      extensions = 'Buttons'
    )
  })
}

shinyApp(ui, server)
