# ---- Libraries ----
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(reshape2)
library(tidyverse)

# ---- Load Data ----
sdg_data <- read_csv("sdg_data.csv")   # combined Goal 7 & 9 dataset

# ---- Goal Indicators ----
goal_indicators <- list(
  "Goal 7" = c(
    "Access to electricity (% of population)" = "EG.ELC.ACCS.ZS",
    "Access to clean fuels for cooking (% of population)" = "EG.CFT.ACCS.ZS",
    "Renewable energy share (% total final energy consumption)" = "EG.FEC.RNEW.ZS",
    "Energy use per capita (kg oil equivalent)" = "EG.USE.PCAP.KG.OE",
    "Electricity production from renewables (% total)" = "EG.ELC.RNWX.ZS"
  ),
  "Goal 9" = c(
    "Fixed broadband subscriptions (per 100 people)" = "IT.NET.BBND.P2",
    "Mobile cellular subscriptions (per 100 people)" = "IT.CEL.SETS.P2",
    "Research and development expenditure (% of GDP)" = "GB.XPD.RSDV.GD.ZS",
    "Manufacturing value added (% of GDP)" = "NV.IND.MANF.ZS",
    "High-technology exports (% of manufactured exports)" = "TX.VAL.TECH.MF.ZS"
  )
)

# ---- Goal Descriptions ----
goal_descriptions <- list(
  "Goal 7" = "Ensure access to affordable, reliable, sustainable and modern energy for all.",
  "Goal 9" = "Build resilient infrastructure, promote inclusive and sustainable industrialization and foster innovation."
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Interactive SDG Data Explorer — Goals 7 & 9"),
  sidebarLayout(
    sidebarPanel(
      selectInput("goal_select", "Select Goal", choices = names(goal_indicators)),
      checkboxInput("show_goal_desc", "Show Goal Description", value = FALSE),
      
      # BOX PLOT
      conditionalPanel(
        condition = "input.tab_selected == 'Boxplot'",
        uiOutput("indicator_box_ui"),
        selectInput("year_box", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        checkboxInput("show_boxplot_table", "Show Data Table", value = FALSE)
      ),
      
      # POINT PLOT
      conditionalPanel(
        condition = "input.tab_selected == 'Point Plot'",
        uiOutput("x_indicator_ui"),
        uiOutput("y_indicator_ui"),
        selectInput("year_point", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        checkboxInput("show_point_table", "Show Data Table", value = FALSE)
      ),
      
      # REGRESSION
      conditionalPanel(
        condition = "input.tab_selected == 'Regression'",
        uiOutput("x_reg_ui"),
        uiOutput("y_reg_ui"),
        selectInput("year_reg", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        checkboxInput("show_reg_table", "Show Data Table", value = FALSE)
      ),
      
      # HISTOGRAM
      conditionalPanel(
        condition = "input.tab_selected == 'Histogram'",
        uiOutput("indicator_hist_ui"),
        selectInput("year_hist", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        sliderInput("bins", "Number of bins", min = 5, max = 50, value = 20),
        checkboxInput("show_hist_table", "Show Data Table", value = FALSE)
      ),
      
      # CORRELATION
      conditionalPanel(
        condition = "input.tab_selected == 'Correlation Matrix'",
        selectInput("year_corr", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        checkboxInput("show_corr_table", "Show Data Table", value = FALSE)
      ),
      
      # SUMMARY
      conditionalPanel(
        condition = "input.tab_selected == 'Summary Statistics'",
        uiOutput("indicator_summary_ui"),
        selectInput("year_summary", "Select Year",
                    choices = sort(unique(sdg_data$year), decreasing = TRUE)),
        checkboxInput("show_summary_table", "Show Data Table", value = FALSE)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab_selected",
        
        # BOX PLOT
        tabPanel("Boxplot",
                 wellPanel(
                   tags$h4("📦 Boxplot"),
                   tags$p("A boxplot visualizes data distribution via minimum, Q1, median, Q3, and maximum values, highlighting spread and outliers.")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_box"))
                 ),
                 plotlyOutput("boxplot", height = "400px"),
                 DTOutput("boxplot_table")
        ),
        
        # POINT PLOT
        tabPanel("Point Plot",
                 wellPanel(
                   tags$h4("📊 Scatter Plot"),
                   tags$p("Shows relationships between two indicators. Hover to see country names.")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_point"))
                 ),
                 plotlyOutput("pointplot", height = "400px"),
                 DTOutput("pointplot_table")
        ),
        
        # REGRESSION
        tabPanel("Regression",
                 wellPanel(
                   tags$h4("📈 Regression"),
                   tags$p("Linear regression models Y ~ X. Red line shows best fit; hover to view countries.")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_reg"))
                 ),
                 plotlyOutput("regplot", height = "400px"),
                 DTOutput("reg_table"),
                 verbatimTextOutput("reg_summary")
        ),
        
        # HISTOGRAM
        tabPanel("Histogram",
                 wellPanel(
                   tags$h4("📊 Histogram"),
                   tags$p("Displays how frequently values occur within specified intervals (bins).")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_hist"))
                 ),
                 plotlyOutput("histogram", height = "400px"),
                 DTOutput("hist_table")
        ),
        
        # CORRELATION
        tabPanel("Correlation Matrix",
                 wellPanel(
                   tags$h4("🔗 Correlation Matrix"),
                   tags$p("Shows pairwise correlations between all indicators for the selected goal and year.")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_corr"))
                 ),
                 plotlyOutput("corr_plot", height = "400px"),
                 DTOutput("corr_table")
        ),
        
        # SUMMARY
        tabPanel("Summary Statistics",
                 wellPanel(
                   tags$h4("📋 Summary Statistics"),
                   tags$p("Provides mean, median, SD, min, and max for the selected indicator and year.")
                 ),
                 conditionalPanel(
                   condition = "input.show_goal_desc",
                   wellPanel(textOutput("goal_description_summary"))
                 ),
                 DTOutput("summary_table"),
                 DTOutput("summary_data_table")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Dynamic dropdowns
  output$indicator_box_ui <- renderUI({
    selectInput("indicator_box", "Choose Indicator", choices = goal_indicators[[input$goal_select]])
  })
  output$x_indicator_ui <- renderUI({
    selectInput("x_indicator", "X-axis Indicator", choices = goal_indicators[[input$goal_select]])
  })
  output$y_indicator_ui <- renderUI({
    selectInput("y_indicator", "Y-axis Indicator", choices = goal_indicators[[input$goal_select]])
  })
  output$x_reg_ui <- renderUI({
    selectInput("x_reg", "Independent Variable (X)", choices = goal_indicators[[input$goal_select]])
  })
  output$y_reg_ui <- renderUI({
    selectInput("y_reg", "Dependent Variable (Y)", choices = goal_indicators[[input$goal_select]])
  })
  output$indicator_hist_ui <- renderUI({
    selectInput("indicator_hist", "Choose Indicator", choices = goal_indicators[[input$goal_select]])
  })
  output$indicator_summary_ui <- renderUI({
    selectInput("indicator_summary", "Choose Indicator", choices = goal_indicators[[input$goal_select]])
  })
  
  # ---- Goal Descriptions for each tab ----
  output$goal_description_box <- renderText({ goal_descriptions[[input$goal_select]] })
  output$goal_description_point <- renderText({ goal_descriptions[[input$goal_select]] })
  output$goal_description_reg <- renderText({ goal_descriptions[[input$goal_select]] })
  output$goal_description_hist <- renderText({ goal_descriptions[[input$goal_select]] })
  output$goal_description_corr <- renderText({ goal_descriptions[[input$goal_select]] })
  output$goal_description_summary <- renderText({ goal_descriptions[[input$goal_select]] })
  
  # ---- Boxplot ----
  output$boxplot <- renderPlotly({
    req(input$indicator_box, input$year_box)
    dat <- sdg_data %>% filter(year == input$year_box) %>%
      select(country, value = all_of(input$indicator_box)) %>% drop_na()
    p <- ggplot(dat, aes(y = value, text = country)) +
      geom_boxplot(fill = "skyblue") + theme_minimal() +
      labs(title = paste(input$goal_select, "-", input$indicator_box, "(", input$year_box, ")"), y = "Value")
    ggplotly(p, tooltip = "text")
  })
  output$boxplot_table <- renderDT({
    req(input$show_boxplot_table)
    dat <- sdg_data %>% filter(year == input$year_box) %>%
      select(Country = country, Value = all_of(input$indicator_box)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ---- Point Plot ----
  output$pointplot <- renderPlotly({
    req(input$x_indicator, input$y_indicator, input$year_point)
    dat <- sdg_data %>% filter(year == input$year_point) %>%
      select(country, x = all_of(input$x_indicator), y = all_of(input$y_indicator)) %>% drop_na()
    p <- ggplot(dat, aes(x, y, text = country)) +
      geom_point(color = "steelblue") + theme_minimal() +
      labs(title = paste(input$goal_select, "Scatter Plot (", input$year_point, ")"),
           x = input$x_indicator, y = input$y_indicator)
    ggplotly(p, tooltip = "text")
  })
  output$pointplot_table <- renderDT({
    req(input$show_point_table)
    dat <- sdg_data %>% filter(year == input$year_point) %>%
      select(Country = country, X = all_of(input$x_indicator), Y = all_of(input$y_indicator)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ---- Regression ----
  output$regplot <- renderPlotly({
    req(input$x_reg, input$y_reg, input$year_reg)
    dat <- sdg_data %>% filter(year == input$year_reg) %>%
      select(x = all_of(input$x_reg), y = all_of(input$y_reg), country) %>% drop_na()
    model <- lm(y ~ x, data = dat)
    eq <- paste0("y = ", round(coef(model)[2],2), "x + ", round(coef(model)[1],2))
    p <- ggplot(dat, aes(x, y, text = country)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      annotate("text", x = min(dat$x), y = max(dat$y), label = eq, hjust = 0, vjust = 1) +
      theme_minimal() + labs(title = paste(input$goal_select, "Regression (", input$year_reg, ")"))
    ggplotly(p, tooltip = "text")
  })
  output$reg_table <- renderDT({
    req(input$show_reg_table)
    dat <- sdg_data %>% filter(year == input$year_reg) %>%
      select(Country = country, X = all_of(input$x_reg), Y = all_of(input$y_reg)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
  output$reg_summary <- renderPrint({
    dat <- sdg_data %>% filter(year == input$year_reg) %>%
      select(x = all_of(input$x_reg), y = all_of(input$y_reg)) %>% drop_na()
    summary(lm(y ~ x, data = dat))
  })
  
  # ---- Histogram ----
  output$histogram <- renderPlotly({
    req(input$indicator_hist, input$year_hist)
    dat <- sdg_data %>% filter(year == input$year_hist) %>%
      select(value = all_of(input$indicator_hist)) %>% drop_na()
    p <- ggplot(dat, aes(x = value)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = paste(input$goal_select, "Histogram (", input$year_hist, ")"), x = "Value", y = "Count")
    ggplotly(p)
  })
  output$hist_table <- renderDT({
    req(input$show_hist_table)
    dat <- sdg_data %>% filter(year == input$year_hist) %>%
      select(Value = all_of(input$indicator_hist)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ---- Correlation ----
  output$corr_plot <- renderPlotly({
    req(input$year_corr)
    inds <- goal_indicators[[input$goal_select]]
    dat <- sdg_data %>% filter(year == input$year_corr) %>% select(all_of(inds)) %>% drop_na()
    corr <- cor(dat)
    corr_long <- melt(corr)
    plot_ly(corr_long, x = ~Var1, y = ~Var2, z = ~value,
            type = "heatmap", colorscale = "RdBu", reversescale = TRUE) %>%
      layout(title = paste(input$goal_select, "Correlation Matrix (", input$year_corr, ")"))
  })
  output$corr_table <- renderDT({
    req(input$show_corr_table)
    inds <- goal_indicators[[input$goal_select]]
    dat <- sdg_data %>% filter(year == input$year_corr) %>%
      select(Country = country, all_of(inds)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  # ---- Summary Statistics ----
  output$summary_table <- renderDT({
    req(input$indicator_summary, input$year_summary)
    dat <- sdg_data %>% filter(year == input$year_summary) %>%
      select(value = all_of(input$indicator_summary)) %>% drop_na()
    stats <- data.frame(
      Mean = round(mean(dat$value),2),
      Median = round(median(dat$value),2),
      SD = round(sd(dat$value),2),
      Min = min(dat$value),
      Max = max(dat$value)
    )
    datatable(stats, rownames = FALSE, options = list(dom = "t"))
  })
  output$summary_data_table <- renderDT({
    req(input$show_summary_table)
    dat <- sdg_data %>% filter(year == input$year_summary) %>%
      select(Country = country, Value = all_of(input$indicator_summary)) %>% drop_na()
    datatable(dat, options = list(pageLength = 8, scrollX = TRUE))
  })
}

# ---- Run App ----
shinyApp(ui, server)
