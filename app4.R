# app.R — SDG 7 Explorer (with "All Years" option + Year selectable + fixed boxplot)

# ---- Libraries ----
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(reshape2)
library(shinythemes)

# ---- Load Data ----
data_path <- "sdg7_data_latest.csv"
sdg7 <- read_csv(data_path, show_col_types = FALSE)

# ---- Normalize column names ----
name_fix <- function(df) {
  nms <- names(df)
  repl <- c(
    "year" = "Year",
    "country" = "Country",
    "iso3" = "ISO3",
    "iso2" = "ISO2",
    "region" = "Region",
    "incomegroup" = "IncomeGroup"
  )
  idx <- tolower(nms) %in% names(repl)
  names(df)[idx] <- unname(repl[tolower(nms[idx])])
  df
}
sdg7 <- name_fix(sdg7)

# ---- Indicator Labels ----
indicator_labels <- c(
  "Year"               = "Year",
  "EG.ELC.ACCS.ZS"     = "Access to electricity (% of population)",
  "EG.CFT.ACCS.ZS"     = "Access to clean fuels for cooking (% of population)",
  "EG.FEC.RNEW.ZS"     = "Renewable energy share (% total final energy consumption)",
  "EG.ELC.RNWX.ZS"     = "Electricity production from renewables (% total)",
  "EG.EGY.PRIM.PP.KD"  = "Energy intensity (MJ per USD of GDP, PPP 2017)",
  "EG.USE.PCAP.KG.OE"  = "Energy use per capita (kg oil equivalent)",
  "EG.USE.ELEC.KH.PC"  = "Electric power consumption (kWh per capita)",
  "EN.ATM.CO2E.PC"     = "CO₂ emissions (metric tons per capita)",
  "EN.CO2.ETOT.ZS"     = "CO₂ from electricity and heat production (% of total)"
)
indicator_codes <- names(indicator_labels)

# ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("UN SDG 7 — Affordable & Clean Energy Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      selectInput("year", "Year",
                  choices = c("All Years", sort(unique(sdg7$Year), decreasing = TRUE)),
                  selected = "All Years"),
      selectizeInput("region", "Region (optional)",
                     choices = sort(unique(na.omit(sdg7$Region))),
                     multiple = TRUE, options = list(placeholder = "All regions")),
      selectizeInput("income", "Income Group (optional)",
                     choices = sort(unique(na.omit(sdg7$IncomeGroup))),
                     multiple = TRUE, options = list(placeholder = "All income groups")),
      selectizeInput("countries", "Countries (optional)",
                     choices = sort(unique(sdg7$Country)),
                     multiple = TRUE, options = list(placeholder = "All countries")),
      hr(),
      conditionalPanel("input.tab_selected == 'Boxplot'",
                       selectInput("box_ind", "Indicator", choices = setNames(indicator_codes, indicator_labels)),
                       checkboxInput("show_box_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Scatter'",
                       selectInput("sc_x", "X Indicator", choices = setNames(indicator_codes, indicator_labels)),
                       selectInput("sc_y", "Y Indicator", choices = setNames(indicator_codes, indicator_labels)),
                       checkboxInput("show_sc_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Regression'",
                       selectInput("reg_x", "Independent (X)", choices = setNames(indicator_codes, indicator_labels)),
                       selectInput("reg_y", "Dependent (Y)", choices = setNames(indicator_codes, indicator_labels)),
                       checkboxInput("show_reg_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Histogram'",
                       selectInput("hist_ind", "Indicator", choices = setNames(indicator_codes, indicator_labels)),
                       sliderInput("bins", "Bins", 5, 60, 25, 1),
                       checkboxInput("show_hist_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Correlation'",
                       checkboxInput("show_corr_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Summary'",
                       selectInput("sum_ind", "Indicator", choices = setNames(indicator_codes, indicator_labels)),
                       checkboxInput("show_sum_tbl", "Show data table", FALSE)),
      conditionalPanel("input.tab_selected == 'Raw Data'",
                       downloadButton("download_filtered", "Download filtered CSV"))
    ),
    
    mainPanel(
      tabsetPanel(id = "tab_selected", type = "tabs",
                  tabPanel("Boxplot",
                           h4("📦 Boxplot"),
                           plotlyOutput("boxplot", height = "420px"),
                           conditionalPanel("input.show_box_tbl", DTOutput("tbl_box"))
                  ),
                  tabPanel("Scatter",
                           h4("📊 Scatter Plot"),
                           plotlyOutput("scatter", height = "420px"),
                           conditionalPanel("input.show_sc_tbl", DTOutput("tbl_scatter"))
                  ),
                  tabPanel("Regression",
                           h4("📈 Regression"),
                           plotlyOutput("regplot", height = "420px"),
                           verbatimTextOutput("reg_summary"),
                           conditionalPanel("input.show_reg_tbl", DTOutput("tbl_reg"))
                  ),
                  tabPanel("Histogram",
                           h4("📊 Histogram"),
                           plotlyOutput("hist", height = "420px"),
                           conditionalPanel("input.show_hist_tbl", DTOutput("tbl_hist"))
                  ),
                  tabPanel("Correlation",
                           h4("🔗 Correlation Matrix"),
                           plotlyOutput("corrplot", height = "520px"),
                           conditionalPanel("input.show_corr_tbl", DTOutput("tbl_corr"))
                  ),
                  tabPanel("Summary",
                           h4("📋 Summary Statistics"),
                           DTOutput("summary"),
                           conditionalPanel("input.show_sum_tbl", DTOutput("tbl_sum"))
                  ),
                  tabPanel("Raw Data",
                           h4("🗃️ Full Raw Dataset"),
                           DTOutput("raw_table")
                  )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- sdg7
    if (input$year != "All Years") df <- df %>% filter(Year == input$year)
    if (length(input$region)) df <- df %>% filter(Region %in% input$region)
    if (length(input$income)) df <- df %>% filter(IncomeGroup %in% input$income)
    if (length(input$countries)) df <- df %>% filter(Country %in% input$countries)
    df
  })
  
  # ---- Boxplot ----
  output$boxplot <- renderPlotly({
    req(input$box_ind)
    dat <- filtered_data() %>% select(Country, Year, value = all_of(input$box_ind)) %>% drop_na()
    
    p <- if (input$year == "All Years") {
      ggplot(dat, aes(x = factor(Year), y = value, text = Country)) +
        geom_boxplot(fill = "#0072B2") +
        labs(title = paste0(indicator_labels[input$box_ind], " (All Years)"),
             x = "Year", y = "Value")
    } else {
      ggplot(dat, aes(y = value, text = Country)) +
        geom_boxplot(fill = "#0072B2") +
        labs(title = paste0(indicator_labels[input$box_ind], " (", input$year, ")"),
             x = NULL, y = "Value")
    } +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  output$tbl_box <- renderDT({
    dat <- filtered_data() %>% select(Country, Year, all_of(input$box_ind)) %>% drop_na()
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Scatter ----
  output$scatter <- renderPlotly({
    req(input$sc_x, input$sc_y)
    dat <- filtered_data() %>% select(Country, Year, x = all_of(input$sc_x), y = all_of(input$sc_y)) %>% drop_na()
    dat$x <- as.numeric(dat$x)
    dat$y <- as.numeric(dat$y)
    p <- ggplot(dat, aes(x = x, y = y, color = Year, text = Country)) +
      geom_point(size = 2) +
      labs(x = indicator_labels[input$sc_x],
           y = indicator_labels[input$sc_y],
           title = paste0(indicator_labels[input$sc_x], " vs ", indicator_labels[input$sc_y],
                          if (input$year == "All Years") " (All Years)" else paste0(" (", input$year, ")"))) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")
  })
  
  # ---- Regression ----
  output$regplot <- renderPlotly({
    req(input$reg_x, input$reg_y)
    dat <- filtered_data() %>% select(Country, x = all_of(input$reg_x), y = all_of(input$reg_y)) %>% drop_na()
    dat$x <- as.numeric(dat$x); dat$y <- as.numeric(dat$y)
    if (nrow(dat) < 3) return(NULL)
    model <- lm(y ~ x, dat)
    eq <- paste0("y = ", round(coef(model)[2], 2), "x + ", round(coef(model)[1], 2),
                 " | R²=", round(summary(model)$r.squared, 2))
    p <- ggplot(dat, aes(x = x, y = y, text = Country)) +
      geom_point(color = "#1b9e77") +
      geom_smooth(method = "lm", se = TRUE, color = "#d95f02") +
      labs(x = indicator_labels[input$reg_x], y = indicator_labels[input$reg_y], title = eq) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")
  })
  
  # ---- Histogram ----
  output$hist <- renderPlotly({
    req(input$hist_ind)
    dat <- filtered_data() %>% select(value = all_of(input$hist_ind)) %>% drop_na()
    p <- ggplot(dat, aes(x = value)) +
      geom_histogram(bins = input$bins, fill = "#4daf4a", color = "white") +
      labs(x = indicator_labels[input$hist_ind], y = "Count",
           title = paste0(indicator_labels[input$hist_ind],
                          if (input$year == "All Years") " (All Years)" else paste0(" (", input$year, ")"))) +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  # ---- Correlation ----
  output$corrplot <- renderPlotly({
    dat <- filtered_data() %>% select(all_of(indicator_codes)) %>% drop_na()
    if (nrow(dat) < 3) return(NULL)
    cm <- cor(dat, use = "pairwise.complete.obs")
    cm_long <- melt(cm)
    plot_ly(cm_long, x = ~Var1, y = ~Var2, z = ~value, type = "heatmap",
            colorscale = "RdBu", reversescale = TRUE) %>%
      layout(title = paste("Correlation Matrix",
                           if (input$year == "All Years") "(All Years)" else paste0("(", input$year, ")")))
  })
  
  # ---- Summary ----
  output$summary <- renderDT({
    dat <- filtered_data() %>% select(val = all_of(input$sum_ind)) %>% drop_na()
    stats <- tibble(
      Mean = round(mean(dat$val), 2),
      Median = round(median(dat$val), 2),
      SD = round(sd(dat$val), 2),
      Min = round(min(dat$val), 2),
      Max = round(max(dat$val), 2),
      N = nrow(dat)
    )
    datatable(stats, options = list(dom = "t"))
  })
  
  # ---- Raw Data ----
  output$raw_table <- renderDT({
    datatable(filtered_data(), extensions = "Buttons",
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() paste0("sdg7_filtered_",
                                 if (input$year == "All Years") "all_years" else input$year, ".csv"),
    content = function(file) write_csv(filtered_data(), file)
  )
}

# ---- Run App ----
shinyApp(ui, server)
