# app.R — Tabs on the right, Inputs on the left

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(RColorBrewer)
library(broom)

# ---- Load data ----
data_path <- "sdg7_data_latest.csv"
sdg7 <- read_csv(data_path, show_col_types = FALSE)
if (!is.numeric(sdg7$Year)) sdg7$Year <- as.integer(sdg7$Year)

# ---- Indicator labels ----
indicator_labels <- c(
  "Access_to_electricity"       = "Access to electricity (% of population)",
  "Access_to_clean_fuels"       = "Access to clean fuels for cooking (% of population)",
  "Renewable_energy_share"      = "Renewable energy share (% total final energy consumption)",
  "Electricity_from_renewables" = "Electricity production from renewables (% total)",
  "Energy_intensity"            = "Energy intensity (MJ per USD of GDP, PPP 2017)",
  "Energy_use_per_capita"       = "Energy use per capita (kg of oil equivalent)",
  "Electric_power_consumption"  = "Electric power consumption (kWh per capita)"
)
indicator_codes <- names(indicator_labels)

# ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("UN SDG 7 — Energy Trends Explorer"),
  
  tabsetPanel(
    # ==== TREND TAB ====
    tabPanel("Trend Over Time",
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput("trend_ind", "Indicator",
                                    choices = setNames(indicator_codes, indicator_labels)),
                        selectizeInput("countries", "Select Country (one or more)",
                                       choices = sort(unique(sdg7$Country)),
                                       multiple = TRUE),
                        selectizeInput("region", "Select Region(s)",
                                       choices = c("All", sort(unique(na.omit(sdg7$Region)))),
                                       multiple = TRUE),
                        checkboxInput("show_trend_tbl", "Show data table", FALSE)
                      )
               ),
               column(9,
                      h4("📈 Indicator Trend Over Time"),
                      plotlyOutput("trendplot", height = "450px"),
                      conditionalPanel("input.show_trend_tbl", DTOutput("tbl_trend"))
               )
             )
    ),
    
    # ==== BOXPLOT TAB ====
    tabPanel("Boxplot by Year",
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput("box_ind", "Indicator (for Boxplot)",
                                    choices = setNames(indicator_codes, indicator_labels)),
                        selectizeInput("box_region", "Select Region(s)",
                                       choices = sort(unique(na.omit(sdg7$Region))),
                                       multiple = TRUE)
                      )
               ),
               column(9,
                      h4("📊 Indicator Distribution by Year"),
                      plotlyOutput("boxplot", height = "450px"),
                      DTOutput("box_table")
               )
             )
    ),
    
    # ==== REGRESSION TAB ====
    tabPanel("Regression Analysis",
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput("x_var", "X Variable",
                                    choices = setNames(indicator_codes, indicator_labels)),
                        selectInput("y_var", "Y Variable",
                                    choices = setNames(indicator_codes, indicator_labels),
                                    selected = "Energy_use_per_capita"),
                        selectizeInput("reg_countries", "Select Country (one or more)",
                                       choices = sort(unique(sdg7$Country)),
                                       multiple = TRUE),
                        selectizeInput("reg_region", "Select Region(s)",
                                       choices = c("All", sort(unique(na.omit(sdg7$Region)))),
                                       multiple = TRUE),
                        checkboxInput("show_reg_tbl", "Show data table", FALSE)
                      )
               ),
               column(9,
                      h4("📉 Regression Analysis Between Indicators"),
                      htmlOutput("reg_equation"),
                      plotlyOutput("reg_plot", height = "450px"),
                      conditionalPanel("input.show_reg_tbl", DTOutput("reg_table"))
               )
             )
    ),
    
    # ==== RAW DATA TAB ====
    tabPanel("Raw Data",
             fluidRow(
               column(3,
                      wellPanel(
                        downloadButton("download_filtered", "Download dataset")
                      )
               ),
               column(9,
                      h4("🗃️ Full Dataset"),
                      DTOutput("raw_table")
               )
             )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- TREND ----
  trend_data <- reactive({
    req(input$trend_ind)
    validate(need(length(input$countries) > 0 | length(input$region) > 0,
                  "Please select at least one country or region."))
    
    dat <- tibble()
    
    if (length(input$countries) > 0) {
      dat <- sdg7 %>%
        filter(Country %in% input$countries) %>%
        select(Name = Country, Year, Region, Value = all_of(input$trend_ind)) %>%
        mutate(Type = "Country") %>%
        drop_na(Value)
    }
    
    if ("All" %in% input$region) {
      all_avg <- sdg7 %>%
        group_by(Year) %>%
        summarise(Value = mean(.data[[input$trend_ind]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Name = "All Regions (Average)", Type = "Region")
      dat <- bind_rows(dat, all_avg)
    } else if (length(input$region) > 0) {
      region_avg <- sdg7 %>%
        filter(Region %in% input$region) %>%
        group_by(Region, Year) %>%
        summarise(Value = mean(.data[[input$trend_ind]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Name = paste0(Region, " (avg by year)"), Type = "Region")
      dat <- bind_rows(dat, region_avg)
    }
    dat
  })
  
  output$trendplot <- renderPlotly({
    dat <- trend_data()
    validate(need(nrow(dat) > 0, "No data available."))
    
    p <- ggplot(dat, aes(x = Year, y = Value, group = Name)) +
      geom_line(aes(color = Name, linetype = Type), size = 1.2) +
      geom_point(aes(color = Name), size = 2, show.legend = FALSE) +
      scale_linetype_manual(values = c("Country" = "solid", "Region" = "dashed")) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank()) +
      labs(title = paste(indicator_labels[input$trend_ind], "over Time"),
           x = "Year", y = NULL)
    
    plt <- ggplotly(p)
    for (i in seq_along(plt$x$data)) {
      plt$x$data[[i]]$name <- gsub("\\(Country,|\\(Region,|\\)|Name|Type", "", plt$x$data[[i]]$name)
    }
    plt %>% layout(legend = list(title = list(text = "")))
  })
  
  output$tbl_trend <- renderDT({
    datatable(trend_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- BOXPLOT ----
  output$boxplot <- renderPlotly({
    req(input$box_ind)
    validate(need(length(input$box_region) > 0, "Please select at least one region."))
    
    dat <- sdg7 %>%
      filter(Region %in% input$box_region) %>%
      select(Region, Year, Value = all_of(input$box_ind)) %>%
      drop_na(Value)
    
    p <- ggplot(dat, aes(x = factor(Year), y = Value, fill = Region)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 16) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank()) +
      labs(title = paste("Distribution of", indicator_labels[input$box_ind], "by Year"),
           x = "Year", y = indicator_labels[input$box_ind])
    
    ggplotly(p)
  })
  
  output$box_table <- renderDT({
    req(input$box_ind, input$box_region)
    dat <- sdg7 %>%
      filter(Region %in% input$box_region) %>%
      select(Region, Year, Value = all_of(input$box_ind)) %>%
      drop_na(Value)
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- REGRESSION ----
  reg_data <- reactive({
    req(input$x_var, input$y_var)
    validate(need(length(input$reg_countries) > 0 | length(input$reg_region) > 0,
                  "Please select at least one country or region."))
    
    dat <- tibble()
    
    if (length(input$reg_countries) > 0) {
      dat <- sdg7 %>%
        filter(Country %in% input$reg_countries) %>%
        select(Country, Region, Year,
               X = all_of(input$x_var), Y = all_of(input$y_var)) %>%
        mutate(Type = "Country") %>%
        drop_na(X, Y)
    }
    
    if ("All" %in% input$reg_region) {
      all_avg <- sdg7 %>%
        group_by(Year) %>%
        summarise(X = mean(.data[[input$x_var]], na.rm = TRUE),
                  Y = mean(.data[[input$y_var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Country = "All Regions (Average)", Type = "Region")
      dat <- bind_rows(dat, all_avg)
    } else if (length(input$reg_region) > 0) {
      region_avg <- sdg7 %>%
        filter(Region %in% input$reg_region) %>%
        group_by(Region, Year) %>%
        summarise(X = mean(.data[[input$x_var]], na.rm = TRUE),
                  Y = mean(.data[[input$y_var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Country = paste0(Region, " (avg by year)"), Type = "Region")
      dat <- bind_rows(dat, region_avg)
    }
    
    dat
  })
  
  output$reg_equation <- renderUI({
    dat <- reg_data()
    validate(need(nrow(dat) > 2, "Not enough data for regression."))
    
    unique_names <- unique(dat$Country)
    colors <- RColorBrewer::brewer.pal(min(length(unique_names), 8), "Set2")
    eq_text <- c()
    
    for (i in seq_along(unique_names)) {
      ctry <- unique_names[i]
      sub <- dat %>% filter(Country == ctry)
      if (nrow(sub) > 2) {
        fit <- lm(Y ~ X, data = sub)
        r2 <- summary(fit)$r.squared
        eq_text <- c(eq_text, paste0(
          "<span style='color:", colors[i], "'><b>", ctry, ":</b> Y = ",
          round(coef(fit)[1], 3), " + ",
          round(coef(fit)[2], 3), "X (R² = ", round(r2, 3), ")</span>"
        ))
      }
    }
    
    HTML(paste(eq_text, collapse = "<br>"))
  })
  
  output$reg_plot <- renderPlotly({
    dat <- reg_data()
    validate(need(nrow(dat) > 2, "Not enough data for regression."))
    
    unique_names <- unique(dat$Country)
    colors <- RColorBrewer::brewer.pal(min(length(unique_names), 8), "Set2")
    
    p <- ggplot(dat, aes(x = X, y = Y, color = Country)) +
      geom_point(size = 3, alpha = 0.8, show.legend = FALSE) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, show.legend = TRUE) +
      scale_color_manual(values = colors) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank()) +
      labs(
        title = paste("Regression:", indicator_labels[input$y_var],
                      "vs", indicator_labels[input$x_var]),
        x = indicator_labels[input$x_var],
        y = indicator_labels[input$y_var]
      )
    
    ggplotly(p)
  })
  
  output$reg_table <- renderDT({
    datatable(reg_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- RAW DATA ----
  output$raw_table <- renderDT({
    datatable(sdg7, extensions = "Buttons",
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() "sdg7_data_latest.csv",
    content = function(file) write_csv(sdg7, file)
  )
}

# ---- Run App ----
shinyApp(ui, server)
