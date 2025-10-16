# app.R — SDG7 with "All" Option for Region Selector

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)

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
    tabPanel("Trend Over Time",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("trend_ind", "Indicator",
                             choices = setNames(indicator_codes, indicator_labels)),
                 selectizeInput("countries", "Select Country (one or more)",
                                choices = sort(unique(sdg7$Country)),
                                multiple = TRUE),
                 selectizeInput("region", "Select Region(s)",
                                choices = c("All", sort(unique(na.omit(sdg7$Region)))),
                                multiple = TRUE),
                 checkboxInput("show_trend_tbl", "Show data table", FALSE)
               ),
               mainPanel(
                 h4("📈 Indicator Trend Over Time"),
                 plotlyOutput("trendplot", height = "450px"),
                 conditionalPanel("input.show_trend_tbl", DTOutput("tbl_trend"))
               )
             )
    ),
    
    tabPanel("Boxplot by Year",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("box_ind", "Indicator (for Boxplot)",
                             choices = setNames(indicator_codes, indicator_labels)),
                 selectizeInput("box_region", "Select Region(s)",
                                choices = sort(unique(na.omit(sdg7$Region))),
                                multiple = TRUE)
               ),
               mainPanel(
                 h4("📊 Indicator Distribution by Year"),
                 plotlyOutput("boxplot", height = "450px"),
                 DTOutput("box_table")
               )
             )
    ),
    
    tabPanel("Raw Data",
             h4("🗃️ Full Dataset"),
             downloadButton("download_filtered", "Download dataset"),
             DTOutput("raw_table")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Trend Plot ----
  output$trendplot <- renderPlotly({
    req(input$trend_ind)
    validate(need(length(input$countries) > 0 | length(input$region) > 0,
                  "Please select at least one country or one region."))
    
    dat <- tibble()
    
    # Country data
    if (length(input$countries) > 0) {
      dat <- sdg7 %>%
        filter(Country %in% input$countries) %>%
        select(Name = Country, Year, Region, Value = all_of(input$trend_ind)) %>%
        mutate(Type = "Country") %>%
        drop_na(Value)
    }
    
    # Region averages or All
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
        mutate(Name = paste0(Region, " Average"), Type = "Region")
      dat <- bind_rows(dat, region_avg)
    }
    
    dat$Name <- trimws(gsub("^Region,|^Country,|\\(|\\)", "", dat$Name))
    
    p <- ggplot(dat, aes(x = Year, y = Value, group = Name)) +
      geom_line(aes(color = Name,
                    linetype = ifelse(Type == "Region", "Region", "Country")),
                size = 1.2) +
      geom_point(aes(color = Name), size = 2, show.legend = FALSE) +
      scale_linetype_manual(values = c("Country" = "solid", "Region" = "dashed")) +
      labs(
        title = paste(indicator_labels[input$trend_ind], "over Time"),
        x = "Year", y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank())
    
    plt <- ggplotly(p, tooltip = c("x", "y", "color"))
    for (i in seq_along(plt$x$data)) {
      if (plt$x$data[[i]]$mode == "markers") plt$x$data[[i]]$showlegend <- FALSE
      if (grepl("\\(Country|\\(Region", plt$x$data[[i]]$name)) {
        plt$x$data[[i]]$name <- gsub("\\(Country,|\\(Region,|\\)", "", plt$x$data[[i]]$name)
      }
    }
    plt %>% layout(legend = list(title = list(text = "")))
  })
  
  # ---- Boxplot ----
  output$boxplot <- renderPlotly({
    req(input$box_ind)
    validate(need(length(input$box_region) > 0, "Please select at least one region."))
    
    dat <- sdg7 %>%
      filter(Region %in% input$box_region) %>%
      select(Region, Year, Value = all_of(input$box_ind)) %>%
      drop_na(Value)
    
    p <- ggplot(dat, aes(x = factor(Year), y = Value, fill = Region)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 16) +
      labs(
        title = paste("Distribution of", indicator_labels[input$box_ind], "by Year"),
        x = "Year", y = indicator_labels[input$box_ind]
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  # ---- Boxplot Data Table ----
  output$box_table <- renderDT({
    req(input$box_ind, input$box_region)
    dat <- sdg7 %>%
      filter(Region %in% input$box_region) %>%
      select(Region, Year, Value = all_of(input$box_ind)) %>%
      drop_na(Value)
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Raw Data ----
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
