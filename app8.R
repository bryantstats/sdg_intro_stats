# app.R — SDG7 Dashboard
# Adds: Map Visualization (Plotly + Leaflet) and Summary Statistics (stacked plots)
# Keeps: About, Trend (clean legend), Boxplot, Regression (color-matched equations), Raw Data

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(RColorBrewer)
library(broom)
library(leaflet)
library(scales)

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

year_min <- min(sdg7$Year, na.rm = TRUE)
year_max <- max(sdg7$Year, na.rm = TRUE)

# ---- Custom CSS for dashboard cards ----
custom_css <- "
.card-panel {
  background-color: #f9f9f9;
  border-radius: 12px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.08);
  padding: 15px;
  margin-bottom: 15px;
}
.card-panel h4 {
  margin-top: 5px;
  color: #2c3e50;
  font-weight: 600;
}
"

# ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(custom_css))),
  titlePanel("🌍 UN SDG 7 — Energy Trends Explorer"),
  br(),
  
  tabsetPanel(
    # ==== ABOUT TAB ====
    tabPanel("About This App",
             fluidRow(
               column(12,
                      div(class = "card-panel",
                          h3("About This App"),
                          p("This interactive dashboard explores the United Nations Sustainable Development Goal (SDG 7): ",
                            strong("Ensure access to affordable, reliable, sustainable and modern energy for all.")),
                          p("It connects statistical techniques to real-world data from the ",
                            strong("World Bank UN SDG database"),
                            " for educational use in an Introduction to Statistics course."),
                          tags$ul(
                            tags$li(strong("📈 Trend Over Time:"), " visualize how energy indicators evolve."),
                            tags$li(strong("📊 Boxplot by Year:"), " compare yearly distributions across regions."),
                            tags$li(strong("📉 Regression Analysis:"), " explore relationships between indicators."),
                            tags$li(strong("🌍 Map Visualization:"), " view the indicator on Plotly and Leaflet maps."),
                            tags$li(strong("🧮 Summary Statistics:"), " quick descriptive stats with visual distributions."),
                            tags$li(strong("🗃️ Raw Data:"), " view or download the full dataset.")
                          ),
                          p("Created by ", strong("Dr. Ken [Your Last Name]"),
                            " (Associate Professor), to demonstrate applied data analysis within the UN SDG framework."),
                          tags$hr(),
                          p(em("Use the tabs above to explore global energy data interactively."))
                      )
               )
             )
    ),
    
    # ==== TREND TAB ====
    tabPanel("Trend Over Time",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Controls"),
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
                      div(class = "card-panel",
                          h4("📈 Indicator Trend Over Time"),
                          plotlyOutput("trendplot", height = "450px"),
                          conditionalPanel("input.show_trend_tbl", DTOutput("tbl_trend"))
                      )
               )
             )
    ),
    
    # ==== BOXPLOT TAB ====
    tabPanel("Boxplot by Year",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Controls"),
                          selectInput("box_ind", "Indicator (for Boxplot)",
                                      choices = setNames(indicator_codes, indicator_labels)),
                          selectizeInput("box_region", "Select Region(s)",
                                         choices = sort(unique(na.omit(sdg7$Region))),
                                         multiple = TRUE)
                      )
               ),
               column(9,
                      div(class = "card-panel",
                          h4("📊 Indicator Distribution by Year"),
                          plotlyOutput("boxplot", height = "450px"),
                          DTOutput("box_table")
                      )
               )
             )
    ),
    
    # ==== REGRESSION TAB ====
    tabPanel("Regression Analysis",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Controls"),
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
                      div(class = "card-panel",
                          h4("📉 Regression Analysis Between Indicators"),
                          htmlOutput("reg_equation"),
                          plotlyOutput("reg_plot", height = "450px"),
                          conditionalPanel("input.show_reg_tbl", DTOutput("reg_table"))
                      )
               )
             )
    ),
    
    # ==== MAP VISUALIZATION TAB (new) ====
    tabPanel("Map Visualization",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Map Controls"),
                          selectInput("map_ind", "Indicator",
                                      choices = setNames(indicator_codes, indicator_labels)),
                          sliderInput("map_year", "Year", min = year_min, max = year_max,
                                      value = year_max, step = 1, sep = ""),
                          checkboxInput("map_log", "Use log scale (Plotly color)", FALSE)
                      )
               ),
               column(9,
                      div(class = "card-panel",
                          h4("🌐 Explore by Map"),
                          tabsetPanel(
                            tabPanel("Plotly Choropleth",
                                     plotlyOutput("map_plotly", height = "520px"),
                                     br(),
                                     DTOutput("map_table")
                            ),
                            tabPanel("Leaflet Map",
                                     leafletOutput("map_leaflet", height = "520px"),
                                     br(),
                                     DTOutput("map_table_leaflet")
                            )
                          )
                      )
               )
             )
    ),
    
    # ==== SUMMARY STATISTICS TAB (new) ====
    tabPanel("Summary Statistics",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Controls"),
                          selectInput("sum_ind", "Indicator",
                                      choices = setNames(indicator_codes, indicator_labels)),
                          selectizeInput("sum_regions", "Region(s)",
                                         choices = sort(unique(na.omit(sdg7$Region))),
                                         multiple = TRUE),
                          selectizeInput("sum_countries", "Country(ies)",
                                         choices = sort(unique(sdg7$Country)),
                                         multiple = TRUE),
                          sliderInput("sum_year", "Year range",
                                      min = year_min, max = year_max,
                                      value = c(max(year_min, year_max-10), year_max),
                                      step = 1, sep = "")
                      )
               ),
               column(9,
                      div(class = "card-panel",
                          h4("🧮 Descriptive Statistics"),
                          DTOutput("sum_table"),
                          br(),
                          h4("📦 Boxplot"),
                          plotlyOutput("sum_box", height = "320px"),
                          br(),
                          h4("📊 Histogram"),
                          plotlyOutput("sum_hist", height = "320px")
                      )
               )
             )
    ),
    
    # ==== RAW DATA TAB ====
    tabPanel("Raw Data",
             fluidRow(
               column(3,
                      div(class = "card-panel",
                          h4("⚙️ Options"),
                          downloadButton("download_filtered", "Download dataset")
                      )
               ),
               column(9,
                      div(class = "card-panel",
                          h4("🗃️ Full Dataset"),
                          DTOutput("raw_table")
                      )
               )
             )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---------- TREND ----------
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
    # Sort legend: countries first, then regions
    dat %>% arrange(factor(Type, levels = c("Country", "Region")), Name)
  })
  
  output$trendplot <- renderPlotly({
    dat <- trend_data()
    validate(need(nrow(dat) > 0, "No data available."))
    
    p <- ggplot(dat, aes(x = Year, y = Value, group = Name)) +
      geom_line(aes(color = Name, linetype = Type), size = 1.3, show.legend = TRUE) +
      geom_point(aes(color = Name), size = 2, show.legend = FALSE) +
      scale_linetype_manual(values = c("Country" = "solid", "Region" = "dashed")) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank()) +
      labs(title = paste(indicator_labels[input$trend_ind], "over Time"),
           x = "Year", y = NULL)
    
    plt <- ggplotly(p)
    for (i in seq_along(plt$x$data)) {
      # clean duplicate/verbose legend text
      plt$x$data[[i]]$name <- gsub("\\(Country,|\\(Region,|\\)|,1", "", plt$x$data[[i]]$name)
      # show legend only for lines
      if (plt$x$data[[i]]$mode == "markers") plt$x$data[[i]]$showlegend <- FALSE
    }
    plt %>% layout(legend = list(title = list(text = "")))
  })
  
  output$tbl_trend <- renderDT({
    datatable(trend_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------- BOXPLOT ----------
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
  
  # ---------- REGRESSION ----------
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
  
  # ---------- MAPS ----------
  map_data <- reactive({
    req(input$map_ind, input$map_year)
    sdg7 %>%
      filter(Year == input$map_year) %>%
      select(Country, ISO3, Region, longitude, latitude,
             Value = all_of(input$map_ind)) %>%
      mutate(Value = as.numeric(Value))
  })
  
  # Plotly choropleth (ISO3-based)
  output$map_plotly <- renderPlotly({
    dat <- map_data()
    validate(need(nrow(dat) > 0, "No data available for that year."))
    
    zvals <- dat$Value
    if (input$map_log) {
      # keep zeros/negatives as NA for log scaling
      zvals <- ifelse(zvals > 0, log10(zvals), NA_real_)
      ztitle <- paste0("log10(", indicator_labels[input$map_ind], ")")
    } else {
      ztitle <- indicator_labels[input$map_ind]
    }
    
    # Basic choropleth with colorbar; countries without data appear as default geo background
    plt <- plot_ly(
      data = dat,
      type = "choropleth",
      locations = ~ISO3,
      locationmode = "ISO-3",
      z = ~zvals,
      text = ~paste0(Country, "<br>", ztitle, ": ", ifelse(is.na(zvals), "N/A",
                                                           if (input$map_log) round(zvals, 3) else comma(Value))),
      hoverinfo = "text",
      colorscale = "Viridis",
      reversescale = FALSE,
      marker = list(line = list(color = "white", width = 0.3)),
      showscale = TRUE,
      colorbar = list(title = ztitle)
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = toRGB("gray80"),
          projection = list(type = "equirectangular"),
          bgcolor = toRGB("white")
        ),
        margin = list(l = 0, r = 0, t = 10, b = 0)
      )
    plt
  })
  
  output$map_table <- renderDT({
    dat <- map_data() %>%
      arrange(desc(!is.na(Value)), Country)
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Leaflet bubbles (using capital coords)
  output$map_leaflet <- renderLeaflet({
    dat <- map_data()
    validate(need(nrow(dat) > 0, "No data available for that year."))
    
    # color palette for values (handle NAs)
    pal <- colorNumeric("YlGnBu", domain = dat$Value, na.color = "#d9d9d9")
    
    # radius scaling (robust to outliers)
    rng <- range(dat$Value, na.rm = TRUE)
    scale_radius <- function(x) {
      if (all(is.na(x))) return(rep(6, length(x)))
      rescaled <- rescale(x, to = c(6, 18), from = rng)
      ifelse(is.na(rescaled), 6, rescaled)
    }
    
    leaflet(dat, options = leafletOptions(minZoom = 1, worldCopyJump = TRUE)) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~scale_radius(Value),
        stroke = TRUE, weight = 1, color = "#444444",
        fillColor = ~pal(Value), fillOpacity = 0.85,
        label = ~paste0(Country, ": ", ifelse(is.na(Value), "N/A", signif(Value, 4))),
        popup = ~paste0("<b>", Country, "</b><br>",
                        indicator_labels[input$map_ind], "<br>",
                        "Year: ", input$map_year, "<br>",
                        "Value: ", ifelse(is.na(Value), "N/A", signif(Value, 6)))
      ) %>%
      addLegend(
        position = "bottomright", pal = pal, values = ~Value,
        title = indicator_labels[input$map_ind], opacity = 0.9, labFormat = labelFormat()
      )
  })
  
  output$map_table_leaflet <- renderDT({
    dat <- map_data() %>%
      arrange(desc(!is.na(Value)), Country)
    datatable(dat, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------- SUMMARY STATISTICS ----------
  sum_data <- reactive({
    req(input$sum_ind)
    dat <- sdg7 %>%
      filter(Year >= input$sum_year[1], Year <= input$sum_year[2]) %>%
      select(Country, Region, Year, Value = all_of(input$sum_ind)) %>%
      drop_na(Value)
    
    if (length(input$sum_regions) > 0) {
      dat <- dat %>% filter(Region %in% input$sum_regions)
    }
    if (length(input$sum_countries) > 0) {
      dat <- dat %>% filter(Country %in% input$sum_countries)
    }
    dat
  })
  
  output$sum_table <- renderDT({
    dat <- sum_data()
    validate(need(nrow(dat) > 0, "No data in the selected filters."))
    
    stats <- dat %>%
      summarise(
        n = n(),
        missing = sum(is.na(Value)),
        mean = mean(Value, na.rm = TRUE),
        median = median(Value, na.rm = TRUE),
        sd = sd(Value, na.rm = TRUE),
        min = min(Value, na.rm = TRUE),
        max = max(Value, na.rm = TRUE),
        IQR = IQR(Value, na.rm = TRUE)
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    
    datatable(stats, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$sum_box <- renderPlotly({
    dat <- sum_data()
    validate(need(nrow(dat) > 0, "No data in the selected filters."))
    
    p <- ggplot(dat, aes(x = factor(Year), y = Value)) +
      geom_boxplot(fill = "#3498db", alpha = 0.7, outlier.shape = 16) +
      theme_minimal(base_size = 14) +
      labs(x = "Year", y = indicator_labels[input$sum_ind])
    
    ggplotly(p)
  })
  
  output$sum_hist <- renderPlotly({
    dat <- sum_data()
    validate(need(nrow(dat) > 0, "No data in the selected filters."))
    
    p <- ggplot(dat, aes(x = Value)) +
      geom_histogram(bins = 30, alpha = 0.85) +
      theme_minimal(base_size = 14) +
      labs(x = indicator_labels[input$sum_ind], y = "Count")
    
    ggplotly(p)
  })
  
  # ---------- RAW DATA ----------
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
