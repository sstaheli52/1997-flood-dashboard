library(shiny)
library(ggplot2)
library(readr)
library(plotly)
library(terra)
library(sf)
library(viridis)
library(leaflet)
library(dplyr)
library(bslib)

#---Loading Data---
# Daily global evaporation values that contribted to CZ precip
et_data <- read_csv("data_prep/E2P_EPs_timeseries.csv")
et_data$day <- 1:nrow(et_data) # adds a day column for July 1997

# Raster stack of daily E2P_EPs values for spatial time lapse map
et_raster_stack <- readRDS("data_prep/E2P_EPs_cropped.rds")

# Shapefile of European basins
basins <- st_read("data/dissolved_basins/dissolved_hydrobasins_eu.shp")

# Daily precip data over CZ (MSWEP data)
precip_data <- read_csv("data_prep/MSWEP_CZ_timeseries.csv")

# Spatial data for the leaflet plot (July 7 & 18) including per basin E2P_EPs
basin_map_data <- readRDS("data_prep/basin_map_data_evap.rds")

# Difference between 1997 and climatology over two windows
diffs <- readRDS("data_prep/basin_diff_data.rds")

# Exceedance Probability (rarity of July 1997 precip vs climatology)
exceed <- readRDS("data_prep/basin_exceed_data.rds")

# Rolling Spearman Correlation between E2P_EPs
corr <- readRDS("data_prep/basin_corr_data.rds")


#---UI---
ui <- fluidPage(
  
  # Theme setting
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Open Sans")
  ),
  
  # Page styling
  tags$head(
    tags$style(HTML("
      h1 {
        text-align: center;
        margin-bottom: 30px;
      }
      .well {
        max-width: 300px;
        margin-left: auto;
        margin-right: auto;
      }
.section-header {
  background-color: #d4efdf;
  padding: 20px;
  border-radius: 8px;
  margin-bottom: 30px;
}

.section-timeseries {
  background-color: #a9dfbf;
  padding: 20px;
  border-radius: 8px;
  margin-bottom: 30px;
}

.section-map {
  background-color: #a2d9ce;
  padding: 20px;
  border-radius: 8px;
  margin-bottom: 30px;
}

.section-basins {
  background-color: #d0ece7;
  padding: 20px;
  border-radius: 8px;
  margin-bottom: 30px;
}
    ")),
    
    # Sticky top bar styling
    tags$style(HTML("
  .top-bar {
    position: sticky;
    top: 0;
    z-index: 999;
    background-color: #f8f9fa;
    padding: 4px 10px;
    border-bottom: 1px solid #ccc;
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 20px;
  }
  #display_opts label.control-label {
    display: none;
  }
  .form-check-inline {
    margin-right: 10px;
  }
"))
    
  ),
  
  # Sticky top bar's checkboxes
  div(class = "top-bar",
      checkboxGroupInput("display_opts", "Components:",
                         choices = c("Timeseries" = "timeseries",
                                     "Time Lapse Map" = "map_plot",
                                     "Basin Map + Info" = "basins"),
                         selected = c("timeseries", "map_plot", "basins"),
                         inline = TRUE
      )
  ),
  
  # Dashboard title and description
  div(class = "section-header",
      h1("1997 Czech Flood Dashboard", style = "text-align: center;"),
      p("This dashboard explores the evaporation sources and precipitation patterns during the July 1997 Central European flood, focusing on contributions to rainfall in the Czech Republic. The peak precipitation days in the Czech Republic occured in the periods of July 4-8 and 14-23.",
        style = "text-align: center;")
  ),
  
  # Main content
  fluidRow(
    column(
      12,
      uiOutput("main_panels")
    )
  )
)


server <- function(input, output) {
  
  # Perpare spatial basin map data based on the selected day
  filtered_map_data <- reactive({
    req(input$selected_day) # To make sure input is available
    
    # Get the basin map data just for the chosen day
    map_data <- basin_map_data %>% filter(day == input$selected_day)
    
    # Get the extra data to show later (differences and correlations)
    diffs_31 <- diffs %>% filter(window == "31-day")
    diffs_14 <- diffs %>% filter(window == "14-day")
    
    corr_30 <- corr %>% filter(window == "30-day")
    corr_15 <- corr %>% filter(window == "15-day")
    
    # Join everything together by basin name
    map_data <- map_data %>%
      left_join(diffs_31, by = "basin_name") %>%
      rename(abs_diff_31 = abs_diff, rel_diff_31 = rel_diff) %>%
      left_join(diffs_14, by = "basin_name") %>%
      rename(abs_diff_14 = abs_diff, rel_diff_14 = rel_diff) %>%
      left_join(corr_30, by = "basin_name") %>%
      rename(rho_30 = rho, sig_dates_30 = sig_dates, nonsig_dates_30 = nonsig_dates) %>%
      left_join(corr_15, by = "basin_name") %>%
      rename(rho_15 = rho, sig_dates_15 = sig_dates, nonsig_dates_15 = nonsig_dates)
    
    return(map_data)
  })
  
  # Save which basin the user clicks on
  selected_basin_info <- reactiveVal(NULL)
  
  observeEvent(input$basin_map_shape_click, {
    click <- input$basin_map_shape_click
    selected_basin <- click$id  # This is the basin_name from layerId
    
    # Grab the info for just that basin
    basin_data <- filtered_map_data() %>% 
      filter(basin_name == selected_basin)
    
    # Save it for display in the side panel
    selected_basin_info(basin_data)
  })
  
  output$timeseries <- renderPlotly({
    # Merge evaporation + precip data by day
    df <- merge(et_data, precip_data, by = "day")

    # Create labels for tooltips
    df$evap_label <- paste0("Global Mean Evaporation: ", round(df$mean, 4), " mm/day")
    df$precip_label <- paste0("CZ Mean Precipitation: ", round(df$precip, 4), " mm/day")
    
    # Start building the plot with basic labels and styling
    p <- ggplot(df, aes(x = day)) +
      labs(
        title = "Mean Evaporation (Global) vs Mean Precipitation (CZ) – July 1997",
        x = "Day of July 1997",
        y = "Value (mm/day)",
        caption = "E2P_EPs = global evaporation contributing to CZ precipitation"
      ) +
      scale_x_continuous(breaks = 1:31) +
      theme_minimal()
    
    # Add evaporation line and points if the checkbox is clicked, rounded and values clarified
    if ("Evaporation" %in% input$ts_vars) {
      p <- p +
        geom_line(data = df, aes(x = day, y = mean, text = evap_label, group = 1), color = "#ff9a4f", size = 1.2) +
        geom_point(data = df, aes(x = day, y = mean, text = evap_label), color = "black", size = 2)
    }
    
    # Add precipitation line and points if the checkbox is clicked, rounded and values clarified
    if ("Precipitation" %in% input$ts_vars) {
      p <- p +
        geom_line(data = df, aes(x = day, y = precip, text = precip_label, group = 1), color = "#75a0ea", size = 1.2) +
        geom_point(data = df, aes(x = day, y = precip, text = precip_label), color = "black", size = 2)
    }
    
    # Turn it into an interactive plot
    ggplotly(p, tooltip = "text")
  })
  
  output$basin_map <- renderLeaflet({
    # Grab the map data for the currently selected day
    map_data <- filtered_map_data()
    
    # If there's no data or no geometry column, stop and return nothing
    if (nrow(map_data) == 0 || !("geometry" %in% colnames(map_data))) {
      return(NULL)
    }
    
    # Create a color scale for the basin values
    pal <- colorNumeric(
      palette = "viridis",
      domain = map_data$E2P_EPs_1997,
      na.color = "transparent",
      reverse = FALSE
    )
    
    # Build the leaflet map for each basin
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(E2P_EPs_1997),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = ~ifelse(is.na(E2P_EPs_1997) | E2P_EPs_1997 == 0, 0.1, 0.7), # transparency for 0s or NAs
        layerId = ~basin_name, # used for clickin on basins
        label = ~basin_name, # hover label
        # hover effects
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = map_data$E2P_EPs_1997,
                title = "E2P_EPs (mm)",
                opacity = 1)
  })
  
  output$map_plot <- renderPlot({
    # Make sure a day is selected before anything further happens
    req(input$day)
    
    # Get the raster layer for the selected day
    raster_day <- terra::subset(et_raster_stack, input$day)
    
    # If there is nothing, don't plot
    if (is.null(raster_day)) return(NULL)
    
    # Replace zeros and NAs with NA to clean up the visualization
    vals <- values(raster_day)
    if (!is.null(vals)) {
      values(raster_day)[is.na(vals) | vals == 0] <- NA
    }
    
    # Plot the raster map (evaporation)
    plot(raster_day,
         main = paste("E2P_EPs - Day", input$day),
         col = viridis::plasma(100),
         maxpixels = 1e6,
         axes = FALSE,
         mar = c(3, 3, 2, 2),
         legend.args = list(text = "E2P_EPs", side = 4, line = 2.5))
    
    # Adding the basin outlines on top of the map
    plot(basins$geometry, add = TRUE, border = "black", lwd = 0.6)
    
    # Draw a border around the full map
    e <- ext(raster_day)
    rect(e[1], e[3], e[2], e[4], border = "black", lwd = 1.5)
  })
  
  
  output$basin_info <- renderUI({
    # Grab the info for the basin the user clicks
    basin <- selected_basin_info()
    
    # If nothing is selected, show this prompt
    if (is.null(basin) || nrow(basin) == 0) {
      return("Click on a basin to see its details.")
    }
    
    # Only show exceedance data if it's July 7
    exceedance_table <- NULL
    if (input$selected_day == "July 7") {
      # Filter the exceedance categories
      ex_14 <- exceed %>%
        filter(basin_name == basin$basin_name, window == "14-day") %>%
        select(category, fraction)
      
      ex_31 <- exceed %>%
        filter(basin_name == basin$basin_name, window == "31-day") %>%
        select(category, fraction)
      
      # function to turn each exceedance table into HTML
      format_table <- function(ex_df, label) {
        if (nrow(ex_df) == 0) return("")
        
        paste0(
          "<b>Exceedance (", label, " window):</b><br>",
          "<table style='width:60%; border-collapse: collapse; margin-top: 5px;'>",
          "<tr><th style='text-align:left;'>Category</th><th style='text-align:right;'>Fraction</th></tr>",
          paste0(
            apply(ex_df, 1, function(row) {
              paste0("<tr><td>", row["category"], "</td><td style='text-align:right;'>", round(as.numeric(row["fraction"]), 3), "</td></tr>")
            }),
            collapse = ""
          ),
          "</table><br>"
        )
      }
      
      # Combine both exceedance tables into one block
      exceedance_table <- paste0(
        format_table(ex_14, "14-day"),
        format_table(ex_31, "31-day")
      )
      
      } else {
      exceedance_table <- "<i>Exceedance data is only available for July 7.</i><br><br>"
      }
    
    # Display all the basin info
HTML(paste0(
    "<b>Basin:</b> ", basin$basin_name, "<br>",
    "<b>E2P_EPs Total:</b> ", round(basin$E2P_EPs_1997, 2), " mm<br>",
    "<b>Area:</b> ", format(round(basin$area_km2, 0), big.mark = ","), " km²<br>",
    "<b>E2P_EPs (mm/day):</b> ", signif(basin$E2P_EPs_mm, 4), "<br><br>",

    "<b>Difference (31-day window):</b>",
    "<ul>",
    "<li>Absolute: ", round(basin$abs_diff_31, 2), " mm</li>",
    "<li>Relative: ", round(basin$rel_diff_31, 2), " %</li>",
    "</ul><br>",

    "<b>Difference (14-day window):</b>",
    "<ul>",
    "<li>Absolute: ", round(basin$abs_diff_14, 2), " mm</li>",
    "<li>Relative: ", round(basin$rel_diff_14, 2), " %</li>",
    "</ul><br>",

    exceedance_table,
    "<b>Correlation (30-day window):</b>",
    "<ul>",
    "<li>Rho: ", round(basin$rho_30, 2), "</li>",
    "<li>Significant Dates: ", ifelse(!is.na(basin$sig_dates_30), basin$sig_dates_30, "None"), "</li>",
    "<li>Zero Dates: ", ifelse(!is.na(basin$nonsig_dates_30), basin$nonsig_dates_30, "None"), "</li>",
    "</ul><br>",

    "<b>Correlation (15-day window):</b>",
    "<ul>",
    "<li>Rho: ", round(basin$rho_15, 2), "</li>",
    "<li>Significant Dates: ", ifelse(!is.na(basin$sig_dates_15), basin$sig_dates_15, "None"), "</li>",
    "<li>Zero Dates: ", ifelse(!is.na(basin$nonsig_dates_15), basin$nonsig_dates_15, "None"), "</li>",
    "</ul>"
  ))
})
  
  output$main_panels <- renderUI({
    # Start with an empty list and add sections based on user selection
    panels <- list()
    
    # Timeseries section
    if ("timeseries" %in% input$display_opts) {
      panels <- append(panels, list(
        div(class = "section-timeseries", # green box background
            div(style = "text-align: center;",
                plotlyOutput("timeseries", height = "300px"), # the actual timeseries chart
                div(
                  style = "margin-top: 10px;",
                  checkboxGroupInput("ts_vars", NULL, # Inline checkboxes of evap and precip
                                     choices = c("Evaporation", "Precipitation"),
                                     selected = c("Evaporation", "Precipitation"),
                                     inline = TRUE)
                )
            )
        )
      ))
    }
    
    # Time Lapse Map section
    if ("map_plot" %in% input$display_opts) {
      panels <- append(panels, list(
        div(class = "section-map", # blue background
            fluidRow(
              column(
                width = 3,
                div(
                  sliderInput("day", "Day of July 1997:", # Slider to pick the day
                              min = 1, max = 31, value = 1,
                              animate = animationOptions(interval = 1000)),
                  p("This map shows where evaporation contributed to CZ precipitation (E2P_EPs) throughout July over European river basins."),
                  style = "padding-right: 10px;"
                )
              ),
              column(
                width = 9,
                plotOutput("map_plot", height = "400px") # Static raster plot
              )
            )
        )
      ))
    }
    
    # Basin Map + Info section
    if ("basins" %in% input$display_opts) {
      panels <- append(panels, list(
        div(class = "section-basins",
            fluidRow(
              column(
                width = 8,
                leafletOutput("basin_map", height = 400) # interactive map
              ),
              column(
                width = 4,
                p("This map displays the total evaporation (E2P_EPs) contributed by the river basins on the peak flooding days of July 7th and July 18th. Due to the flood caused by a dual-VB cyclone, the contributing basins vary significantly between the two phases.",
                  tags$br(),
                  tags$br(),
                  "Difference = Comparison between July 1997 and climatology (1980-2020) over two windows.",
                  tags$br(),
                  "Exceedance = The lowest categories of exceedance probability mean that precipitation is extremely rare compared to climatology (ex. <5%). The right column shows the fraction of the basin that displayed that category.",
                  tags$br(),
                  "Correlation = Between evaporation (E2P_EPs) and precipition (MSWEP) data accumulated from 15 and 30 days before each day in July."),
                selectInput("selected_day", "Select Basin Day:",
                            choices = c("July 7", "July 18"),
                            selected = "July 7"),
                div(style = "margin-top: 10px;",
                    uiOutput("basin_info")) # Shows data once a basin is clicked
              )
            )
        )
      ))
    }
    # Combine all the selections and return
    fluidRow(column(12, tagList(panels)))
  })
}

# Launches the app by taking the ui and server logic
shinyApp(ui = ui, server = server)




