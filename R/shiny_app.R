run_glacier_app <- function(
    glacier_map_data_proj,
    glacier_map_data_leaflet,
    glacier_plot_data,
    glacier_summary_data,
    temperature_data
) {
  
  library(shiny)
  library(leaflet)
  library(ggplot2)
  library(dplyr)
  library(sf)
  library(bslib)
  library(later)
  library(htmltools)
  
  # Glacier-inspired color palette
  ice_blue <- "#7DD3FC"
  glacier_blue <- "#2A9FD6"
  deep_blue <- "#0B253A"
  snow_white <- "#F8FBFF"
  light_ice <- "#EAF7FF"
  debris_brown <- "#8B6F47"
  debris_dark <- "#5C4630"
  heat_orange <- "#F97316"
  
  # =========================================================
  # Data
  # =========================================================
  
  # Keep all glaciers available in the data
  glacier_choices <- glacier_map_data_proj |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(glac_name)) |>
    dplyr::distinct(glac_name) |>
    dplyr::arrange(glac_name) |>
    dplyr::pull(glac_name)
  
  default_glacier <- "Rhonegletscher"
  
  if (!default_glacier %in% glacier_choices) {
    default_glacier <- glacier_choices[1]
  }
  
  # Latest available ice-cover mask for each glacier
  latest_glacier_dates <- glacier_map_data_proj |>
    sf::st_drop_geometry() |>
    dplyr::filter(line_type == "glac_bound") |>
    dplyr::group_by(glac_id, glac_name) |>
    dplyr::summarise(
      latest_year = max(year, na.rm = TRUE),
      .groups = "drop"
    )
  
  latest_glaciers_leaflet <- glacier_map_data_proj |>
    dplyr::filter(line_type == "glac_bound") |>
    dplyr::inner_join(
      latest_glacier_dates,
      by = c("glac_id", "glac_name")
    ) |>
    dplyr::filter(year == latest_year) |>
    dplyr::select(-latest_year) |>
    sf::st_make_valid() |>
    dplyr::group_by(glac_id, glac_name, src_date, year) |>
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) |>
    sf::st_cast("MULTIPOLYGON", warn = FALSE) |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    sf::st_transform(4326)
  
  # =========================================================
  # UI
  # =========================================================
  
  ui <- fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = glacier_blue,
      base_font = bslib::font_google("Source Sans 3"),
      bg = snow_white,
      fg = deep_blue
    ),
    
    tags$head(
      tags$style(HTML("
        body {
          background: linear-gradient(180deg, #F8FBFF 0%, #EAF7FF 100%);
          color: #0B253A;
        }

        .app-hero {
          background: linear-gradient(135deg, #0B253A 0%, #2A9FD6 55%, #7DD3FC 100%);
          color: white;
          padding: 26px 30px;
          border-radius: 18px;
          margin-bottom: 22px;
          box-shadow: 0 6px 18px rgba(11, 37, 58, 0.18);
        }

        .app-hero h1 {
          margin: 0;
          font-weight: 800;
          letter-spacing: 0.3px;
        }

        .app-hero p {
          margin: 8px 0 0 0;
          font-size: 16px;
          opacity: 0.92;
        }

        .well {
          background-color: rgba(255, 255, 255, 0.92);
          border: 1px solid #CDEEFF;
          border-radius: 16px;
          box-shadow: 0 4px 14px rgba(42, 159, 214, 0.12);
        }

        .btn {
          border-radius: 10px;
          font-weight: 600;
        }

        .btn-default {
          background-color: #EAF7FF;
          border-color: #B7E7FA;
          color: #0B253A;
        }

        .btn-default:hover {
          background-color: #D8F1FF;
          border-color: #7DD3FC;
        }

        .selectize-input, .form-control {
          border-radius: 10px;
          border-color: #B7E7FA;
        }

        .irs--shiny .irs-bar,
        .irs--shiny .irs-single {
          background: #2A9FD6;
          border-color: #2A9FD6;
        }

        .irs--shiny .irs-handle {
          border-color: #2A9FD6;
        }

        .glacier-card {
          border: 1px solid #CDEEFF;
          border-radius: 14px;
          padding: 12px;
          background-color: rgba(255,255,255,0.94);
          box-shadow: 0 4px 12px rgba(42, 159, 214, 0.12);
        }

        .glacier-card h5 {
          color: #0B253A;
          font-weight: 800;
          margin-top: 0;
        }

        .leaflet-container {
          border-radius: 12px;
        }

        .click-hint {
          font-size: 12px;
          color: #587083;
          margin-top: -4px;
          margin-bottom: 8px;
        }

        .info-grid {
          display: grid;
          grid-template-columns: 1fr;
          gap: 8px;
        }

        .info-box {
          background: linear-gradient(180deg, #FFFFFF 0%, #F3FAFF 100%);
          border: 1px solid #CDEEFF;
          border-radius: 12px;
          padding: 9px 10px;
        }

        .info-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 6px;
        }
          
        .info-box {
          background: linear-gradient(180deg, #FFFFFF 0%, #F3FAFF 100%);
          border: 1px solid #CDEEFF;
          border-radius: 10px;
          padding: 6px 8px;
          min-height: 58px;
        }

        .info-box-wide {
          grid-column: span 2;
        }
          
        .info-label {
          font-size: 10.5px;
          color: #5B7184;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.2px;
          line-height: 1.1;
        }

        .info-value {
          font-size: 14px;
          color: #0B253A;
          font-weight: 800;
          margin-top: 3px;
          line-height: 1.15;
        }

        .map-year-label {
          background: rgba(255, 255, 255, 0.92);
          border: 2px solid #7DD3FC;
          border-radius: 14px;
          padding: 10px 18px;
          margin: 0;
          box-shadow: 0 4px 14px rgba(11, 37, 58, 0.25);
          color: #0B253A;
          text-align: center;
          min-width: 120px;
        }

        .map-year-main {
          font-size: 38px;
          font-weight: 900;
          line-height: 1;
        }

        .map-wrapper {
          position: relative;
        }

        .map-refocus-button {
          position: absolute;
          top: 210px;
          left: 20px;
          right: auto;
          z-index: 1000;
          background-color: rgba(255, 255, 255, 0.92);
          border: 1.5px solid #7DD3FC;
          color: #0B253A;
          border-radius: 10px;
          padding: 6px 10px;
          font-size: 12px;
          font-weight: 800;
          box-shadow: 0 3px 10px rgba(11, 37, 58, 0.22);
        }

        .map-refocus-button:hover {
          background-color: #EAF7FF;
          border-color: #2A9FD6;
        }
      "))
    ),
    
    tags$div(
      class = "app-hero",
      tags$h1("Swiss glacier retreat explorer"),
      tags$p("Explore glacier ice-cover evolution, debris cover and regional summer temperature anomalies.")
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = "glacier_name",
          label = "Select a glacier",
          choices = glacier_choices,
          selected = default_glacier,
          options = list(
            maxOptions = length(glacier_choices),
            placeholder = "Type a glacier name..."
          )
        ),
        
        tags$div(
          class = "click-hint",
          "You can also select a glacier by clicking it on the overview map below."
        ),
        
        uiOutput("year_ui"),
        
        fluidRow(
          column(4, actionButton("prev_year", "Previous")),
          column(4, actionButton("play_pause", "Play")),
          column(4, actionButton("next_year", "Next"))
        ),
        
        br(),
        
        checkboxInput(
          inputId = "show_ice",
          label = "Show ice cover",
          value = TRUE
        ),
        
        checkboxInput(
          inputId = "show_debris",
          label = "Show debris cover",
          value = TRUE
        ),
        
        checkboxInput(
          inputId = "show_temperature",
          label = "Compare with summer temperature anomaly",
          value = FALSE
        ),
        
        hr(),
        
        tags$div(
          class = "glacier-card",
          tags$h5("Glacier information"),
          uiOutput("glacier_info")
        ),
        
        hr(),
        
        tags$div(
          class = "glacier-card",
          tags$h5("Latest glacier masks overview"),
          tags$div(
            class = "click-hint",
            "Click on a glacier to select it."
          ),
          leafletOutput("overview_map", height = 260, width = "100%")
        )
      ),
      
      mainPanel(
        tags$div(
          class = "glacier-card map-wrapper",
          leafletOutput("map", height = 650),
          actionButton(
            inputId = "refocus_map",
            label = "Refocus",
            class = "map-refocus-button"
          )
        ),
        
        br(),
        
        tags$div(
          class = "glacier-card",
          plotOutput("area_plot", height = 300)
        ),
        
        conditionalPanel(
          condition = "input.show_temperature == true",
          br(),
          tags$div(
            class = "glacier-card",
            plotOutput("temperature_plot", height = 330)
          )
        )
      )
    )
  )
  
  # =========================================================
  # Server
  # =========================================================
  
  server <- function(input, output, session) {
    
    playing <- reactiveVal(FALSE)
    current_year <- reactiveVal(NULL)
    
    # -------------------------------------------------------
    # Small formatting helpers
    # -------------------------------------------------------
    
    fmt <- function(x, digits = 2) {
      ifelse(
        is.na(x),
        "NA",
        format(round(x, digits), big.mark = " ", scientific = FALSE)
      )
    }
    
    info_box <- function(label, value, wide = FALSE) {
      tags$div(
        class = if (wide) "info-box info-box-wide" else "info-box",
        tags$div(class = "info-label", label),
        tags$div(class = "info-value", value)
      )
    }
    
    # -------------------------------------------------------
    # Selected glacier data
    # -------------------------------------------------------
    
    glacier_selected_leaflet <- reactive({
      req(input$glacier_name)
      
      glacier_map_data_leaflet |>
        dplyr::filter(glac_name == input$glacier_name)
    })
    
    glacier_selected_plot <- reactive({
      req(input$glacier_name)
      
      glacier_plot_data |>
        dplyr::filter(glac_name == input$glacier_name) |>
        dplyr::arrange(year)
    })
    
    # -------------------------------------------------------
    # Available years for selected glacier
    # -------------------------------------------------------
    
    available_years <- reactive({
      x <- glacier_selected_leaflet()
      req(nrow(x) > 0)
      
      x |>
        sf::st_drop_geometry() |>
        dplyr::distinct(year) |>
        dplyr::arrange(year) |>
        dplyr::pull(year)
    })
    
    closest_available_year <- function(year_value, yrs) {
      yrs[which.min(abs(yrs - year_value))]
    }
    
    # -------------------------------------------------------
    # Dynamic slider
    # -------------------------------------------------------
    
    output$year_ui <- renderUI({
      yrs <- available_years()
      req(length(yrs) > 0)
      
      sliderInput(
        inputId = "year_selected",
        label = "Year",
        min = min(yrs),
        max = max(yrs),
        value = min(yrs),
        step = 1,
        sep = ""
      )
    })
    
    # -------------------------------------------------------
    # Reset when changing glacier
    # -------------------------------------------------------
    
    observeEvent(input$glacier_name, {
      yrs <- available_years()
      req(length(yrs) > 0)
      
      first_year <- min(yrs)
      
      current_year(first_year)
      playing(FALSE)
      
      updateActionButton(
        session = session,
        inputId = "play_pause",
        label = "Play"
      )
      
      updateSliderInput(
        session = session,
        inputId = "year_selected",
        min = min(yrs),
        max = max(yrs),
        value = first_year
      )
    }, ignoreInit = FALSE)
    
    # -------------------------------------------------------
    # Select glacier by clicking on the overview map
    # -------------------------------------------------------
    
    observeEvent(input$overview_map_shape_click, {
      clicked <- input$overview_map_shape_click
      req(clicked$id)
      
      clicked_glacier <- clicked$id
      
      if (clicked_glacier %in% glacier_choices) {
        updateSelectInput(
          session = session,
          inputId = "glacier_name",
          selected = clicked_glacier
        )
      }
    })
    
    # -------------------------------------------------------
    # Sync slider -> current_year
    # -------------------------------------------------------
    
    observeEvent(input$year_selected, {
      req(!is.null(input$year_selected))
      
      yrs <- available_years()
      req(length(yrs) > 0)
      
      selected_year <- closest_available_year(input$year_selected, yrs)
      current_year(selected_year)
      
      if (!identical(as.integer(input$year_selected), as.integer(selected_year))) {
        updateSliderInput(
          session = session,
          inputId = "year_selected",
          value = selected_year
        )
      }
    }, ignoreInit = TRUE)
    
    # -------------------------------------------------------
    # Navigation buttons
    # -------------------------------------------------------
    
    observeEvent(input$next_year, {
      yrs <- available_years()
      req(length(yrs) > 0)
      req(!is.null(current_year()))
      
      i <- match(current_year(), yrs)
      if (is.na(i)) i <- 1
      
      next_yr <- if (i < length(yrs)) yrs[i + 1] else yrs[1]
      
      current_year(next_yr)
      updateSliderInput(session, "year_selected", value = next_yr)
    })
    
    observeEvent(input$prev_year, {
      yrs <- available_years()
      req(length(yrs) > 0)
      req(!is.null(current_year()))
      
      i <- match(current_year(), yrs)
      if (is.na(i)) i <- 1
      
      prev_yr <- if (i > 1) yrs[i - 1] else yrs[length(yrs)]
      
      current_year(prev_yr)
      updateSliderInput(session, "year_selected", value = prev_yr)
    })
    
    # -------------------------------------------------------
    # Animation
    # -------------------------------------------------------
    
    animate_step <- function() {
      if (!isTRUE(isolate(playing()))) return(NULL)
      
      yrs <- isolate(available_years())
      yr  <- isolate(current_year())
      
      req(length(yrs) > 0)
      req(!is.null(yr))
      
      i <- match(yr, yrs)
      if (is.na(i)) i <- 1
      
      next_yr <- if (i < length(yrs)) yrs[i + 1] else yrs[1]
      
      current_year(next_yr)
      updateSliderInput(session, "year_selected", value = next_yr)
      
      later::later(animate_step, delay = 1.0)
    }
    
    observeEvent(input$play_pause, {
      new_state <- !playing()
      playing(new_state)
      
      updateActionButton(
        session = session,
        inputId = "play_pause",
        label = if (new_state) "Pause" else "Play"
      )
      
      if (new_state) {
        later::later(animate_step, delay = 1.0)
      }
    })
    
    # -------------------------------------------------------
    # Current year spatial data
    # -------------------------------------------------------
    
    glacier_year_leaflet <- reactive({
      req(!is.null(current_year()))
      
      glacier_selected_leaflet() |>
        dplyr::filter(year == current_year())
    })
    
    # -------------------------------------------------------
    # Initial map
    # -------------------------------------------------------
    
    output$map <- renderLeaflet({
      x <- glacier_selected_leaflet()
      req(nrow(x) > 0)
      
      bb <- sf::st_bbox(x)
      
      leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery) |>
        fitBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        ) |>
        addScaleBar(position = "bottomleft") |>
        addControl(
          html = HTML(
            "<div style='background: rgba(255,255,255,0.85); 
                         padding: 6px 8px; 
                         border-radius: 4px; 
                         text-align:center; 
                         font-weight:bold;'>
               N<br>↑
             </div>"
          ),
          position = "topright"
        )
    })
    
    # -------------------------------------------------------
    # Overview map with latest mask of each glacier
    # -------------------------------------------------------
    
    output$overview_map <- renderLeaflet({
      req(nrow(latest_glaciers_leaflet) > 0)
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles(providers$Esri.WorldImagery) |>
        setView(lng = 8.2, lat = 46.6, zoom = 7) |>
        addPolygons(
          data = latest_glaciers_leaflet,
          layerId = ~glac_name,
          group = "latest_glaciers",
          fillColor = ice_blue,
          fillOpacity = 0.38,
          color = glacier_blue,
          weight = 0.5,
          label = ~glac_name,
          highlightOptions = highlightOptions(
            weight = 2,
            color = deep_blue,
            fillOpacity = 0.55,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>", glac_name, "</b>",
            "<br>Latest mask date: ", src_date,
            "<br>Year: ", year,
            "<br><i>Click to select this glacier</i>"
          )
        )
    })
    
    # Highlight selected glacier in overview map
    observeEvent(input$glacier_name, {
      selected_latest <- latest_glaciers_leaflet |>
        dplyr::filter(glac_name == input$glacier_name)
      
      leafletProxy("overview_map") |>
        clearGroup("selected_glacier")
      
      if (nrow(selected_latest) > 0) {
        leafletProxy("overview_map") |>
          addPolygons(
            data = selected_latest,
            group = "selected_glacier",
            fillColor = heat_orange,
            fillOpacity = 0.55,
            color = heat_orange,
            weight = 1.8,
            popup = ~paste0(
              "<b>", glac_name, "</b>",
              "<br>Latest mask date: ", src_date,
              "<br>Year: ", year
            )
          )
      }
    }, ignoreInit = FALSE)
    
    # -------------------------------------------------------
    # Recenter main map when changing glacier
    # -------------------------------------------------------
    
    observeEvent(input$glacier_name, {
      x <- glacier_selected_leaflet()
      req(nrow(x) > 0)
      
      bb <- sf::st_bbox(x)
      
      leafletProxy("map") |>
        fitBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        )
    })
    
    observeEvent(input$refocus_map, {
      x <- glacier_year_leaflet()
      req(nrow(x) > 0)
      
      bb <- sf::st_bbox(x)
      
      leafletProxy("map") |>
        fitBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        )
    })
    
    # -------------------------------------------------------
    # Glacier information
    # -------------------------------------------------------
    
    output$glacier_info <- renderUI({
      req(input$glacier_name)
      
      info <- glacier_summary_data |>
        dplyr::filter(glac_name == input$glacier_name)
      
      req(nrow(info) > 0)
      
      year_first <- as.numeric(info$year_first[1])
      year_last  <- as.numeric(info$year_last[1])
      date_span_years <- year_last - year_first
      
      area_first_m2 <- as.numeric(info$area_first_m2[1])
      area_last_m2  <- as.numeric(info$area_last_m2[1])
      
      area_loss_total_m2 <- as.numeric(info$area_loss_total_m2[1])
      area_loss_total_pct <- as.numeric(info$area_loss_total_pct[1])
      area_loss_total_football_fields <- as.numeric(info$area_loss_total_football_fields[1])
      
      area_loss_rate_km2_per_year <- ifelse(
        date_span_years > 0,
        (area_loss_total_m2 / 1e6) / date_span_years,
        NA_real_
      )
      
      area_loss_rate_pct_per_year <- ifelse(
        date_span_years > 0,
        area_loss_total_pct / date_span_years,
        NA_real_
      )
      
      football_fields_loss_per_year <- ifelse(
        date_span_years > 0,
        area_loss_total_football_fields / date_span_years,
        NA_real_
      )
      
      tags$div(
        class = "info-grid",
        
        info_box("Observation period", paste0(year_first, " – ", year_last), wide = TRUE),
        
        info_box("Initial area", paste0(fmt(area_first_m2 / 1e6, 2), " km²")),
        info_box("Final area", paste0(fmt(area_last_m2 / 1e6, 2), " km²")),
        
        info_box("Total loss", paste0(fmt(area_loss_total_m2 / 1e6, 2), " km²")),
        info_box("Relative loss", paste0(fmt(area_loss_total_pct, 1), " %")),
        
        info_box("Annual loss", paste0(fmt(area_loss_rate_km2_per_year, 4), " km²/yr")),
        info_box("Annual relative loss", paste0(fmt(area_loss_rate_pct_per_year, 2), " %/yr")),
        
        info_box("Football fields lost", paste0(fmt(area_loss_total_football_fields, 0), " fields")),
        info_box("Football fields / year", paste0(fmt(football_fields_loss_per_year, 1), " fields/yr"))
      )
    })
    
    # -------------------------------------------------------
    # Update main map
    # -------------------------------------------------------
    
    observe({
      req(current_year())
      
      data <- glacier_year_leaflet()
      req(nrow(data) > 0)
      
      bounds <- data |>
        dplyr::filter(line_type == "glac_bound")
      
      debris <- data |>
        dplyr::filter(line_type == "debris_cov")
      
      proxy <- leafletProxy("map") |>
        clearShapes() |>
        clearControls()
      
      proxy <- proxy |>
        addScaleBar(position = "bottomleft") |>
        addControl(
          html = HTML(
            paste0(
              "<div class='map-year-label'>",
              "<div class='map-year-main'>", current_year(), "</div>",
              "</div>"
            )
          ),
          position = "topleft"
        ) |>
        addControl(
          html = HTML(
            "<div style='background: rgba(255,255,255,0.85); 
                   padding: 6px 8px; 
                   border-radius: 4px; 
                   text-align:center; 
                   font-weight:bold;'>
         N<br>↑
       </div>"
          ),
          position = "topright"
        )
      
      legend_colors <- c()
      legend_labels <- c()
      
      if (isTRUE(input$show_ice)) {
        legend_colors <- c(legend_colors, ice_blue)
        legend_labels <- c(legend_labels, "Ice cover")
      }
      
      if (isTRUE(input$show_debris)) {
        legend_colors <- c(legend_colors, debris_brown)
        legend_labels <- c(legend_labels, "Debris cover")
      }
      
      if (length(legend_colors) > 0) {
        proxy <- proxy |>
          addLegend(
            position = "bottomright",
            colors = legend_colors,
            labels = legend_labels,
            title = "Legend"
          )
      }
      
      if (isTRUE(input$show_ice) && nrow(bounds) > 0) {
        proxy |>
          addPolygons(
            data = bounds,
            fillColor = ice_blue,
            fillOpacity = 0.38,
            color = glacier_blue,
            weight = 1.3,
            popup = ~paste0(
              "<b>", glac_name, "</b>",
              "<br>Year: ", year,
              "<br>Ice-cover area: ", round(area_m2 / 1e6, 3), " km²"
            )
          )
      }
      
      if (isTRUE(input$show_debris) && nrow(debris) > 0) {
        proxy |>
          addPolygons(
            data = debris,
            fillColor = debris_brown,
            fillOpacity = 0.42,
            color = debris_dark,
            weight = 1,
            popup = ~paste0(
              "<b>", glac_name, "</b>",
              "<br>Year: ", year,
              "<br>Debris-cover area: ", round(area_m2 / 1e6, 3), " km²"
            )
          )
      }
    })
    
    # -------------------------------------------------------
    # Area graph
    # -------------------------------------------------------
    
    output$area_plot <- renderPlot({
      x <- glacier_selected_plot()
      req(nrow(x) > 0)
      req(!is.null(current_year()))
      
      ggplot(x, aes(year, area_m2 / 1e6)) +
        geom_line(linewidth = 1.1, color = glacier_blue) +
        geom_point(size = 2.5, color = glacier_blue) +
        geom_vline(
          xintercept = current_year(),
          linetype = "dashed",
          color = deep_blue
        ) +
        labs(
          title = paste("Ice-cover evolution of", input$glacier_name),
          subtitle = "Ice-cover area derived from glacier outlines",
          x = "Year",
          y = "Ice-cover area (km²)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 15, color = deep_blue),
          plot.subtitle = element_text(color = "grey40"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E5EEF5")
        )
    })
    
    # -------------------------------------------------------
    # Temperature anomaly + glacier area graph
    # -------------------------------------------------------
    
    output$temperature_plot <- renderPlot({
      req(isTRUE(input$show_temperature))
      
      x <- glacier_selected_plot()
      req(nrow(x) > 0)
      req(nrow(temperature_data) > 0)
      
      start_year <- 1984
      end_year   <- 2024
      
      x_period <- x |>
        dplyr::filter(year >= start_year, year <= end_year)
      
      temp_period <- temperature_data |>
        dplyr::filter(year >= start_year, year <= end_year)
      
      req(nrow(x_period) > 0)
      req(nrow(temp_period) > 0)
      
      area_values <- x_period$area_m2 / 1e6
      temp_values <- temp_period$temp_anomaly_jja
      
      area_range <- range(area_values, na.rm = TRUE)
      temp_range <- range(temp_values, na.rm = TRUE)
      
      validate(
        need(all(is.finite(area_range)), "Invalid ice-cover area values."),
        need(all(is.finite(temp_range)), "Invalid temperature anomaly values."),
        need(diff(area_range) != 0, "Ice-cover area is constant over the displayed period."),
        need(diff(temp_range) != 0, "Temperature anomaly is constant over the displayed period.")
      )
      
      temp_period <- temp_period |>
        dplyr::mutate(
          temp_scaled = (temp_anomaly_jja - temp_range[1]) /
            (temp_range[2] - temp_range[1]) *
            (area_range[2] - area_range[1]) +
            area_range[1]
        )
      
      ggplot() +
        geom_line(
          data = x_period,
          aes(x = year, y = area_m2 / 1e6, color = "Ice-cover area"),
          linewidth = 1.1
        ) +
        geom_point(
          data = x_period,
          aes(x = year, y = area_m2 / 1e6, color = "Ice-cover area"),
          size = 2.5
        ) +
        geom_line(
          data = temp_period,
          aes(x = year, y = temp_scaled, color = "Temperature anomaly"),
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = current_year(),
          linetype = "dotted",
          color = "grey40"
        ) +
        scale_x_continuous(
          limits = c(start_year, end_year),
          breaks = seq(start_year, end_year, by = 5)
        ) +
        scale_y_continuous(
          name = "Ice-cover area (km²)",
          sec.axis = sec_axis(
            trans = ~ (. - area_range[1]) / (area_range[2] - area_range[1]) *
              (temp_range[2] - temp_range[1]) + temp_range[1],
            name = "JJA mean temperature anomaly (°C)"
          )
        ) +
        scale_color_manual(
          values = c(
            "Ice-cover area" = glacier_blue,
            "Temperature anomaly" = heat_orange
          )
        ) +
        labs(
          title = "Ice-cover and regional summer temperature anomaly",
          subtitle = paste(input$glacier_name, "| Displayed period: 1984 - 2024"),
          x = "Year",
          color = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 15, color = deep_blue),
          plot.subtitle = element_text(color = "grey40"),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E5EEF5")
        )
    })
  }
  
  # =========================================================
  # App
  # =========================================================
  
  shinyApp(ui, server)
}