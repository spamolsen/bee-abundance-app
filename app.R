library(shiny)
library(ggplot2)
library(dplyr)
library(mgcv)
library(plotly)



# MODEL FORMULAS
#
# # Pan Trap Model
# allvar_pan_vars <- c(
#   "Pan_trap_abundance",
#   "days_since_origin_scaled",
#   "month",
#   "year",
#   "sample_date",
#   "sample_site",
#   "pu_temp_scaled",
#   "pd_temp_scaled",
#   "pu_humidity_scaled",
#   "pu_cloudcover_scaled",
#   "temp_diff_scaled",
#   "pu_windspeed_scaled",
#   "pu_dew",
#   "pu_pressure",
#   "pu_visibility"
# )
# allvar_pan_data <- na.omit(data[, unique(allvar_pan_vars)])

# bam_pan_allvar <- bam(
#   Pan_trap_abundance ~
#     s(days_since_origin_scaled, bs = "cr") +
#       s(month, bs = "cc", k = 7) +
#       s(year, bs = "re") +
#       s(sample_date, bs = "re") +
#       s(sample_site, bs = "re") +
#       te(pu_temp_scaled, pd_temp_scaled, bs = c("cr", "cr"), k = c(5, 5)) +
#       te(
#         pu_humidity_scaled,
#         pu_cloudcover_scaled,
#         bs = c("cr", "cr"),
#         k = c(6, 6)
#       ) +
#       temp_diff_scaled + pu_windspeed_scaled +
#       s(pu_dew, bs = "cr") +
#       s(pu_pressure, bs = "cr", k = 3) +
#       s(pu_visibility, bs = "cr", k = 3),
#   family = nb(),
#   method = "fREML",
#   discrete = TRUE,
#   data = allvar_pan_data
# )

# # Sweep Net Model
# allvar_sweep_vars <- c(
#   "Sweepnet_abundance",
#   "days_since_origin_scaled",
#   "month",
#   "year",
#   "sample_date",
#   "sample_site",
#   "pu_temp_scaled",
#   "pd_temp_scaled",
#   "pu_humidity_scaled",
#   "pu_cloudcover_scaled",
#   "temp_diff_scaled",
#   "pu_windspeed_scaled",
#   "pu_dew",
#   "pu_pressure",
#   "pu_visibility"
# )

# allvar_sweep_data <- na.omit(data[, unique(allvar_sweep_vars)])

# bam_sweep_allvar <- bam(
#   Sweepnet_abundance ~
#     s(days_since_origin_scaled, bs = "cr") +
#       s(month, bs = "cc", k = 7) +
#       s(year, bs = "re") +
#       s(sample_date, bs = "re") +
#       s(sample_site, bs = "re") +
#       te(pu_temp_scaled, pd_temp_scaled, bs = c("cr", "cr"), k = c(5, 5)) +
#       te(
#         pu_humidity_scaled,
#         pu_cloudcover_scaled,
#         bs = c("cr", "cr"),
#         k = c(6, 6)
#       ) +
#       temp_diff_scaled + pu_windspeed_scaled +
#       s(pu_dew, bs = "cr") +
#       s(pu_pressure, bs = "cr", k = 3) +
#       s(pu_visibility, bs = "cr", k = 3),
#   family = nb(),
#   method = "fREML",
#   discrete = TRUE,
#   data = allvar_sweep_data
# )





# Load new models and data (expects these objects in publication_models.RData)
load("publication_models.RData") # bam_pan_allvar, bam_sweep_allvar, allvar_pan_data, allvar_sweep_data

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap", rel = "stylesheet"),
    tags$style(HTML(
"      .shiny-spinner-output-container, .shiny-busy, .loading, .progress, .shiny-output-error,
      div:contains('Loading'), span:contains('Loading'), p:contains('Loading') {
        display: none !important;
      }
      .modal, .modal-backdrop {
        display: none !important;
      }
      * {
        font-family: 'Times New Roman', Times, serif !important;
        font-weight: bold !important;
      }
      body { background-color: #E6E6FA; }
      .well { background-color: #F0E6FF; border: none; border-radius: 10px; padding: 20px; margin-right: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      .interpretation { background-color: #FFFFFF; padding: 15px; border-radius: 8px; margin-top: 20px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
      .header-container {
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        align-items: center;
        margin: -15px -15px 20px -15px;
        padding: 30px 40px;
        background-color: #FFFFFF;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        width: calc(100% + 30px);
      }
      .header-image {
        max-width: 80px;
        height: auto;
        margin-left: 20px;
        filter: hue-rotate(280deg) saturate(0.8) brightness(0.7);
      }
      .title-text {
        margin: 0;
        font-size: 36px;
        flex-grow: 1;
        font-family: 'Lato', sans-serif !important;
        color: #2A0047;
        letter-spacing: 0.5px;
        font-weight: 700 !important;
      }
      pre {
        white-space: pre-wrap;
        word-break: keep-all;
        word-wrap: normal;
        font-size: 14px;
        line-height: 1.4;
      }
      .shiny-text-output {
        white-space: pre-wrap;
        word-break: keep-all;
        word-wrap: normal;
      }
"))
  ),
  div(class = "header-container",
      h2("GAMM Analysis of Bee Abundance", 
         class = "title-text"),
      a(href = "https://massasoitstem.com/staging-site-1/",
        target = "_blank",
        img(src = "STEMinternshipLogo.png",
            class = "header-image",
            alt = "STEM Internship Logo"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Model Type:",
        choices = c(
          "Pan Trap" = "bam_pan_allvar",
          "Sweep Net" = "bam_sweep_allvar"
        ),
        selected = "bam_pan_allvar"
      ),
      selectInput("plot_type", "Plot Type", choices = c(
        "Temporal Trend" = "days_since_origin_scaled",
        "Seasonal Pattern" = "month",
        "Year Effect" = "year",
        "Site Effect" = "sample_site",
        "Temperature Interaction" = "temp_interaction",
        "Humidity-Cloud Cover Interaction" = "humidity_cloud_interaction",
        "Temperature Difference" = "temp_diff",
        "Wind Speed" = "windspeed",
        "Dew Point" = "dew",
        "Pressure" = "pressure",
        "Visibility" = "visibility"
      ),
        selected = "days_since_origin_scaled"
      ),
      selectInput("vis_type", "Visualization Type:",
                 choices = c(
                   "Smooth Only" = "smooth_only",
                   "Scatter + Smooth" = "scatter_smooth",
                   "Contour/Heatmap" = "contour",
                   "3D Surface" = "3d"
                 ),
                 selected = "scatter_smooth"),
      checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE),
      dateRangeInput("date_range", "Date Range",
               start = NULL, end = NULL),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Visualizations",
                 plotlyOutput("model_plot", height = "500px"),
                 div(class = "interpretation",
                     h4("Model Description"),
                     verbatimTextOutput("model_description"),
                     h4("Model Statistics"),
                     pre(
                      style = "white-space: pre-wrap;",
                      textOutput("model_stats")
                     ),
                     h4("Plot Interpretation"),
                     verbatimTextOutput("plot_interpretation"))),
        tabPanel("Model Diagnostics",
                 plotOutput("diagnostic_plot", height = "600px"),
                 div(class = "interpretation",
                     h4("Diagnostic Interpretation"),
                     verbatimTextOutput("diagnostic_interpretation")))
      )
    )
  )
)

server <- function(input, output, session) {
  # Remove any loading screens or progress indicators
  observeEvent(1, {
    # Remove any modal dialogs
    removeModal()
    # Clear any notifications
    if(exists('closeNotification', envir = as.environment('package:shiny'))) {
      session$closeNotification()
    }
  }, once = TRUE)
  
  # Load data and set up reactive values
  model_data <- reactiveValues(
    pan_data = NULL,
    sweep_data = NULL,
    min_date = NULL,
    max_date = NULL
  )
  
  # Initialize data when the app starts
  observe({
    req(bam_pan_allvar, bam_sweep_allvar)
    
    # Get data from the models
    model_data$pan_data <- model.frame(bam_pan_allvar)
    model_data$sweep_data <- model.frame(bam_sweep_allvar)
    
    # Set date range
    all_dates <- c(
      as.Date(model_data$pan_data$sample_date, format = "%Y-%m-%d"),
      as.Date(model_data$sweep_data$sample_date, format = "%Y-%m-%d")
    )
    
    model_data$min_date <- min(all_dates, na.rm = TRUE)
    model_data$max_date <- max(all_dates, na.rm = TRUE)
    
    # Update date range input
    updateDateRangeInput(session, "date_range",
      start = model_data$min_date,
      end = model_data$max_date,
      min = model_data$min_date,
      max = model_data$max_date
    )
  })
  
  # Function to get filtered data based on current inputs
  get_filtered_data <- reactive({
    req(input$date_range, model_data$pan_data, model_data$sweep_data)
    
    # Get the appropriate dataset based on model type
    current_data <- if(input$model_type == "bam_pan_allvar") {
      model_data$pan_data
    } else {
      model_data$sweep_data
    }
    
    # Convert sample_date to Date if it's not already
    if ("sample_date" %in% names(current_data)) {
      current_data$date <- as.Date(current_data$sample_date, format = "%Y-%m-%d")
    } else {
      current_data$date <- as.Date(rownames(current_data), format = "%Y-%m-%d")
    }
    
    # Filter by date range
    filtered_data <- current_data %>%
      filter(date >= as.Date(input$date_range[1]),
             date <= as.Date(input$date_range[2]))
    
    filtered_data
  })
  
  # This is now handled in the data loading observer
  
  # Model predictions with confidence intervals
  get_predictions <- function(model, newdata) {
    pred <- predict(model$gam, newdata = newdata, se.fit = TRUE)
    data.frame(
      fit = pred$fit,
      lower = pred$fit - 1.96 * pred$se.fit,
      upper = pred$fit + 1.96 * pred$se.fit
    )
  }
  
  # Model description
  output$model_description <- renderText({
    is_pan <- input$model_type == "gamm_pan_allvar"
    paste0(
      "Model Type: ", if(is_pan) "Pan Trap" else "Sweep Net", " GAMM\n",
      paste0(
        "Model Structure:\n",
        "- Response: ", if(is_pan) "Pan trap abundance" else "Sweep net abundance", "\n",
        "- Family: Negative binomial\n",
        "- Link function: Log\n",
        "- Random effects: Sample site\n",
        "- Temporal modeling: ", if(is_pan) "Smooth term (k=)" else "Enhanced smooth term (k=)", "\n",
        "- Temporal correlation: ", if(is_pan) "None (ACF =)" else "Moderate (ACF =)", "\n",
        "- Seasonal pattern: Cyclic cubic spline (k=6)\n",
        "- Interactions: Temperature-Humidity and Wind-Cloud\n"
      )
    )
  })

  # Model statistics output
  output$model_stats <- renderText({
    req(input$model_type)
    model <- if(input$model_type == "gamm_pan_allvar") bam_pan_allvar else bam_sweep_allvar
    
    # Calculate residuals if not already available
    if(!exists("pan_resids")) {
      pan_resids <- data.frame(
        scaledResiduals = residuals(bam_pan_allvar, type = "scaled.pearson")
      )
    }
    if(!exists("sweep_resids")) {
      sweep_resids <- data.frame(
        scaledResiduals = residuals(bam_sweep_allvar, type = "scaled.pearson")
      )
    }
    resids <- if(input$model_type == "gamm_pan_allvar") pan_resids else sweep_resids
    
    tryCatch({
      # Get model summary
      sum_model <- summary(model)
      
      # Get deviance explained
      dev_expl <- sum_model$dev.expl
      
      # Calculate ACF at lag 1
      acf_value <- acf(resids$scaledResiduals, lag.max = 1, plot = FALSE)$acf[2]
      
      # Get significance of terms
      s_table <- sum_model$s.table
      signif_terms <- sapply(1:nrow(s_table), function(i) {
        p_val <- s_table[i, "p-value"]
        if (p_val < 0.001) return("p < 0.001 ***")
        else if (p_val < 0.01) return(sprintf("p = %.3f **", p_val))
        else if (p_val < 0.05) return(sprintf("p = %.3f *", p_val))
        else if (p_val < 0.1) return(sprintf("p = %.3f .", p_val))
        else return(sprintf("p = %.3f", p_val))
      })
      names(signif_terms) <- rownames(s_table)
      
      # Find term names for new model formula
      temporal_term <- grep("days_since_origin", names(signif_terms), value = TRUE)[1]
      seasonal_term <- grep("month", names(signif_terms), value = TRUE)[1]
      year_term <- grep("year", names(signif_terms), value = TRUE)[1]
      sample_date_term <- grep("sample_date", names(signif_terms), value = TRUE)[1]
      site_term <- grep("sample_site", names(signif_terms), value = TRUE)[1]
      temp_interaction_term <- grep("pu_temp_scaled.*pd_temp_scaled", names(signif_terms), value = TRUE)[1]
      humidity_cloud_interaction_term <- grep("pu_humidity_scaled.*pu_cloudcover_scaled", names(signif_terms), value = TRUE)[1]
      temp_diff_term <- grep("temp_diff_scaled", names(signif_terms), value = TRUE)[1]
      wind_term <- grep("pu_windspeed_scaled", names(signif_terms), value = TRUE)[1]
      dew_term <- grep("pu_dew", names(signif_terms), value = TRUE)[1]
      pressure_term <- grep("pu_pressure", names(signif_terms), value = TRUE)[1]
      visibility_term <- grep("pu_visibility", names(signif_terms), value = TRUE)[1]
      
      paste0(
        "Model Summary Statistics for ", 
        if(input$model_type == "gamm_pan_allvar") "Pan Trap" else "Sweep Net", 
        " GAMM:\n\n",
        "Model Fit:\n",
        sprintf("• Deviance explained: %.1f%%\n", dev_expl * 100),
        sprintf("• Empirical ACF at lag 1: %.3f\n\n", acf_value),
        "Significance of Terms:\n",
        if(!is.na(temporal_term)) paste0("• Temporal trend (days_since_origin_scaled): ", signif_terms[temporal_term], "\n") else "",
        if(!is.na(seasonal_term)) paste0("• Seasonal pattern (month): ", signif_terms[seasonal_term], "\n") else "",
        if(!is.na(year_term)) paste0("• Year effect: ", signif_terms[year_term], "\n") else "",
        if(!is.na(sample_date_term)) paste0("• Sample date effect: ", signif_terms[sample_date_term], "\n") else "",
        if(!is.na(site_term)) paste0("• Site effect: ", signif_terms[site_term], "\n") else "",
        if(!is.na(temp_interaction_term)) paste0("• Temperature interaction (pu_temp_scaled × pd_temp_scaled): ", signif_terms[temp_interaction_term], "\n") else "",
        if(!is.na(humidity_cloud_interaction_term)) paste0("• Humidity × Cloud Cover interaction: ", signif_terms[humidity_cloud_interaction_term], "\n") else "",
        if(!is.na(temp_diff_term)) paste0("• Temperature difference (temp_diff_scaled): ", signif_terms[temp_diff_term], "\n") else "",
        if(!is.na(wind_term)) paste0("• Wind speed (pu_windspeed_scaled): ", signif_terms[wind_term], "\n") else "",
        if(!is.na(dew_term)) paste0("• Dew point (pu_dew): ", signif_terms[dew_term], "\n") else "",
        if(!is.na(pressure_term)) paste0("• Pressure (pu_pressure): ", signif_terms[pressure_term], "\n") else "",
        if(!is.na(visibility_term)) paste0("• Visibility (pu_visibility): ", signif_terms[visibility_term], "\n") else "",
        "\nSignificance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"
      )
      paste0(
        "Model Summary Statistics for ", 
        if(input$model_type == "gamm_pan_allvar") "Pan Trap" else "Sweep Net", 
        " GAMM:\n\n",
        "Model Fit:\n",
        sprintf("• Deviance explained: %.1f%%\n", dev_expl * 100),
        sprintf("• Empirical ACF at lag 1: %.3f\n\n", acf_value),
        "Significance of Terms:\n",
        if(!is.na(temporal_term)) paste0("• Temporal trend (days_since_origin_scaled): ", signif_terms[temporal_term], "\n") else "",
        if(!is.na(seasonal_term)) paste0("• Seasonal pattern (month): ", signif_terms[seasonal_term], "\n") else "",
        if(!is.na(year_term)) paste0("• Year effect: ", signif_terms[year_term], "\n") else "",
        if(!is.na(sample_date_term)) paste0("• Sample date effect: ", signif_terms[sample_date_term], "\n") else "",
        if(!is.na(site_term)) paste0("• Site effect: ", signif_terms[site_term], "\n") else "",
        if(!is.na(temp_interaction_term)) paste0("• Temperature interaction (pu_temp_scaled × pd_temp_scaled): ", signif_terms[temp_interaction_term], "\n") else "",
        if(!is.na(humidity_cloud_interaction_term)) paste0("• Humidity × Cloud Cover interaction (pu_humidity_scaled × pu_cloudcover_scaled): ", signif_terms[humidity_cloud_interaction_term], "\n") else "",
        if(!is.na(temp_diff_term)) paste0("• Temperature difference (temp_diff_scaled): ", signif_terms[temp_diff_term], "\n") else "",
        if(!is.na(wind_term)) paste0("• Wind speed (pu_windspeed_scaled): ", signif_terms[wind_term], "\n") else "",
        if(!is.na(dew_term)) paste0("• Dew point (pu_dew): ", signif_terms[dew_term], "\n") else "",
        if(!is.na(pressure_term)) paste0("• Pressure (pu_pressure): ", signif_terms[pressure_term], "\n") else "",
        if(!is.na(visibility_term)) paste0("• Visibility (pu_visibility): ", signif_terms[visibility_term], "\n") else "",
        "\nSignificance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"
      )
    }, error = function(e) {
      print(paste("Error in model_stats:", e$message))
      "Error loading model statistics. Please check the console for details."
    })
  })



  # Observer to update visualization type based on plot type
  observeEvent(input$plot_type, {
    # Set default visualization types for new model variables and interactions
    if (input$plot_type %in% c(
      "temp_temp_interaction",  # pu_temp_scaled × pd_temp_scaled
      "humidity_cloud_interaction"  # pu_humidity_scaled × pu_cloudcover_scaled
    )) {
      # Interaction plots default to contour/heatmap
      updateSelectInput(session, "vis_type", selected = "contour")
    } else if (input$plot_type %in% c(
      "days_since_origin_scaled", "month", "year", "sample_date", "sample_site",
      "temp_diff_scaled", "pu_windspeed_scaled", "pu_dew",
      "pu_pressure", "pu_visibility"
    )) {
      # Single variable plots default to scatter+smooth
      updateSelectInput(session, "vis_type", selected = "scatter_smooth")
    }
  }, ignoreInit = TRUE)  # Don't trigger on initial load

  # Debug observer for visualization type changes
  observeEvent(input$vis_type, {
    cat("Visualization type changed to:", input$vis_type, "\n")
  })

  # Debug observer for plot type changes
  observeEvent(input$plot_type, {
    cat("Plot type changed to:", input$plot_type, "\n")
  })

  # Main plot output
  output$model_plot <- renderPlotly({
    req(input$plot_type, input$vis_type, input$model_type)
    
    # Get filtered data
    plot_data <- get_filtered_data()
    
    # Check if data is available
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(plot_ly() %>% 
               add_text(x = 0.5, y = 0.5, text = "No data available for the selected filters", 
                        textposition = "middle center") %>%
               layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
    }
    
    # Set y-axis label based on model type
    y_label <- if(input$model_type == "bam_pan_allvar") {
      "Pan Trap Abundance (log+1)"
    } else {
      "Sweep Net Abundance (log+1)"
    }
    
    # Set plot title prefix
    plot_title_prefix <- if(input$model_type == "bam_pan_allvar") {
      "Pan Trap: "
    } else {
      "Sweep Net: "
    }
    
    # Filter data based on date range and prepare scaled variables
    filtered_data <- plot_data %>%
      mutate(
        # Only new model variables and interactions
        days_since_origin_scaled = as.numeric(days_since_origin_scaled),
        month = as.factor(month),
        year = as.factor(year),
        sample_date = as.factor(sample_date),
        sample_site = as.factor(sample_site),
        pu_temp_scaled = as.numeric(pu_temp_scaled),
        pd_temp_scaled = as.numeric(pd_temp_scaled),
        temp_diff_scaled = as.numeric(temp_diff_scaled),
        pu_humidity_scaled = as.numeric(pu_humidity_scaled),
        pu_cloudcover_scaled = as.numeric(pu_cloudcover_scaled),
        pu_windspeed_scaled = as.numeric(pu_windspeed_scaled),
        pu_dew = as.numeric(pu_dew),
        pu_pressure = as.numeric(pu_pressure),
        pu_visibility = as.numeric(pu_visibility)
      )
    
    # Set the response variable based on model type
    if (input$model_type == "bam_pan_allvar") {
      # Get the response variable name from the model
      resp_var <- all.vars(bam_pan_allvar$formula)[1]
      # Get the response values from the model frame
      filtered_data$abundance <- bam_pan_allvar$model[[resp_var]]
    } else {
      # Get the response variable name from the model
      resp_var <- all.vars(bam_sweep_allvar$formula)[1]
      # Get the response values from the model frame
      filtered_data$abundance <- bam_sweep_allvar$model[[resp_var]]
    }
    
    # Set plot title prefix
    plot_title_prefix <- if(input$model_type == "bam_pan_allvar") "Pan Trap" else "Sweep Net"


    if(input$plot_type == "temp_interaction") {
      # 2D interaction: pu_temp_scaled vs pd_temp_scaled
      if (!all(c("pu_temp_scaled", "pd_temp_scaled") %in% names(filtered_data)) ||
          all(is.na(filtered_data$pu_temp_scaled)) ||
          all(is.na(filtered_data$pd_temp_scaled))) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Required temperature variables not available", size = 8, fontface = "bold") +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        return(ggplotly(p))
      }
      plot_data <- filtered_data[!is.na(filtered_data$pu_temp_scaled) & !is.na(filtered_data$pd_temp_scaled), ]
      
      if (nrow(plot_data) < 2 || length(unique(plot_data$pu_temp_scaled)) < 2 || 
          length(unique(plot_data$pd_temp_scaled)) < 2) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Not enough data for Temperature Interaction plot", 
                   size = 8, fontface = "bold") +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        return(ggplotly(p))
      }
      
      # Create prediction grid
      x_seq <- seq(min(plot_data$pu_temp_scaled, na.rm=TRUE), 
                  max(plot_data$pu_temp_scaled, na.rm=TRUE), 
                  length.out=25)
      y_seq <- seq(min(plot_data$pd_temp_scaled, na.rm=TRUE), 
                  max(plot_data$pd_temp_scaled, na.rm=TRUE), 
                  length.out=25)
      grid <- expand.grid(pu_temp_scaled = x_seq, pd_temp_scaled = y_seq)
      
      # Filter data based on model type
      filtered_data <- if(input$model_type == "bam_pan_allvar") {
        allvar_pan_data
      } else {
        allvar_sweep_data
      }
      # Create new data for prediction with all required variables
      new_data <- plot_data[1, ]
      new_data <- new_data[rep(1, nrow(grid)), ]
      new_data$pu_temp_scaled <- grid$pu_temp_scaled
      new_data$pd_temp_scaled <- grid$pd_temp_scaled
      
      # Predict using the model
      grid$pred <- tryCatch({
        predict(model, newdata = new_data, type = "response", exclude = NULL)
      }, error = function(e) {
        message("Prediction error: ", e$message)
        rep(NA, nrow(grid))
      })
      
      # Reshape for plotting
      mat <- matrix(grid$pred, nrow = length(x_seq), ncol = length(y_seq))
      
      # Create the plot
      p <- plot_ly(x = x_seq, y = y_seq, z = mat, type = "surface") %>%
        layout(scene = list(
          xaxis = list(title = "Pick-up Temperature (scaled)"),
          yaxis = list(title = "Put-down Temperature (scaled)"),
          zaxis = list(title = "Predicted Abundance")
        ),
        title = paste0(plot_title_prefix, " Temperature Interaction")
      )
      
      return(p %>% layout(hoverlabel = list(bgcolor = "white"), dragmode = "zoom"))
    }
    if(input$plot_type == "humidity_cloud_interaction") {
      # 2D interaction: pu_humidity_scaled vs pu_cloudcover_scaled
      if (!all(c("pu_humidity_scaled", "pu_cloudcover_scaled") %in% names(filtered_data)) ||
          all(is.na(filtered_data$pu_humidity_scaled)) ||
          all(is.na(filtered_data$pu_cloudcover_scaled))) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Required humidity/cloud cover variables not available", size = 6, fontface = "bold") +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        return(ggplotly(p))
      }
      plot_data <- filtered_data[!is.na(filtered_data$pu_humidity_scaled) & !is.na(filtered_data$pu_cloudcover_scaled), ]
      if (nrow(plot_data) < 2 || length(unique(plot_data$pu_humidity_scaled)) < 2 || length(unique(plot_data$pu_cloudcover_scaled)) < 2) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Not enough data for Humidity-Cloud Cover Interaction", size = 6, fontface = "bold") +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        return(ggplotly(p))
      }
      x_seq <- seq(min(plot_data$pu_humidity_scaled, na.rm=TRUE), max(plot_data$pu_humidity_scaled, na.rm=TRUE), length.out=50)
      y_seq <- seq(min(plot_data$pu_cloudcover_scaled, na.rm=TRUE), max(plot_data$pu_cloudcover_scaled, na.rm=TRUE), length.out=50)
      grid <- expand.grid(pu_humidity_scaled = x_seq, pu_cloudcover_scaled = y_seq)
      model <- if(input$model_type == "gamm_pan_allvar") gamm_pan_allvar else gamm_sweep_allvar
      base_row <- plot_data[1, , drop=FALSE]
      keep_cols <- intersect(names(base_row), setdiff(names(base_row), c("pu_humidity_scaled", "pu_cloudcover_scaled")))
      pred_data <- cbind(base_row[rep(1, nrow(grid)), keep_cols, drop=FALSE], grid)
      grid$pred <- tryCatch({
        predict(model, newdata = pred_data, type = "response")
      }, error = function(e) rep(NA, nrow(grid)))
      mat <- matrix(grid$pred, nrow = 50, ncol = 50)
      return(
        plot_ly(x = x_seq, y = y_seq, z = mat, type = "surface") %>%
          layout(scene = list(xaxis = list(title = "Humidity (scaled)"),
                              yaxis = list(title = "Cloud Cover (scaled)"),
                              zaxis = list(title = "Predicted Abundance")),
                 title = paste0(plot_title_prefix, " Humidity-Cloud Cover Interaction")) %>%
          layout(hoverlabel = list(bgcolor = "white"), 
                 dragmode = "zoom",
                 margin = list(l = 65, r = 50, b = 65, t = 90))
      )
    }
    if(input$plot_type == "temporal") {
      p <- ggplot(filtered_data, aes(x = days_since_origin_scaled, y = abundance))
        
      if(input$vis_type == "scatter_smooth") {
        p <- p + geom_point(alpha = 0.5)
      }
        
      if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
        p <- p + geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
                     se = input$show_ci)
      } else if(input$vis_type == "contour") {
        p <- p + stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
          scale_fill_viridis_c() +
          geom_density_2d(color = "white", alpha = 0.5)
      }
        
      p <- p + labs(x = "Days Since Origin (scaled)",
                    y = y_label,
                    fill = "Density",
                    title = paste0(plot_title_prefix, " Temporal Trend")) +
        theme_minimal()
        
      if(input$vis_type == "3d") {
        # Create matrix for 3D surface
        days_range <- range(filtered_data$days_since_origin_scaled, na.rm = TRUE)
        abundance_range <- range(filtered_data$abundance, na.rm = TRUE)
        days_seq <- seq(days_range[1], days_range[2], length.out = 50)
        abundance_seq <- seq(abundance_range[1], abundance_range[2], length.out = 50)
          
        # Calculate density
        dens <- MASS::kde2d(filtered_data$days_since_origin_scaled, 
                           filtered_data$abundance, n = 50)
          
        return(
          plot_ly() %>%
            add_surface(x = days_seq, y = abundance_seq, z = dens$z,
                       colorscale = "Viridis") %>%
            layout(scene = list(xaxis = list(title = "Days Since Origin (scaled)"),
                               yaxis = list(title = "Abundance"),
                               zaxis = list(title = "Density")),
                   title = paste0(plot_title_prefix, " Temporal Trend (3D)")) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom",
                   margin = list(l = 65, r = 50, b = 65, t = 90))
        )
      } else {
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
      }
      
    } else if(input$plot_type == "seasonal") {
      p <- ggplot(filtered_data, aes(x = as.numeric(month), y = abundance)) +
        scale_x_continuous(breaks = 1:12, 
                         labels = month.abb,
                         limits = c(1, 12))
      
      if(input$vis_type == "scatter_smooth") {
        p <- p + geom_point(alpha = 0.5, position = position_jitter(width = 0.2))
      }
      
      if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
        p <- p + geom_smooth(method = "gam", 
                           formula = y ~ s(x, bs = "cc", k = 7),
                           se = input$show_ci)
      } else if(input$vis_type == "contour") {
        p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                               geom = "raster", 
                               contour = FALSE) +
          scale_fill_viridis_c() +
          geom_density_2d(color = "white", alpha = 0.5)
      }
      
      p <- p + labs(x = "Month",
                    y = y_label,
                    fill = "Density",
                    title = paste0(plot_title_prefix, " Seasonal Pattern")) +
        theme_minimal()
      
      if(input$vis_type == "3d") {
        # Create matrix for 3D surface
        month_range <- range(as.numeric(filtered_data$month), na.rm = TRUE)
        abundance_range <- range(filtered_data$abundance, na.rm = TRUE)
        month_seq <- seq(month_range[1], month_range[2], length.out = 50)
        abundance_seq <- seq(abundance_range[1], abundance_range[2], length.out = 50)
        
        # Calculate density
        dens <- MASS::kde2d(as.numeric(filtered_data$month), 
                           filtered_data$abundance, n = 50)
        
        return(
          plot_ly() %>%
            add_surface(x = month_seq, 
                       y = abundance_seq, 
                       z = dens$z,
                       colorscale = "Viridis") %>%
            layout(scene = list(xaxis = list(title = "Month",
                                           ticktext = month.abb[1:12],
                                           tickvals = 1:12),
                               yaxis = list(title = "Abundance"),
                               zaxis = list(title = "Density")),
                   title = paste0(plot_title_prefix, " Seasonal Pattern (3D)")) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom",
                   margin = list(l = 65, r = 50, b = 65, t = 90))
        )
      } else {
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
      }
    } else {
      # Environmental effects plots
      if(input$plot_type == "temperature") {
        p <- ggplot(filtered_data, aes(x = pu_temp, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5, position = position_jitter(width = 0.1))
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 5, bs = "tp"),
                             se = input$show_ci,
                             color = "#E41A1C",  # Red color for temperature
                             fill = "#E41A1C",   # Same color for CI
                             alpha = 0.2)         # Slight transparency for CI
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c(option = "plasma") +
            geom_density_2d(color = "white", alpha = 0.3)
        }
        
        p <- p + labs(x = "Temperature (°C)",
                      y = y_label,
                      fill = "Density",
                      title = paste0(plot_title_prefix, " Temperature Effect")) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))
        
        if(input$vis_type == "3d") {
          # Create matrix for 3D surface
          temp_range <- range(filtered_data$pu_temp, na.rm = TRUE)
          abundance_range <- range(filtered_data$abundance, na.rm = TRUE)
          temp_seq <- seq(temp_range[1], temp_range[2], length.out = 50)
          abundance_seq <- seq(abundance_range[1], abundance_range[2], length.out = 50)
          
          # Calculate density
          dens <- MASS::kde2d(filtered_data$pu_temp, 
                             filtered_data$abundance, n = 50)
          
          return(
            plot_ly() %>%
              add_surface(x = temp_seq, 
                         y = abundance_seq, 
                         z = dens$z,
                         colorscale = "Viridis") %>%
              layout(scene = list(xaxis = list(title = "Temperature (°C)"),
                                 yaxis = list(title = "Abundance"),
                                 zaxis = list(title = "Density")),
                     title = paste0(plot_title_prefix, " Temperature Effect (3D)")) %>%
              layout(hoverlabel = list(bgcolor = "white"),
                     dragmode = "zoom",
                     margin = list(l = 65, r = 50, b = 65, t = 90))
          )
        } else {
          return(
            ggplotly(p) %>%
              layout(hoverlabel = list(bgcolor = "white"),
                     dragmode = "zoom")
          )
        }
      } else if(input$plot_type == "temp_temp_interaction") {
        # Temperature pickup vs dropoff interaction
        p <- ggplot(filtered_data) +
          geom_point(aes(x = pu_temp, y = pd_temp, color = abundance), alpha = 0.6) +
          scale_color_viridis_c() +
          labs(x = "Pickup Temperature (°C)",
               y = "Dropoff Temperature (°C)",
               color = "Abundance",
               title = paste0(plot_title_prefix, " Temperature (Pickup vs Dropoff)")) +
          theme_minimal()
        
        ggplotly(p) %>%
          layout(hoverlabel = list(bgcolor = "white"),
                 dragmode = "zoom")
      } else if(input$plot_type == "humidity_cloud_interaction") {
        # 2D interaction: pu_humidity_scaled vs pu_cloudcover_scaled
        if (!all(c("pu_humidity_scaled", "pu_cloudcover_scaled") %in% names(filtered_data)) ||
            all(is.na(filtered_data$pu_humidity_scaled)) ||
            all(is.na(filtered_data$pu_cloudcover_scaled))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Required humidity/cloud cover variables not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        plot_data <- filtered_data[!is.na(filtered_data$pu_humidity_scaled) & 
                                  !is.na(filtered_data$pu_cloudcover_scaled), ]
        
        if (nrow(plot_data) < 2 || length(unique(plot_data$pu_humidity_scaled)) < 2 || 
            length(unique(plot_data$pu_cloudcover_scaled)) < 2) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Not enough data for Humidity-Cloud Cover Interaction plot", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        # Create prediction grid
        x_seq <- seq(min(plot_data$pu_humidity_scaled, na.rm=TRUE), 
                    max(plot_data$pu_humidity_scaled, na.rm=TRUE), 
                    length.out=25)
        y_seq <- seq(min(plot_data$pu_cloudcover_scaled, na.rm=TRUE), 
                    max(plot_data$pu_cloudcover_scaled, na.rm=TRUE), 
                    length.out=25)
        grid <- expand.grid(pu_humidity_scaled = x_seq, pu_cloudcover_scaled = y_seq)
        
        # Get model predictions
        model <- if(input$model_type == "bam_pan_allvar") bam_pan_allvar else bam_sweep_allvar
        
        # Create new data for prediction with all required variables
        new_data <- plot_data[1, ]
        new_data <- new_data[rep(1, nrow(grid)), ]
        new_data$pu_humidity_scaled <- grid$pu_humidity_scaled
        new_data$pu_cloudcover_scaled <- grid$pu_cloudcover_scaled
        
        # Predict using the model
        grid$pred <- tryCatch({
          predict(model, newdata = new_data, type = "response", exclude = NULL)
        }, error = function(e) {
          message("Prediction error: ", e$message)
          rep(NA, nrow(grid))
        })
        
        # Reshape for plotting
        mat <- matrix(grid$pred, nrow = length(x_seq), ncol = length(y_seq))
        
        # Create the plot
        p <- plot_ly(x = x_seq, y = y_seq, z = mat, type = "surface") %>%
          layout(scene = list(
            xaxis = list(title = "Relative Humidity (scaled)"),
            yaxis = list(title = "Cloud Cover (scaled)"),
            zaxis = list(title = "Predicted Abundance")
          ),
          title = paste0(plot_title_prefix, " Humidity-Cloud Cover Interaction")
        )
        
        return(p %>% layout(hoverlabel = list(bgcolor = "white"), dragmode = "zoom"))
      } else if(input$plot_type == "temp_diff") {
        # Temperature difference effect plot
        if (!"temp_diff_scaled" %in% names(filtered_data) || all(is.na(filtered_data$temp_diff_scaled))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Temperature difference variable not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        p <- ggplot(filtered_data, aes(x = temp_diff_scaled, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 5, bs = "cr"),
                             se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Temperature Difference (scaled)",
                      y = y_label,
                      title = paste0(plot_title_prefix, " Temperature Difference Effect")) +
          theme_minimal()
        
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
        
      } else if(input$plot_type == "windspeed") {
        # Wind speed effect plot
        if (!"pu_windspeed_scaled" %in% names(filtered_data) || all(is.na(filtered_data$pu_windspeed_scaled))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Wind speed variable not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        p <- ggplot(filtered_data, aes(x = pu_windspeed_scaled, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 5, bs = "cr"),
                             se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Wind Speed (scaled)",
                      y = y_label,
                      title = paste0(plot_title_prefix, " Wind Speed Effect")) +
          theme_minimal()
        
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
        
      } else if(input$plot_type == "dew") {
        # Dew point effect plot
        if (!"pu_dew" %in% names(filtered_data) || all(is.na(filtered_data$pu_dew))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Dew point variable not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        p <- ggplot(filtered_data, aes(x = pu_dew, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 5, bs = "cr"),
                             se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Dew Point (°C)",
                      y = y_label,
                      title = paste0(plot_title_prefix, " Dew Point Effect")) +
          theme_minimal()
        
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
        
      } else if(input$plot_type == "pressure") {
        # Pressure effect plot
        if (!"pu_pressure" %in% names(filtered_data) || all(is.na(filtered_data$pu_pressure))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Pressure variable not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        p <- ggplot(filtered_data, aes(x = pu_pressure, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 3, bs = "cr"),
                             se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Pressure (hPa)",
                      y = y_label,
                      title = paste0(plot_title_prefix, " Pressure Effect")) +
          theme_minimal()
        
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
        
      } else if(input$plot_type == "visibility") {
        # Visibility effect plot
        if (!"pu_visibility" %in% names(filtered_data) || all(is.na(filtered_data$pu_visibility))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Visibility variable not available", 
                     size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        
        p <- ggplot(filtered_data, aes(x = pu_visibility, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 3, bs = "cr"),
                             se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Visibility (km)",
                      y = y_label,
                      title = paste0(plot_title_prefix, " Visibility Effect")) +
          theme_minimal()
        
        return(
          ggplotly(p) %>%
            layout(hoverlabel = list(bgcolor = "white"),
                   dragmode = "zoom")
        )
      } else if(input$plot_type == "temp-diff") {
        # Robust NA and data checks
        if (!"temp_diff" %in% names(filtered_data) || all(is.na(filtered_data$temp_diff))) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "Temperature difference variable not available", size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        plot_data <- filtered_data[!is.na(filtered_data$temp_diff) & !is.na(filtered_data$abundance), ]
        if (nrow(plot_data) < 2 || length(unique(plot_data$temp_diff)) < 2 || length(unique(plot_data$abundance)) < 2) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "Not enough data for Temperature Difference plot", size = 8, fontface = "bold") +
            theme_void() + xlim(0, 1) + ylim(0, 1)
          return(ggplotly(p))
        }
        p <- ggplot(plot_data, aes(x = temp_diff, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5)
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
                       se = input$show_ci)
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
            scale_fill_viridis_c() +
            geom_density_2d(color = "white", alpha = 0.5)
        }
        
        p <- p + labs(x = "Temperature Difference (°C)",
                      y = y_label,
                      fill = "Density",
                      title = paste0(plot_title_prefix, " Temperature Difference Effect")) +
          theme_minimal()
        
        if(input$vis_type == "3d") {
          temp_diff_range <- range(plot_data$temp_diff, na.rm=TRUE)
          abundance_range <- range(plot_data$abundance, na.rm=TRUE)
          temp_diff_seq <- seq(temp_diff_range[1], temp_diff_range[2], length.out = 50)
          abundance_seq <- seq(abundance_range[1], abundance_range[2], length.out = 50)
          if (length(unique(plot_data$temp_diff)) < 2 || length(unique(plot_data$abundance)) < 2) {
            p <- ggplot() +
              annotate("text", x = 0.5, y = 0.5, label = "Not enough data for 3D Temperature Difference plot", size = 8, fontface = "bold") +
              theme_void() + xlim(0, 1) + ylim(0, 1)
            return(ggplotly(p))
          }
          dens <- MASS::kde2d(plot_data$temp_diff, plot_data$abundance, n = 50)
          return(
            plot_ly() %>%
              add_surface(x = temp_diff_seq, y = abundance_seq, z = dens$z,
                         colorscale = "Viridis") %>%
              layout(scene = list(xaxis = list(title = "Temperature Difference"),
                                 yaxis = list(title = "Abundance"),
                                 zaxis = list(title = "Density")),
                     title = paste0(plot_title_prefix, " Temperature Difference Effect (3D)"))
          )
        } else {
          return(
            ggplotly(p) %>%
              layout(hoverlabel = list(bgcolor = "white"),
                     dragmode = "zoom")
          )
        }
      } else if(input$plot_type == "windspeed") {
        p <- ggplot(filtered_data, aes(x = pu_windspeed, y = abundance))
        
        if(input$vis_type == "scatter_smooth") {
          p <- p + geom_point(alpha = 0.5, position = position_jitter(width = 0.1))
        }
        
        if(input$vis_type %in% c("smooth_only", "scatter_smooth")) {
          p <- p + geom_smooth(method = "gam", 
                             formula = y ~ s(x, k = 5, bs = "tp"),
                             se = input$show_ci,
                             color = "#4DAF4A",  # Green color for wind speed
                             fill = "#4DAF4A",   # Same color for CI
                             alpha = 0.2)         # Slight transparency for CI
        } else if(input$vis_type == "contour") {
          p <- p + stat_density_2d(aes(fill = after_stat(density)), 
                                 geom = "raster", 
                                 contour = FALSE) +
            scale_fill_viridis_c(option = "plasma") +
            geom_density_2d(color = "white", alpha = 0.3)
        }
        
        p <- p + labs(x = "Wind Speed (km/h)",
                      y = y_label,
                      fill = "Density",
                      title = paste0(plot_title_prefix, " Wind Speed Effect")) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5))
        
        if(input$vis_type == "3d") {
          # Create matrix for 3D surface
          wind_range <- range(filtered_data$pu_windspeed, na.rm = TRUE)
          abundance_range <- range(filtered_data$abundance, na.rm = TRUE)
          wind_seq <- seq(wind_range[1], wind_range[2], length.out = 50)
          abundance_seq <- seq(abundance_range[1], abundance_range[2], length.out = 50)
          
          # Calculate density
          dens <- MASS::kde2d(filtered_data$pu_windspeed, 
                             filtered_data$abundance, n = 50)
          
          return(
            plot_ly() %>%
              add_surface(x = wind_seq, 
                         y = abundance_seq, 
                         z = dens$z,
                         colorscale = "Viridis") %>%
              layout(scene = list(xaxis = list(title = "Wind Speed (km/h)"),
                                 yaxis = list(title = "Abundance"),
                                 zaxis = list(title = "Density")),
                     title = paste0(plot_title_prefix, " Wind Speed Effect (3D)")) %>%
              layout(hoverlabel = list(bgcolor = "white"),
                     dragmode = "zoom",
                     margin = list(l = 65, r = 50, b = 65, t = 90))
          )
        } else {
          return(
            ggplotly(p) %>%
              layout(hoverlabel = list(bgcolor = "white"),
                     dragmode = "zoom")
          )
        }
      }
    }
  })

  # Diagnostic plot output
  output$diagnostic_plot <- renderPlot({
    req(input$model_type)
    
    # Get appropriate model and data
    model <- if(input$model_type == "gamm_pan_allvar") bam_pan else bam_sweep
    
    # Create DHARMa residuals
    resids <- simulateResiduals(model)
    
    # Set up plot layout for all diagnostic plots
    # Make plots less wide and taller by adjusting the layout heights
    layout(matrix(c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 2, byrow = TRUE),
           widths = c(0.8, 0.8),  # Make plots less wide
           heights = c(1.2, 1.2, 1.2, 1.2))  # Make plots taller
    par(mar = c(4, 4, 3, 1))
    
    # 1. QQ Plot
    qqnorm(resids$scaledResiduals, main = "1. QQ Plot",
           xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
    qqline(resids$scaledResiduals)
    mtext("Points should follow the line", side = 3, line = 0.2, cex = 0.8)
    
    # 2. Residuals vs. Predicted
    plot(resids$fittedPredictedResponse, resids$scaledResiduals,
         xlab = "Predicted", ylab = "Residuals",
         main = "2. Residuals vs. Predicted")
    abline(h = 0, col = "red", lty = 2)
    lines(lowess(resids$fittedPredictedResponse, resids$scaledResiduals), col = "blue")
    mtext("Should show random scatter around zero", side = 3, line = 0.2, cex = 0.8)
    
    # 3. Scale-Location Plot
    plot(resids$fittedPredictedResponse, sqrt(abs(resids$scaledResiduals)),
         xlab = "Predicted", ylab = expression(sqrt("|")~"Standardized Residuals"~"|"),
         main = "3. Scale-Location")
    lines(lowess(resids$fittedPredictedResponse, sqrt(abs(resids$scaledResiduals))), col = "red")
    mtext("Line should be roughly horizontal", side = 3, line = 0.2, cex = 0.8)
    
    # 4. KS Test Plot
    plot(ecdf(resids$scaledResiduals), main = "4. KS Test Plot",
         xlab = "Residuals", ylab = "Cumulative Probability",
         do.points = FALSE)
    curve(pnorm, add = TRUE, col = "red")
    mtext("ECDF should follow the theoretical line", side = 3, line = 0.2, cex = 0.8)
    
    # 5. Residual Histogram
    hist(resids$scaledResiduals, breaks = 30, main = "5. Residual Distribution",
         xlab = "Standardized Residuals", probability = TRUE)
    curve(dnorm, add = TRUE, col = "red")
    mtext("Should approximate normal distribution", side = 3, line = 0.2, cex = 0.8)
    
    # 6. ACF Plot
    acf(resids$scaledResiduals, main = "6. Autocorrelation Function",
        na.action = na.pass)
    mtext("Lags should not exceed dashed lines", side = 3, line = 0.2, cex = 0.8)
    
    # 7. Dispersion Test
    plot(resids$fittedPredictedResponse, resids$scaledResiduals,
         xlab = "Predicted", ylab = "Residuals",
         main = "7. Dispersion Test")
    abline(h = 0, col = "red", lty = 2)
    lines(lowess(resids$fittedPredictedResponse, resids$scaledResiduals), col = "red")
    mtext("Points should follow red line", side = 3, line = 0.2, cex = 0.8)
    
    # 8. Outlier Test
    plot(resids$scaledResiduals, pch = 16,
         ylab = "Standardized Residuals", xlab = "Observation Number",
         main = "8. Outlier Test")
    abline(h = c(-2, 2), col = "red", lty = 2)
    mtext("Points outside dashed lines may be outliers", side = 3, line = 0.2, cex = 0.8)
    
    # Reset plot layout
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  })

  output$diagnostic_interpretation <- renderText({
    model_type <- if(input$model_type == "gamm_pan_allvar") "Pan Trap" else "Sweep Net"
    paste0(
      "Model Diagnostic Interpretation for ", model_type, " GAMM:\n\n",
      "1. QQ Plot: Shows if residuals follow a normal distribution. Points should follow the diagonal line.\n",
      "2. Residuals vs. Fitted: Shows if there's any pattern in residuals. Points should be randomly scattered.\n",
      "3. Scale-Location: Shows if residual variance is constant. Red line should be horizontal.\n",
      "4. KS Test: Tests uniformity of residuals. Black line should follow red theoretical line.\n",
      "5. Residual Distribution: Shows the distribution of residuals. Should approximate the red normal curve.\n",
      "6. ACF Plot: Shows temporal autocorrelation. Bars should stay within dashed lines.\n",
      "7. Dispersion Test: Tests if variance scales correctly with mean. Points should follow red line.\n",
      "8. Outlier Test: Identifies potential outliers. Points outside dashed lines may be outliers.\n"
    )
  })

  # Plot interpretation for model variables
  output$plot_interpretation <- renderText({
    base_interp <- switch(input$plot_type,
      "days_since_origin_scaled" = paste0(
        "Shows how bee abundance changes over the study period after accounting for seasonal patterns. ",
        "The x-axis represents time scaled from the start of the study, with the curve showing the estimated ",
        "temporal trend in bee abundance after removing seasonal effects."),
      
      "month" = paste0(
        "Shows the seasonal pattern of bee abundance across months. Bees are highly seasonal insects with peak activity ",
        "typically in warmer months. The plot shows the estimated effect of month on bee abundance, helping to identify ",
        "peak activity periods and seasonal trends in the population."),
      
      "year" = paste0(
        "Shows annual variations in bee abundance. This helps identify year-to-year changes that might be related to ",
        "broader environmental factors, climate patterns, or population dynamics beyond the seasonal cycle."),
      
      "sample_date" = paste0(
        "Shows variation in bee abundance across different sampling dates. This accounts for date-specific effects ",
        "that aren't captured by the seasonal or long-term trends, such as short-term weather events or local conditions."),
      
      "sample_site" = paste0(
        "Shows variation in bee abundance across different sampling sites. This accounts for site-specific characteristics ",
        "like habitat quality, floral resources, and microclimate that might influence local bee populations."),
      
      "temp_diff_scaled" = paste0(
        "Shows the effect of temperature difference between pickup and dropoff locations on bee abundance. ",
        "Positive values indicate warmer temperatures at pickup, while negative values indicate warmer temperatures ",
        "at dropoff. The relationship may reveal how bees respond to temperature gradients during their foraging trips."),
      
      "pu_windspeed_scaled" = paste0(
        "Shows how wind speed at the pickup location affects bee abundance. Strong winds can impede bee flight and foraging, ",
        "while calm conditions are generally more favorable. The relationship may not be linear, as some wind can aid in ",
        "scent dispersal but too much can be detrimental."),
      
      "pu_dew" = paste0(
        "Shows how dew point temperature at the pickup location affects bee abundance. Dew point is a measure of atmospheric ",
        "moisture and can influence bee activity, nectar concentration, and flight conditions."),
      
      "pu_pressure" = paste0(
        "Shows how atmospheric pressure at the pickup location affects bee abundance. Changes in barometric pressure ",
        "can influence bee behavior as they can sense approaching weather systems."),
      
      "pu_visibility" = paste0(
        "Shows how visibility at the pickup location affects bee abundance. Visibility can be related to atmospheric ",
        "conditions like fog, haze, or pollution, which might influence bee navigation and foraging behavior."),
      
      "temp_temp_interaction" = paste0(
        "Shows how the interaction between pickup and dropoff temperatures affects bee abundance. This helps ",
        "understand how the combination of temperatures at both locations influences bee activity. The contour ",
        "lines represent different levels of predicted abundance, with warmer colors indicating higher abundance."),
      
      "humidity_cloud_interaction" = paste0(
        "Shows how the interaction between humidity and cloud cover affects bee abundance. This helps understand ",
        "the combined effect of these atmospheric conditions on bee activity. The contour plot reveals complex ",
        "patterns that might not be apparent when looking at each variable in isolation.")
    )
    
    vis_interp <- switch(input$vis_type,
      "smooth_only" = paste0(
        "The smooth line reveals the underlying pattern by fitting a flexible curve through the data. ",
        if(input$show_ci) paste0(
          "The shaded confidence intervals show uncertainty - wider bands mean less certainty about the true relationship. ",
          "Areas with more data points typically have narrower confidence intervals.")
        else "Confidence intervals are hidden to focus on the main trend."),
      
      "scatter_smooth" = paste0(
        "Each point represents a single observation, showing the actual variability in the data. ",
        "The smooth line helps identify the overall pattern by averaging across nearby points. ",
        if(input$show_ci) paste0(
          "Confidence intervals (shaded area) show uncertainty in the trend. ",
          "Wider intervals often occur where data is sparse or highly variable.")
        else "Confidence intervals are hidden to reduce visual complexity."),
      
      "contour" = paste0(
        "The heatmap shows the density of observations, with lighter colors indicating more frequent combinations of values. ",
        "Darker areas show less common conditions, while brighter regions highlight where most observations occur. ",
        "White contour lines connect points of equal density, like elevation lines on a topographic map."),
      
      "3d" = paste0(
        "The 3D surface provides a detailed view of the relationship, with height showing abundance levels. ",
        "Colors reinforce the height information, and you can rotate the plot to examine patterns from different angles. ",
        "Peaks indicate conditions associated with high abundance, while valleys show low abundance conditions."))
    
    paste0(base_interp, vis_interp)
  })

  # Environmental effects plot
  output$env_effects_plot <- renderPlot({
    req(input$model_type)
    model <- if(input$model_type == "gamm_pan_allvar") bam_pan else bam_sweep
    
    # Set up a 3x3 grid for the plots
    par(mfrow = c(3, 2), mar = c(4, 4, 2, 1) + 0.1)
    
    # 1. Temporal trend
    plot(model, select = 1, scale = 0, main = "Temporal Trend", 
         xlab = "Days since origin (scaled)", ylab = "Effect size")
    
    # 2. Seasonal pattern
    plot(model, select = 2, scale = 0, main = "Seasonal Pattern (Month)",
         xlab = "Month", ylab = "Effect size")
    
    # 3. Temperature interaction effect
    vis.gam(model, view = c("pu_temp_scaled", "pd_temp_scaled"), 
            plot.type = "contour", 
            main = "Temperature Interaction Effect",
            xlab = "Pickup Temperature (scaled)",
            ylab = "Dropoff Temperature (scaled)")
    
    # 4. Humidity-Cloud interaction effect
    vis.gam(model, view = c("pu_humidity_scaled", "pu_cloudcover_scaled"), 
            plot.type = "contour", 
            main = "Humidity-Cloud Cover Interaction",
            xlab = "Humidity (scaled)",
            ylab = "Cloud Cover (scaled)")
    
    # 5. Wind speed effect
    plot(model, select = 3, scale = 0, main = "Wind Speed Effect",
         xlab = "Wind Speed (scaled)", ylab = "Effect size")
    
    # 6. Dew point effect
    plot(model, select = 4, scale = 0, main = "Dew Point Effect",
         xlab = "Dew Point", ylab = "Effect size")
  })

  # Environmental effects interpretation
  output$env_effects_interpretation <- renderText({
    paste0(
      "The plots show the estimated effects of key variables on bee abundance:\n\n",
      "1. Temporal Trend: Shows long-term changes in bee abundance over the study period, \n   ",
      "   after accounting for seasonal patterns.\n",
      "2. Seasonal Pattern: Shows how bee abundance varies by month, highlighting \n   ",
      "   seasonal peaks and troughs in activity.\n",
      "3. Temperature Interaction: Shows how the combination of pickup and dropoff \n   ",
      "   temperatures affects bee abundance. Warmer colors indicate higher abundance.\n",
      "4. Humidity-Cloud Cover: Shows how the interaction between humidity and cloud \n   ",
      "   cover influences bee activity. These factors together affect microclimate conditions.\n",
      "5. Wind Speed: Shows how wind speed at the pickup location affects bee abundance, \n   ",
      "   with stronger winds generally reducing flight activity.\n",
      "6. Dew Point: Shows how atmospheric moisture (dew point) affects bee activity, \n   ",
      "   which can influence nectar concentration and flight conditions.\n\n",
      "For smooth terms (1, 2, 5, 6):\n",
      "- Solid line: Estimated effect\n",
      "- Shaded area: 95% confidence interval\n",
      "- Y-axis: Effect size (centered at zero)\n",
      "- Rug plot: Shows data distribution along x-axis\n\n",
      "For interaction terms (3, 4):\n",
      "- Colors: Different abundance levels\n",
      "- Contour lines: Connect points of equal predicted abundance\n",
      "- Warmer colors: Higher predicted abundance"
    )
  })

  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("bee_abundance_", input$plot_type, "_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 7)
    }
  )
  
  # Signal that models are ready
  output$models_ready <- reactive(TRUE)
}

shinyApp(ui, server)
