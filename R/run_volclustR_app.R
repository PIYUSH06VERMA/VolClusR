library(shiny)
library(bslib)
library(mclust)
library(ggplot2)

#' Launch the VolClusR Interactive Dashboard
#'
#' @description Starts a professional, local Shiny web application to interactively
#' upload OHLCV data, execute the C++ engine, and visualize EM clustering.
#'
#' @export
#' @importFrom utils read.csv
run_volclustR_app <- function() {

  ui <- bslib::page_sidebar(
    title = "VolClusR | Quantitative Regime Terminal",
    theme = bslib::bs_theme(version = 5, bootswatch = "cerulean", primary = "#007BFF"),

    sidebar = bslib::sidebar(
      width = 300,
      title = "Engine Parameters",

      shiny::fileInput("file_upload", "1. Upload OHLCV Data (.csv)", accept = c(".csv")),

      # Option 2: An unmissable alert box
      shiny::tags$div(
        class = "alert alert-warning py-2", # 'py-2' keeps it thin and neat
        style = "font-size: 0.85em; margin-top: -10px;", # Pulls it slightly closer to the upload button
        shiny::icon("triangle-exclamation"),
        " The csv file must be a OHLCV dataset, requiring the following columns(Case Sensitive): ",
        shiny::tags$strong("Date, Open, High, Low, Close, Volume")
      ),

      shiny::numericInput("window_size", "2. Rolling Window Size (Days):",
                          value = 150, min = 10, max = 500, step = 10),

      shiny::sliderInput("cluster_range", "3. Max Regimes to Test (BIC):",
                         min = 2, max = 8, value = 5, step = 1),

      shiny::helpText("Sets the maximum number of states the EM algorithm will evaluate before auto-selecting the best fit."),

      shiny::sliderInput("num_pc","4. Number of Principal Componenets:",
                         min=2,max=6,value=2,step=1),

      shiny::helpText("Sets the number of principal components used in data manipulation, which is finally fed to EM algorithm."),
      shiny::hr(),

      shiny::actionButton("run_btn", "EXECUTE PIPELINE",
                          class = "btn-primary w-100 btn-lg",
                          icon = shiny::icon("bolt"))
    ),

    # --- MAIN DASHBOARD AREA ---
    # Top Row: KPI Value Boxes
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "Analyzed Observations",
        value = shiny::textOutput("kpi_obs"),
        theme = "secondary",
        showcase = shiny::icon("database")
      ),
      bslib::value_box(
        title = "Optimal Regimes Found",
        value = shiny::textOutput("kpi_regimes"),
        theme = "primary",
        showcase = shiny::icon("layer-group")
      ),
      bslib::value_box(
        title = "Current Window Setting",
        value = shiny::textOutput("kpi_window"),
        theme = "info",
        showcase = shiny::icon("calendar-days")
      )
    ),

    shiny::br(),

    # Bottom Area: Tabbed Interface for the 5 Plots
    # Increased height to 550px since they now have the whole screen to themselves!
    bslib::navset_card_tab(
      id = "plot_tabs",

      bslib::nav_panel(
        "Master Timeline",
        shiny::plotOutput("plot_timeline", height = "550px")
      ),
      bslib::nav_panel(
        "Return Density",
        shiny::plotOutput("plot_density", height = "550px")
      ),
      bslib::nav_panel(
        "Volatility Clustering",
        shiny::plotOutput("plot_returns", height = "550px")
      ),
      bslib::nav_panel(
        "Transition Matrix",
        shiny::plotOutput("plot_heatmap", height = "550px")
      ),
      bslib::nav_panel(
        "Volatility Shocks",
        shiny::plotOutput("plot_volshock", height = "550px")
      )
    )
  )

  server <- function(input, output, session) {

    engine_plots <- shiny::reactiveVal(NULL)
    kpi_data <- shiny::reactiveValues(obs = "-", regimes = "-", window = "-")

    shiny::observeEvent(input$run_btn, {

      shiny::req(input$file_upload)

      id <- shiny::showNotification("Executing C++ Engine and EM Clustering...",
                                    duration = NULL, type = "message")

      tryCatch({
        df <- read.csv(input$file_upload$datapath)

        if("Date" %in% colnames(df)) {
          if(grepl(":", df$Date[1])) {
            df$Date <- as.POSIXct(df$Date, tz="UTC")
          } else {
            df$Date <- as.Date(df$Date)
          }
        } else {
          stop("The uploaded CSV must contain a 'Date' column.")
        }

        features <- extract_rolling_features(df, window = input$window_size)
        pca_data <- reduce_features_pca(features, ncomp = input$num_pc)
        regime_data <- em_fit(pca_data, centers = 2:input$cluster_range)

        plts <- plot_volatility_regimes(price_data = df,
                                        feature_data = features,
                                        regime_data = regime_data)

        engine_plots(plts)
        kpi_data$obs <- format(nrow(features), big.mark = ",")
        kpi_data$regimes <- length(unique(regime_data$Regime))
        kpi_data$window <- paste(input$window_size, "Days")

        shiny::removeNotification(id)
        shiny::showNotification("Computation Complete!", type = "default", duration = 3)

      }, error = function(e) {
        shiny::removeNotification(id)
        shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })


    output$kpi_obs <- shiny::renderText({ kpi_data$obs })
    output$kpi_regimes <- shiny::renderText({ kpi_data$regimes })
    output$kpi_window <- shiny::renderText({ kpi_data$window })

    output$plot_timeline <- shiny::renderPlot({
      plots <- engine_plots()
      if (is.null(plots)) return(NULL)
      return(plots$Timeline)
    })

    output$plot_density <- shiny::renderPlot({
      plots <- engine_plots()
      if (is.null(plots)) return(NULL)
      return(plots$Density)
    })

    output$plot_returns <- shiny::renderPlot({
      plots <- engine_plots()
      if (is.null(plots)) return(NULL)
      return(plots$Returns)
    })

    output$plot_heatmap <- shiny::renderPlot({
      plots <- engine_plots()
      if (is.null(plots)) return(NULL)
      return(plots$Transitions)
    })

    output$plot_volshock <- shiny::renderPlot({
      plots <- engine_plots()
      if (is.null(plots)) return(NULL)
      return(plots$VolShock)
    })
  }

  shiny::shinyApp(ui, server)
}
