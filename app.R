# Elk Population Estimator

library("shiny")
library("shinyjs")
library("bslib")
library("DT")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("lubridate")
library("readr")
library("chron")
library("rgdal")
library("readxl")
library("Cairo")
library("rjags")
library("coda")
library("truncnorm")
library("doParallel")
library("nimble")
library("xtable")
library("statip")
library("R2jags")

# run functions
source("helpers_stratified.R")

# Set your working directory paths and survey data file path
wd <- getwd()
input_wd <- paste0(getwd(), "/input")
output_wd <- paste0(getwd(), "/output")

# create input and output folders if you haven't already
dir.create(paste0(wd, "/input"))
dir.create(paste0(wd, "/output"))

# Define UI ----
ui <- fluidPage(
  bootstrapLib(bs_theme(bootswatch = "spacelab")),
  useShinyjs(),
  titlePanel("Roosevelt Elk Abundance Estimator"),
  tabsetPanel(
    type = "tabs",
    tabPanel("Model",
             sidebarLayout(
               sidebarPanel(
                 br(),
                 fileInput("data_file", "Upload Excel File", accept = ".xlsx"),
                 actionButton("run_script", "Run Script"),
                 br(),
                 textOutput("script_status"),
                 # hidden button that tells R to download the results after running script
                 downloadButton("download_results", "Download Results")
               ),
               mainPanel(
                 h3("Welcome"),
                 p(
                   "This app is designed to make statistical modelling of elk survey data quick and easy.
            First, upload your formatted Excel datasheet on the left. Once the model has run, you will
            see a download button. Click the button to export your results file. Load the
            file in the Results tab to view your modelled elk abundance estimates."
                 ),
                 verbatimTextOutput("model_progress"),
                 div(
                   id = "plot-container",
                   img(src = "elk_galore.JPG",
                       style = "width: 90%; height: 90%; display: block; margin-left: auto; margin-right: auto;")
                 ),
                verbatimTextOutput("clock_output"),
                tags$style(
                  HTML("#clock_output {
                       position: absolute;
                       bottom: 10px;
                       left: 10px;
                       }")
                )
               )
             )),
    tabPanel("Results",
             sidebarLayout(
               sidebarPanel(uiOutput("sidebarText")),
               mainPanel(
                 h3("Results"),
                 tabsetPanel(
                   id = "tables_plots",
                   type = "tabs",
                   tabPanel("Table",
                            uiOutput("tableUI")),
                   tabPanel("Plot",
                            uiOutput("plotUI"))
                 )
               )
             ))
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive value defaults
  file_path <- reactiveVal(NULL)
  results_path <- reactiveVal(NULL)
  script_finished <- reactiveVal(FALSE)
  results_reactive <- reactiveValues(data = NULL)
  timer <- reactiveTimer(1000)
  
  # Model tab ----
  
  # clock to prevent timing out
  clock <- reactive({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S")
  })
  
  output$clock_output <- renderText(clock())
  
  observe({
    timer()
    output$clock_output <- renderText({
      clock()
    })
  })
  
  # Show the run model button only if a file is uploaded
  observe({
    shinyjs::hide("run_script")
    if(!is.null(file_path())){
      shinyjs::show("run_script")
    }
  })
  
  # Load data from the uploaded Excel file
  observeEvent(input$data_file, {
    file_path(input$data_file$datapath)
  })
  
  # Run the R script on the uploaded file
  observeEvent(input$run_script, {
      # Show progress bar while running the model
      # withProgress(message = "Running the model...", detail = "This can take an hour or more.", {
    withCallingHandlers({
      shinyjs::html("model_progress", "")
      withProgress(message = "Running the model...", detail = "This can take an hour or more.", {
      source(
          "model_stratified.R",
          echo = T,
          local = TRUE,
          keep.source = FALSE,
          encoding = "UTF-8"
        )
    })
    },
    message = function(m){
      shinyjs::html(id="model_progress", html = "", add=T)
    })    
        results_reactive$data <- output
        script_finished(TRUE)
      # })
  })
  
  observe({
    if(script_finished()){
      # Show the download button after the script has run
      shinyjs::show("download_results")
    } else {
      shinyjs::hide("download_results")
    }
    })
  
  # Display script status
  output$script_status <- renderText({
    if (script_finished()==T) {
      "Script finished running!"
      # } else if (input$run_script()) {
      #   "Running...this can take an hour or more"
    } else {
      "Upload a file and run the script."
    }
  })
  
  # The downloadHandler function
  output$download_results <- downloadHandler(
    filename = function() {
      # Set the filename for the downloaded file
      paste0("Results_", format(Sys.time(), "%Y%b%d_%H%M"), ".csv")
    },
    content = function(file) {
      # Prepare the data for download (use the data from reactiveValues)
      write.csv(results_reactive$data, file, row.names = F)
    }
  )
  
  # Results tab ----
  ## Building blocks ----
  # Get results filepath from results_file
  observeEvent(input$results_file, {
    results_path(input$results_file$datapath)
  })
  
  # Load results data
  results <- reactive({
    req(results_path())
    read.csv(results_path(), header = TRUE)
  })
  
  ### year_choices ----
  year_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    years <- unique(results()$year)
    c("All", years)
  })
  
  ### selected_year ----
  selected_year <- reactive({
    req(results())  # Ensure a file is uploaded
    max(results()$year)
  })
  
  ### EPU_choices ----
  EPU_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    EPUs <- sort(unique(results()$EPU))
    c("All", EPUs)
  })
  
  ### facet ----
  facet <- reactive({
    # faceting depends on year selection: if all, then facet by EPU, if one, facet by year (really just labelling top of graph with year)
    req(input$year)
    if (input$year == "All") {
      "EPU"
    } else {
      "year"
    }
  })
  
  ### x_var_data ----
  x_var_data <- reactive({
    req(input$year)
    if (input$year == "All") {
      "year"
    } else {
      "EPU"
    }
  })
  
  ### ci_data ----
  ci_data <- reactive({
    if (!is.null(input$CI) & 1 %in% input$CI & 2 %in% input$CI) {
      list(
        ucl = c(results()$ucl_95, results()$ucl_50),
        lcl = c(results()$lcl_95, results()$lcl_50)
      )
    } else if (1 %in% input$CI) {
      list(ucl = results()$ucl_95,
           lcl = results()$lcl_95)
    } else if (2 %in% input$CI) {
      list(ucl = results()$ucl_50,
           lcl = results()$lcl_50)
    } else {
      list(ucl = NULL,
           lcl = NULL)
    }
  })
  
  ## Construct table ----
  results_table <- reactive({
    req(results(), input$year, input$EPU)
    table_data <- results()
    if (input$year != "All") {
      table_data <- filter(table_data, table_data$year == input$year)
    }
    if (input$EPU != "All") {
      table_data <- filter(table_data, table_data$EPU == input$EPU)
    }
    table <- table_data %>%
      pivot_wider(
        id_cols = c(year, EPU, min_count, target, calf_cow, bull_cow),
        names_from = "method",
        values_from = "estimate"
      ) %>%
      inner_join(
        table_data %>% filter(!is.na(lcl_50)) %>% select(year, EPU, lcl_50, ucl_50, lcl_95, ucl_95),
        by = c("year", "EPU")
      ) %>%
      select(
        year,
        EPU,
        target,
        min_count,
        Standard,
        Model,
        lcl_50,
        ucl_50,
        lcl_95,
        ucl_95,
        calf_cow,
        bull_cow
      )
    arrange(table, desc(year), EPU)
  })
  
  display_table <- reactive({
    req(results_table())
    table <- results_table()
    table <- table %>%
      mutate(
        `50% Confidence Interval` = paste0(lcl_50, " to ", ucl_50),
        `95% Confidence Interval` = paste0(lcl_95, " to ", ucl_95),
        `Calves per 100 Cows` = round(calf_cow, digits = 0),
        `Bulls per 100 Cows` = round(bull_cow, digits = 0)
      ) %>%
      rename(
        Year = year,
        `Minimum count` = min_count,
        `Population target` = target
      ) %>%
      select(
        Year,
        EPU,
        `Population target`,
        `Minimum count`,
        `Standard estimate` = Standard,
        `Model estimate` = Model,
        `50% Confidence Interval`,
        `95% Confidence Interval`,
        `Calves per 100 Cows`,
        `Bulls per 100 Cows`
      )
    table
  })
  
  ## Construct plot ----
  results_plot <- reactive({
    req(results(),
        input$year,
        input$EPU,
        input$method,
        x_var_data(),
        ci_data(),
        facet())
    facet_col <- facet()
    x_var <- x_var_data()
    plot_data <- results()
    plot_data$x <- plot_data[[x_var]]
    if (input$year != "All") {
      plot_data <- filter(plot_data, plot_data$year == input$year)
    }
    if (input$EPU != "All") {
      plot_data <- filter(plot_data, plot_data$EPU == input$EPU)
    }
    if (input$method != "All") {
      plot_data <- filter(plot_data, plot_data$method == input$method)
    }
    x_data <- plot_data[[x_var]]
    p <- ggplot(plot_data, aes(x = x, y = estimate, fill = method)) +
      labs(fill = "Method",
           color = "Method",
           shape = "Target") +
      # name y axis
      scale_y_continuous("Estimated Abundance", limits = c(0,NA)) +
      # use greyscale for point fill & color
      scale_fill_grey(start = 0, end = 0.7) +
      scale_color_grey(start = 0, end = 0.7) +
      # facet wrap
      facet_wrap(as.formula(paste("~", facet())), scales = "free", ncol =
                   3)
    if (x_var_data() == "year") {
      # make sure there's only one label per year
      p <- p +
        scale_x_continuous("Year", breaks = unique(plot_data$year))
    } else {
      # name x axis
      p <- p +
        scale_x_discrete("Elk Population Unit")
    }
    # change x axis labels depending on input$year and input$EPU
    if (x_var_data() == "year") {
      if (input$EPU == "All") {
        p <- p +
          theme(
            axis.text.x = element_text(
              size = 10,
              angle = 45,
              vjust = .8
            ),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(size = 10)
          )
      } else {
        p <- p +
          theme(
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 14),
            strip.text = element_text(size = 14)
          )
      }
    } else {
      p <- p  +
        theme(
          axis.text.x = element_text(
            size = 12,
            angle = 65,
            vjust = .5
          ),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14)
        )
    }
    # add in 95% & 50% confidence intervals as dotted lines
    if (!is.null(ci_data()$ucl) & 1 %in% input$CI) {
      p <- p +
        # 95% (dashed)
        geom_linerange(
          aes(x, ymin = lcl_95, ymax = ucl_95),
          linetype = 2,
          linewidth = 1,
          position = position_dodge(width = 0.3)
        )
    }
    if (!is.null(ci_data()$ucl) & 2 %in% input$CI) {
      p <- p +
        # 50% (solid)
        geom_linerange(
          aes(x, ymin = lcl_50, ymax = ucl_50),
          linewidth = 1,
          position = position_dodge(width = 0.3)
        )
    }
    if (x_var_data() == "year" & !is.null(input$Trend)) {
      if (input$Trend == T) {
        # Add a trendline for each set of points (colored by method)
        p <- p +
          geom_smooth(
            aes(color = method),
            method = lm,
            se = FALSE,
            linewidth = 1,
            position = position_dodge(width = 0.3)
          )
      }
    }
    if (input$Target == T) {
      if (x_var_data() == "year") {
        p <- p +
          # Add a horizontal dotted line at the target population value
          geom_hline(
            aes(
              yintercept = target,
              linetype = "Target",
              color = "Target"
            ),
            linetype = 2,
            color = 'red',
            linewidth = 1
          )
      } else {
        p <- p +
          # add red point for target number
          geom_point(
            aes(
              x = x,
              y = target,
              shape = "Target"
            ),
            shape = 8,
            size = 2,
            color = "red"
          )
      }
    }
    p <- p +
      # Add theme elements
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.key.size = unit(0.75, 'in'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
    # Add points on top of everything
    if (input$EPU == "All" & input$year == "All") {
      p <- p +
        geom_point(
          shape = 21,
          size = 2.5,
          position = position_dodge(width = 0.3)
        )
    } else {
      p <- p +
        geom_point(
          shape = 21,
          size = 3.5,
          position = position_dodge(width = 0.3)
        )
    }
    p
  })
  
  ## Sidebar UI objects ----
  ### Year_select ----
  output$Year_select <- renderUI({
    selectInput(
      "year",
      label = strong("Year"),
      choices = year_choices(),
      selected = if (input$tables_plots == "Table") {
        "All"
      } else {
        selected_year()
      }
    )
  })
  
  ### EPU_select ----
  output$EPU_select <- renderUI({
    selectInput(
      "EPU",
      label = strong("EPU"),
      choices = EPU_choices(),
      selected = "All"
    )
  })
  
  ### Method_select ----
  output$Method_select <- renderUI ({
    req(results())
    selectInput(
      "method",
      label = strong("Method"),
      choices = list("All", "Model", "Standard"),
      selected = "All"
    )
  })
  
  ### CI_check ----
  output$CI_check <- renderUI ({
    req(results())
    checkboxGroupInput(
      "CI",
      strong("Confidence Intervals"),
      choices = list("95%" = 1, "50%" = 2),
      selected = c(1, 2)
    )
  })
  
  ### other_options ----
  output$other_options <- renderUI({
    req(results())
    if (input$year == "All") {
      tagList(
        strong("Other Options"),
        checkboxInput("Target", "Target population", value = F),
        checkboxInput("Trend", "Trendline", value = F)
      )
    } else {
      tagList(
        strong("Other Options"),
        checkboxInput("Target", "Target population", value = F)
      )
    }
  })
  
  ### Export table ----
  output$export_table <- downloadHandler(
    filename = function() {
      paste(if (input$year == "All") {
        "AllYears"
      } else {
        input$year
      },
      if (input$EPU == "All") {
        "AllEPUs"
      } else {
        input$EPU
      },
      ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(results_table(), file, row.names = FALSE)
    }
  )
  output$Export_table <- renderUI ({
    req(results())
    downloadButton("export_table",
                   "Export")
  })
  
  ### Export plot ----
  export_plot_height <- reactive({
    if (input$year == "All" & input$EPU == "All") {
      2.75 * (ceiling(length(EPU_choices()) - 1) / 3)
    } else {
      9
    }
    
  })
  
  output$export_plot <- downloadHandler(
    filename = function() {
      paste(if (input$year == "All") {
        "AllYears"
      } else {
        input$year
      },
      if (input$EPU == "All") {
        "AllEPUs"
      } else {
        str_remove_all(input$EPU, "[ -]")
      },
      if (input$method == "All") {
        "AllMethods"
      } else {
        input$method
      },
      if (!is.null(input$CI)) {
        if (all(c(1, 2) %in% input$CI)) {
          "AllCI"
        } else if (1 %in% input$CI) {
          "95CI"
        } else if (2 %in% input$CI) {
          "50CI"
        }
      },
      if (input$Target == T) {
        "withTarget"
      },
      if (input$year == "All" & !is.null(input$Trend)) {
        if (input$Trend == T) {
          "withTrend"
        }
      },
      ".png",
      sep = "_")
    },
    content = function(file) {
      ggsave(
        file,
        plot = results_plot(),
        device = "png",
        width = 12,
        height = export_plot_height(),
        units = "in"
      )
    }
  )
  output$Export_plot <- renderUI ({
    req(results())
    downloadButton("export_plot",
                   "Export")
  })
  
  ### Display sidebar ----
  output$sidebarText <- renderUI({
    req(input$tables_plots)
    if (input$tables_plots == "Table") {
      tagList(
        br(),
        fileInput("results_file", "Upload Results CSV", accept = ".csv"),
        uiOutput("Year_select"),
        uiOutput("EPU_select"),
        br(),
        uiOutput("Export_table")
      )
    } else if (input$tables_plots == "Plot") {
      tagList(
        br(),
        fileInput("results_file", "Upload Results CSV", accept = ".csv"),
        uiOutput("Year_select"),
        uiOutput("EPU_select"),
        uiOutput("Method_select"),
        uiOutput("CI_check"),
        uiOutput("other_options"),
        br(),
        uiOutput("Export_plot")
      )
    }
  })
  
  
  ## Main panel UI objects -----
  # Text for when no results file is uploaded
  output$no_file <- renderText("No file selected")
  
  ### Table tab ----
  # Display table
  output$table <- renderDataTable({
    display_table()
  })
  # Render UI
  output$tableUI <- renderUI({
    if (!is.null(results_path())) {
      dataTableOutput("table", width = "100%", height = "100%")
    } else {
      tagList(
        br(),
        textOutput("no_file")
      )
    }
  })
  
  ### Plot tab ----
  # Create reactive plot height object
  plot_height <- reactive({
    req(input$method)
    if (input$year == "All" & input$EPU == "All") {
      paste0(ceiling((length(
        EPU_choices()
      ) - 1) / 3) * 25, "vh")
    } else {
      "80vh"
    }
  })
  # Display plot
  output$plot <- renderPlot(results_plot(), res = 96)
  
  # Render UI
  output$plotUI <- renderUI({
    if (!is.null(results_plot())) {
      req(plot_height())
      plotOutput("plot", width = "100%", height = plot_height())
    } else {
      tagList(
        br(),
        textOutput("no_file")
      )
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

# examples

# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer
