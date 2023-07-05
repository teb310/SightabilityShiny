# Elk Population Estimator

list.of.packages <- c("shiny", "shinyjs", "bslib", "DT", "bayesplot", "tidyverse", "lubridate","chron","rgdal", "readxl", "Cairo", "rjags","coda","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
source("helpers.R")

# Set your working directory paths and survey data file path
wd <- getwd()
input_wd <- paste0(getwd(),"/input")
output_wd <- paste0(getwd(),"/output")

# create input and output folders if you haven't already
dir.create(paste0(wd,"/input"))
dir.create(paste0(wd,"/output"))

# Define UI ----
ui <- fluidPage(
  bootstrapLib(bs_theme(bootswatch = "spacelab")),
  useShinyjs(),
  tags$style(
    HTML("
      .plot-container {
        width: 100%;
        height: 100vh;
      }
    ")
  ),
  titlePanel("Roosevelt Elk Abundance Estimator"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Load",
      sidebarLayout(
        sidebarPanel(
          br(),
          fileInput("data_file", "Upload Excel File", accept = ".xlsx"),
          actionButton("run_script", "Run Script"),
          br(),
          textOutput("script_status")
        ),
        mainPanel(
          h3("Welcome"),
          p("This app is designed to make statistical modelling of elk survey data quick and easy.
            First, upload your formatted Excel datasheet on the left. Once the model has run, you will
            see a new file called 'Results_(date-time).csv' in the output folder of this app's directory. Load that
            file into the Results tab to view and export your modelled elk abundance estimates."),
          div(id = "plot-container",
              img(src = "elk_galore.jpg", 
                  style = "width: 90%; height: 90%; display: block; margin-left: auto; margin-right: auto;")
          )
        )
      )
    ),
    tabPanel(
      "Results",
      sidebarLayout(
        sidebarPanel(
          uiOutput("sidebarText")
        ),
        mainPanel(
          h3("Results"),
          tabsetPanel(
            id="tables_plots",
            type="tabs",
            tabPanel("Table",
                     div(class = "plot-container",
                         dataTableOutput("table", width = "100%", height = "100%")
                         )
                     ),
            tabPanel("Plot",
                     div(class = "plot-container",
                         plotOutput("plot", width = "100%", height = "80%")
                     )
                )
          )
        )
      )
    )
  )
)
    

# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive values
  file_path <- reactiveVal(NULL)
  results_path <- reactiveVal(NULL)
  script_finished <- reactiveVal(FALSE)
  ucl <- reactiveVal(NULL)
  lcl <- reactiveVal(NULL)

  # Load data from the uploaded Excel file
  observeEvent(input$data_file, {
      file_path(input$data_file$datapath)
  })
  
  # Run the R script on the uploaded file
  observeEvent(input$run_script, {
    if (!is.null(file_path())) {
      # Show progress bar while running the model
      withProgress(message = "Running the model...", detail = "This can take an hour or more.", {
        source("Bayesian_condensed.R", local = TRUE, keep.source = FALSE, encoding = "UTF-8")
      script_finished(TRUE)
      })
      }
  })
  
  # Display script status
  output$script_status <- renderText({
    if (script_finished()) {
      "Script finished running!"
    # } else if (input$run_script) {
    #   "Running...this can take an hour or more"
    } else {
      "Upload a file and run the script."
    }
  })
  

  
  #load results data
  results <- reactive({
    req(input$results_file)
    read.csv(input$results_file$datapath, header = TRUE)
  })

  
  # generate the choices for the "year" selectInput
  year_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    years <- unique(results()[["year"]])
    c("All", years)
  })
  
  # Get the maximum selected year
  selected_year <- reactive({
    req(results())  # Ensure a file is uploaded
    max(results()[["year"]])
  })
  
  output$Year_select <- renderUI({
    # Use the reactive values in the selectInput
    selectInput(
      "year",
      label = "Year",
      choices = year_choices(),
      selected = selected_year()
    )
  })
  
  # generate the choices for the "EPU" selectInput
  EPU_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    EPUs <- sort(unique(results()[["EPU"]]))
    c("All", EPUs)
  })
  
  output$EPU_select <- renderUI({
    # Use the reactive values in the selectInput
    selectInput(
      "EPU",
      label = "EPU",
      choices = EPU_choices(),
      selected = "All"
    )
  })
  output$Method_select <- renderUI ({
    req(results())
  selectInput(
    "method",
    label = "Method",
    choices = list("All", "Model", "Standard"),
    selected = "All"
  )
  })
  output$CI_check <- renderUI ({
    req(results())
    br()
    checkboxGroupInput(
      "CI",
      h5(strong("Confidence Intervals")),
      choices = list("95%" = 1, "50%" = 2),
      selected = c(1, 2)
    )
  })
  output$Trend_check <- renderUI ({
    req(results())
    br()
    h5(strong("Other Options"))
    checkboxInput("Trend", "Trendline", value = F)
  })
  output$Target_check <- renderUI ({
    req(results())
    checkboxInput("Target", "Target population", value = F)
  })
  output$Export_table <- renderUI ({
    req(results())
    downloadButton("export_table",
                   "Export")
  })
  output$Export_plot <- renderUI ({
    req(results())
    downloadButton("export_plot",
                   "Export")
  })
  output$sidebarText <- renderUI({
    if (input$tables_plots == "Table") {
      tagList(
        br(),
        fileInput("results_file", "Upload Results CSV", accept = ".csv"),
        uiOutput("Year_select"),
        uiOutput("EPU_select"),
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
        h5("Other Options"),
        uiOutput("Trend_check"),
        uiOutput("Target_check"),
        uiOutput("Export_plot")
      )
    }
  })  
  
  facet <- reactive({
    # faceting depends on year selection: if all, then facet by EPU, if one, facet by year (really just labelling top of graph with year)
    req(input$year)
    if (input$year == "All") {
      "EPU"
    } else {
      "year"
      }
    })
  
  x_var_data <- reactive({
    req(input$year)
    if(input$year == "All") {
      "year"
    } else {
      "EPU"
    }
  })
  
  ci_data <- reactive({
    req(results(), input$CI) # Ensure results are available
    if (!is.null(input$CI) & 1 %in% input$CI & 2 %in% input$CI) {
      list(
        ucl = c(results()[["ucl_95"]], results()[["ucl_50"]]),
        lcl = c(results()[["lcl_95"]], results()[["lcl_50"]])
      )
    } else if (1 %in% input$CI) {
      list(
        ucl = results()[["ucl_95"]],
        lcl = results()[["lcl_95"]]
      )
    } else if (2 %in% input$CI) {
      list(
        ucl = results()[["ucl_50"]],
        lcl = results()[["lcl_50"]]
      )
    } else {
      list(
        ucl = NULL,
        lcl = NULL
      )
    }
  })
  
  # Construct table
results_table <- reactive({
    req(results(), input$year, input$EPU, input$method)
    data <- results()
    if (input$year != "All") {
      data <- filter(data, year == input$year)
    }
    if (input$EPU != "All") {
      data <- filter(data, EPU == input$EPU)
    }
    if (input$method != "All") {
      data <- filter(data, method == input$method)
    }
    table <- data %>%
      pivot_wider(
        id_cols = c(year, EPU, min_count, target),
        names_from = "method",
        values_from = "estimate"
      ) %>%
      inner_join(
        data %>% filter(!is.na(lcl_50)) %>% select(year, EPU, lcl_50, ucl_50, lcl_95, ucl_95),
        by = c("year", "EPU")
      ) %>%
      mutate(
        `50% Confidence Interval` = paste0(lcl_50, " to ", ucl_50),
        `95% Confidence Interval` = paste0(lcl_95, " to ", ucl_95)
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
        `95% Confidence Interval`
      )
    table
  })

# Display table
output$table <- renderDataTable(results_table())
  
  # Export table
  output$export_table <- downloadHandler(
    filename = function() {
      paste(
        if(input$year=="All"){
          "All_years"
        } else {
          input$year
        }, 
        if(input$EPU=="All"){
          "All_EPUs"
        } else {
          input$EPU
        },
        if(input$method=="All"){
          "All_methods"
        } else {
          input$method
        },
        ".csv", sep = "_")    },
    content = function(file) {
      write.csv(results_table(), file, row.names = FALSE)
    }
  )
  
  # contruct plot

  results_plot <- reactive({
    req(results(), input$year, input$EPU, input$method, x_var_data())
    ci <- ci_data()
    x_var <- x_var_data()
    facet_col <- facet()
    data <- results()
    if(input$year != "All") {
      data <- filter(data, year==input$year)
    }
    if(input$EPU != "All") {
      data <- filter(data, EPU==input$EPU)
    }
    if(input$method !="All") {
      data <- filter(data, method==input$method)
    }
    x_data <- data[[x_var]]
    p <- ggplot(data, aes(x=.data[[x_var]], y=estimate, fill =method)) +
      labs(fill = "Method", color = "Method", shape = "Target") +
      # name y axis
      scale_y_continuous("Estimated Abundance") +
      # use greyscale for point fill & color
      scale_fill_grey(start = 0, end = 0.7) +
      scale_color_grey(start = 0, end = 0.7) +
      # facet wrap
      facet_wrap(as.formula(paste("~", facet())), scales="free", ncol=4, )
    if (x_var == "year") {
      # make sure there's only one label per year
      p <- p +
      scale_x_continuous("Year", breaks = unique(data$year))
    } else {
      # name x axis
      p <- p +
      scale_x_discrete("Elk Population Unit")
    }
    # add in 95% & 50% confidence intervals as dotted lines    
    if (!is.null(ci$ucl) & 1 %in% input$CI) {
      p <- p +
        # 95% (dashed)
        geom_linerange(aes(.data[[x_var]], ymin = lcl_95, ymax = ucl_95), linetype = 2, linewidth = 1, position = position_dodge(width = 0.3))
    }
    if (!is.null(ci$ucl) & 2 %in% input$CI) {
      p <- p +
        # 50% (solid)
        geom_linerange(aes(.data[[x_var]], ymin = lcl_50, ymax = ucl_50), linewidth = 1, position = position_dodge(width = 0.3))
    }
    if(x_var == "year" & input$Trend == T){
      # Add a trendline for each set of points (colored by method)
      p <- p +
        geom_smooth(aes(color=method), method=lm, se=FALSE, linewidth=1, position = position_dodge(width=0.3))
    }
    if (input$Target == T) {
      if (x_var == "year") {
        p <- p +
          # Add a horizontal dotted line at the target population value
          geom_hline(aes(yintercept = target, linetype = "Target", color = "Target"), linetype = 2, color = 'red', linewidth = 1)
      } else {
        p <- p +
          # add red point for target number
          geom_point(aes(x = .data[[x_var]], y = target, shape = "Target"), shape=8, size = 3, color = "red")
      }
    }
    p <- p +
      # Add theme elements
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text.x = element_text(size=14, angle = 45, vjust = .6),
            axis.title.x = element_text(size=16), axis.text.y = element_text(size=14), 
            axis.title.y = element_text(size=16), legend.key.size = unit(0.75, 'in'), 
            legend.text = element_text(size=12), legend.title = element_text(size=14),
            strip.text = element_text(size = 14)) +
      # Add points on top of everything
      geom_point(shape=21, size=4, position = position_dodge(width = 0.3))
    p
  })
  
  # Display plot
  output$plot <- renderPlot(results_plot(), res = 96)

  output$export_plot <- downloadHandler(
    filename = function() {
      paste(
        if(input$year=="All"){
          "All_years"
        } else {
          input$year
        }, 
        if(input$EPU=="All"){
          "All_EPUs"
        } else {
          input$EPU
        },
        if(input$method=="All"){
          "All_methods"
        } else {
          input$method
        },
        ".png", sep = "_")
    },
    content = function(file) {
      ggsave(file, plot = results_plot(), device = "png",
             width = 12, height = 9, units="in")
    }
  )
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
