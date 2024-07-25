# Elk Population Estimator
# Setup -----------------------------------------------------------------------

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
library("readxl")
library("rjags")
library("coda")
library("truncnorm")
library("doParallel")
library("nimble")
library("xtable")
library("statip")
library("R2jags")

# Set your working directory paths and survey data file path
wd <- getwd()
input_wd <- paste0(wd, "/input")
output_wd <- paste0(wd, "/output")

# create input and output folders if you haven't already
dir.create(paste0(wd, "/input"))
dir.create(paste0(wd, "/output"))

# set tz (computer automatically does UTM)
Sys.setenv(TZ = "America/Los_Angeles")

# Define UI -------------------------------------------------------------------
ui <- fluidPage(
  bootstrapLib(bs_theme(bootswatch = "spacelab")),
  useShinyjs(),
  titlePanel("Roosevelt Elk Abundance Estimator"),
  fluidRow(column(
    width = 12,
    htmlOutput("last_updated", style = "position: absolute; top: 1%; right: 1%; font-size: 14px;")
  )),
  tabsetPanel(
    type = "tabs",
    tabPanel("Welcome",
             sidebarLayout(
               sidebarPanel(
                 br(),
                 h3("Welcome!"),
                 br(),
                 p(
                   "This app is designed to make statistical modelling of elk survey data quick and easy.
            First, format your survey data using the template."
                 ),
                 br(),
                 downloadButton("download_template", "Download Template")
               ),
               mainPanel(div(
                 id = "plot-container",
                 img(src = "elk_galore.JPG",
                     style = "width: 100%; height: 100%; display: block; margin-left: auto; margin-right: auto;")
               ))
             )),
    tabPanel(
      "Model",
      tags$head(tags$style(
        HTML("
      #download_results {
        visibility: hidden;
      }
    ")
      )),
      sidebarLayout(
        sidebarPanel(
          br(),
          fileInput("data_file", "Upload Excel File", accept = ".xlsx"),
          actionButton("run_script", "Run Script"),
          br(),
          htmlOutput("script_status"),
          downloadButton("download_results")
        ),
        mainPanel(
          br(),
          h3("Run the model"),
          br(),
          p(
            "Now that your data is formatted, upload your Excel datasheet
                   on the left. After pressing 'Run model', progress updates will
                   be loaded to this screen. The model may take over an hour to run.
                   Once the model is finished running, the results
                   file will automatically download as a CSV. Load the file in the
                   Results tab to view your modelled elk abundance estimates by population unit.
                   Estimates include population abundance with measures of precision (coefficient
                   of variation, 95% and 50% confidence intervals), calf:cow and bull:cow ratios,
                   and percent branch-antlered males."
          ),
          br(),
          p(
            "There may be long pauses between progress updates. As long as the clock in the lower-left corner is running and the left panel says 'Script running...', the model is working."
          ),
          htmlOutput("model_progress"),
          br(),
          htmlOutput("model_error"),
          verbatimTextOutput("clock_output"),
          tags$style(
            HTML("#clock_output {
                       position: absolute;
                       bottom: 10px;
                       left: 10px;
                       }")
          )
        )
      )
    ),
    tabPanel("Results",
             sidebarLayout(
               sidebarPanel(uiOutput("sidebarText"), width = 3),
               mainPanel(
                 width = 9,
                 h3("Results"),
                 tabsetPanel(
                   id = "tables_plots",
                   type = "tabs",
                   tabPanel("Table",
                            tags$head(tags$style(
                              HTML(
                                "
                                     .datatables {
                                     font-size: 0.9vw;
                                     }

                                     @media screen and (max-width: 1333px) {
                                        .datatables {
                                          font-size: 12px;
                                    }
                                     "
                              )
                            )),
                            uiOutput("tableUI")),
                   tabPanel("Plot",
                            uiOutput("plotUI"))
                 )
               )
             ))
  )
)


# Define server logic ---------------------------------------------------------
server <- function(input, output, session) {
  # Welcome tab ---------------------------------------------------------------
  
  # Download template
  output$download_template <- downloadHandler(
    filename = function() {
      "sightability_template.xlsx"
    },
    content = function(file) {
      file.copy("www/sightability_template.xlsx", file)
    }
  )
  
  output$last_updated <- renderUI({
    paste("App last updated:", format(file.info("app.R")$mtime, "%b %e %Y"))
  })
  
  # Model tab -----------------------------------------------------------------
  
  ## Clock ------
  # clock to prevent timing out
  clock <- reactive({
    invalidateLater(1000)
    format(Sys.time(), "%H:%M:%S %Z")
  })
  output$clock_output <- renderText(clock())
  
  ## Reset txt files ----
  writeLines("FALSE", "running.txt")
  writeLines("FALSE", "done.txt")
  writeLines("", "progress.txt")
  writeLines("", "error.txt")
  
  ## RVs ------
  # default values
  rv <- reactiveValues(
    textstream = c(""),
    errorstream = c(""),
    running = FALSE,
    done = FALSE,
    error = FALSE,
    results_filename = NULL,
    results_filepath = NULL,
    results_file = NULL
  )
  file_path <- reactiveVal(NULL)
  
  # continuously evaluate
  observe({
    clock()
    rv$running <- paste(readLines("running.txt", warn = F))
    if (isolate(rv$running)) {
      rv$textstream <-
        paste(readLines("progress.txt"), collapse = "<br/>")
      rv$errorstream <-
        paste(readLines("error.txt"), collapse = "<br/>")
    }
    if (rv$errorstream != "") {
      rv$error <- TRUE
      rv$running <- FALSE
    }
    rv$done <- paste(readLines("done.txt", warn = F))
  })
  
  
  ## File upload ----
  # Set the path of the uploaded Excel file
  observeEvent(input$data_file, {
    file_path(input$data_file$datapath)
    # reset rvs
    writeLines("FALSE", "done.txt")
    rv$textstream <- ""
    rv$errorstream <- ""
  })
  # Show the run model button only if a file is uploaded
  observe({
    shinyjs::hide("run_script")
    if (!is.null(file_path()) & rv$running==FALSE) {
      shinyjs::show("run_script")
    }
  })
  
  ## Run model ----
  # Run the R script on the uploaded file
  observeEvent(input$run_script, {
    rv$results_file <- NULL
    system2("Rscript",
            args = c("model_stratified.R", file_path()),
            wait = F)
  })
  # Show console outputs
  output$model_progress <- renderUI({
    HTML(paste0("<div style='color: #00AA10;'>", rv$textstream, "</div>"))
  })
  output$model_error <- renderUI({
    HTML(paste0("<div style='color: #FF0000;'>", rv$errorstream, "</div>"))
  })
  
  
  # Show script status
  output$script_status <- renderUI({
    if (rv$running) {
      status <- "Script running..."
    } else if (rv$error) {
      status <- "Error"
    } else if (rv$done) {
      status <- "Script finished running! Check your downloads for the results file."
    } else {
      status <- ""
    }
    HTML(paste0("<div style='color:grey;'>", status, "</div>"))
  })
  
  ## Download results ----
  # retrieve results file names once the script is done running
  observe({
    if (rv$done) {
      # first need to sort the files by modified date/time
      files <- list.files(paste0(getwd()), full.names = TRUE, recursive = T, pattern = "Results")
      file_info <- file.info(files)
      file_info$file_name <- rownames(file_info)
      sorted_files <- files[order(file_info$mtime, decreasing = T)]
      # then retrieve newest file
      rv$results_filepath <-
        paste0(sorted_files[1])
      # cut out the rest of the pathname to get just the filename
      rv$results_filename <-
        str_extract(rv$results_filepath, "(?<=output/).+")
      rv$results_file <- read_csv(rv$results_filepath)
    }
  })
  
  # downloadHandler
  output$download_results <- downloadHandler(
    filename = function() {
      # Set the filename for the downloaded file
      paste0(rv$results_filename)
    },
    content = function(file) {
      # Prepare the data for download (use the data from reactiveValues)
      write.csv(rv$results_file, file, row.names = F)
    }
  )
  
  # press download once the results file is ready
  observe({
    if (!is.null(rv$results_file)) {
      runjs("$('#download_results')[0].click();")
    }
  })
  
  
  # Results tab ---------------------------------------------------------------
  
  # Reactive value defaults
  results_path <- reactiveVal(NULL)
  results_reactive <- reactiveValues(data = NULL)
  values <- reactiveValues()
  
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
  
  ### year ----
  year_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    years <- unique(results()$year)
    years
  })
  
  year_span <- reactive({
    c(min(year_choices()), max(year_choices()))
  })
  
  selected_year <- reactiveVal()
  
  # update when it's changed in either tab
  observeEvent(input$year, {
    selected_year(c(input$year[1], input$year[2]))
  })
  
  ### EPU ----
  EPU_choices <- reactive({
    req(results())  # Ensure a file is uploaded
    EPUs <- sort(unique(results()$EPU))
    c("All", EPUs)
  })
  
  selected_EPU <- reactiveVal()
  
  # update when it's changed in either tab
  observeEvent(input$EPU, {
    selected_EPU(input$EPU)
  })
  
  ### method ----
  selected_method <- reactiveVal("All")
  
  # update when it's changed in either tab
  observeEvent(input$method, {
    selected_method(input$method)
  })
  
  ### target ----
  selected_target <- reactiveVal(FALSE)
  
  # update when it's changed in either tab
  observeEvent(input$target, {
    selected_target(input$target)
  })
  
  ### trend ----
  selected_trend <- reactiveVal(FALSE)
  
  # update when it's changed in either tab
  observeEvent(input$trend, {
    selected_trend(input$trend)
  })
  
  ### facet ----
  facet <- reactive({
    # faceting depends on year selection: if all, then facet by EPU, if one, facet by year (really just labelling top of graph with year)
    req(input$year)
    if (input$year[1] != input$year[2]) {
      "EPU"
    } else {
      "year"
    }
  })
  
  ### x_var_data ----
  x_var_data <- reactive({
    req(input$year)
    if (input$year[1] == input$year[2]) {
      "EPU"
    } else {
      "year"
    }
  })
  
  ### CI ----
  selected_CI <- reactiveVal(c("1", "2"))
  
  # update when it's changed in either tab
  observeEvent(input$CI, {
    selected_CI(input$CI)
  })
  
  ci_data <- reactive({
    if (1 %in% input$CI & 2 %in% input$CI) {
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
  
  ## Table ----
  results_table <- reactive({
    req(results(), input$year, input$EPU)
    table_data <- results()
    # only show the years selected
    table_data <-
      filter(table_data,
             table_data$year %in% seq(input$year[1], input$year[2]))
    # only show the EPUs selected
    if (input$EPU != "All") {
      table_data <- filter(table_data, table_data$EPU == input$EPU)
    }
    table <- table_data %>%
      # make it wide
      pivot_wider(
        id_cols = c(year, EPU, min_count, target),
        names_from = "method",
        values_from = "estimate"
      ) %>%
      # add the rest of the columns back in
      inner_join(
        table_data %>% filter(!is.na(lcl_50)) %>% select(
          year,
          EPU,
          lcl_50,
          ucl_50,
          lcl_95,
          ucl_95,
          calf_cow,
          bull_cow,
          percent_branched
        ),
        by = c("year", "EPU")
      ) %>%
      # add exclamation when estimate < min_count
      mutate(exclamation = ifelse(Model < min_count, "!!", "")) %>%
      select(
        year,
        EPU,
        target,
        min_count,
        Standard,
        Model,
        exclamation,
        lcl_50,
        ucl_50,
        lcl_95,
        ucl_95,
        calf_cow,
        bull_cow,
        percent_branched
      )
    # arrange it by most recent year, then alphabetically by EPU
    arrange(table, desc(year), EPU)
  })
  # Further adjustments
  display_table <- reactive({
    req(results_table())
    table <- results_table()
    table <- table %>%
      # Calculate columns
      mutate(
        `50% confidence interval` = paste0(lcl_50, " to ", ucl_50),
        `95% confidence interval` = paste0(lcl_95, " to ", ucl_95),
        `Calves per 100 cows` = round(calf_cow, digits = 0),
        `Bulls per 100 cows` = round(bull_cow, digits = 0),
        `Percent branch-antlered males` = round(percent_branched, digits =
                                                  0)
      ) %>%
      # make names intuitive
      select(
        Year = year,
        EPU,
        `Population target` = target,
        `Minimum count` = min_count,
        `Standard estimate` = Standard,
        `Model estimate` = Model,
        ` ` = exclamation,
        `50% confidence interval`,
        `95% confidence interval`,
        `Calves per 100 cows`,
        `Bulls per 100 cows`,
        `Percent branch-antlered males`
      )
    table
  })
  
  ## SBOT Table ----
  SBOT_table <- reactive({
    req(results(), input$year, input$EPU)
    table_data <- results()
    table <- table_data %>%
      # make it wide
      pivot_wider(
        id_cols = c(year, EPU, min_count, target, cows_observed, bulls_observed, yearlings_observed, calves_observed, unclassified_observed),
        names_from = "method",
        values_from = "estimate"
      ) %>%
      # add the rest of the columns back in
      inner_join(
        table_data %>% filter(!is.na(lcl_50)) %>% select(
          year,
          EPU,
          lcl_50,
          ucl_50,
          lcl_95,
          ucl_95,
          calf_cow,
          bull_cow,
          percent_branched
        ),
        by = c("year", "EPU")
      ) %>%
      select(
        EPU,
        year,
        cows_observed,
        bulls_observed,
        yearlings_observed,
        calves_observed,
        unclassified_observed,
        total_observed = min_count,
        population_target = target,
        standard_estimate = Standard,
        model_estimate = Model,
        lower_50_confidence = lcl_50,
        upper_50_confidence = ucl_50,
        lower_95_confidence = lcl_95,
        upper_95_confidence = ucl_95,
        estimated_calf_per_100_cow = calf_cow,
        estimated_bull_per_100_cow = bull_cow
      )
    # arrange it by ascending year, then alphabetically by EPU
    arrange(table, year, EPU)
    table
  })
  
  ## Plot ----
  ### setup data ----

results_plot <- eventReactive(input$replot_button, {
  isolate({
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
      if (any(input$year != year_span())) {
        plot_data <-
          filter(plot_data,
                 plot_data$year %in% seq(input$year[1], input$year[2]))
      }
      if (input$EPU != "All") {
        plot_data <- filter(plot_data, plot_data$EPU == input$EPU)
      }
      if (input$method != "All") {
        plot_data <- filter(plot_data, plot_data$method == input$method)
      }
      if (x_var == "EPU") {
        selected_trend(FALSE)
      }
      x_data <- plot_data[[x_var]]
      
      ### create plot ----
      p <-
        ggplot(plot_data, aes(x = x, y = estimate, fill = method)) +
        # color points by method always
        labs(fill = "Method") +
        # name y axis
        scale_y_continuous("Estimated Abundance", limits = c(0, NA)) +
        # use greyscale for point fill & color (grey is better than white)
        scale_fill_grey(start = 0, end = 1) +
        scale_color_grey(start = 0, end = 0.7) +
        # facet wrap
        facet_wrap(as.formula(paste("~", facet())), scales = "free", ncol =
                     3)
      
      ### axis settings ----
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
      
      ### plot CIs ----
      if (!is.null(ci_data()$ucl) & !is.null(input$CI)) {
        if (x_var_data() == "EPU") {
          if (1 %in% input$CI) {
            p <- p +
              # 95% (dashed)
              geom_linerange(
                aes(
                  x,
                  ymin = lcl_95,
                  ymax = ucl_95,
                  linetype = "95% CI"
                ),
                linewidth = 1,
                position = position_dodge(width = 0.3)
              )
          }
          if (2 %in% input$CI) {
            p <- p +
              # 50% (solid)
              geom_linerange(
                aes(
                  x,
                  ymin = lcl_50,
                  ymax = ucl_50,
                  linetype = "50% CI"
                ),
                linewidth = 1,
                position = position_dodge(width = 0.3)
              )
          }
        } else {
          if (input$target == T) {
            if (1 %in% input$CI) {
              p <- p +
                # 95% (dashed)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_95,
                    ymax = ucl_95,
                    linetype = "95% CI",
                    color = "95% CI"
                  ),
                  linewidth = 1,
                  show.legend = F,
                  position = position_dodge(width = 0.3)
                )
            }
            if (2 %in% input$CI) {
              p <- p +
                # 50% (solid)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_50,
                    ymax = ucl_50,
                    linetype = "50% CI",
                    color = "50% CI"
                  ),
                  linewidth = 1,
                  show.legend = F,
                  position = position_dodge(width = 0.3)
                )
            }
          } else if (selected_trend() == T) {
            if (1 %in% input$CI) {
              p <- p +
                # 95% (dashed)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_95,
                    ymax = ucl_95,
                    linetype = "95% CI",
                    color = "95% CI"
                  ),
                  linewidth = 1,
                  position = position_dodge(width = 0.3)
                )
            }
            if (2 %in% input$CI) {
              p <- p +
                # 50% (solid)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_50,
                    ymax = ucl_50,
                    linetype = "50% CI",
                    color = "50% CI"
                  ),
                  linewidth = 1,
                  position = position_dodge(width = 0.3)
                )
            }
          } else {
            if (1 %in% input$CI) {
              p <- p +
                # 95% (dashed)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_95,
                    ymax = ucl_95,
                    linetype = "95% CI"
                  ),
                  linewidth = 1,
                  position = position_dodge(width = 0.3)
                )
            }
            if (2 %in% input$CI) {
              p <- p +
                # 50% (solid)
                geom_linerange(
                  aes(
                    x,
                    ymin = lcl_50,
                    ymax = ucl_50,
                    linetype = "50% CI"
                  ),
                  linewidth = 1,
                  position = position_dodge(width = 0.3)
                )
            }
          }
        }
      }
      
      ### plot target ----
      if (input$target == T) {
        if (x_var_data() == "year") {
          # Add a horizontal dotted line at the target population value
          p <- p +
            geom_hline(aes(
              yintercept = target,
              linetype = "Target",
              color = "Target"
            ),
            linewidth = 1)
        } else {
          p <- p +
            # target point
            geom_point(aes(
              x = x,
              y = target,
              color = "Target"
            ),
            size = 2,
            shape = 8)
        }
      }
      ### plot trend ----
      if (x_var_data() == "year" & !is.null(input$trend)) {
        if (input$trend == T) {
          # Add a trendline for each set of points (colored by method)
          p <- p +
            geom_smooth(
              aes(color = method, linetype = method),
              method = lm,
              se = FALSE,
              linewidth = 1,
              show.legend = F,
              position = position_dodge(width = 0.3)
            )
        }
      }
      ### plot points ----
      # Add points on top of everything
      if (input$EPU == "All" & input$year[1] != input$year[2]) {
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
      ### Legend ----
      if (selected_trend() == T) {
        p <- p +
          # add scaling info for legend items
          scale_linetype_manual(values = c(
            "Model" = 1,
            "Standard" = 1,
            "Target" = 2,
            "95% CI" = 3,
            "50% CI" = 1
          )) +
          scale_color_manual (
            values = c(
              "Model" = "black",
              "Standard" = "grey",
              "Target" = "red",
              "95% CI" = "steelblue",
              "50% CI" = "steelblue"
            )
          ) +
          labs(linetype = "",
               color = "")
      } else {
        if (input$target == F) {
          p <- p +
            # add scaling info for legend items
            scale_linetype_manual(values = c("95% CI" = 3,
                                             "50% CI" = 1)) +
            labs(linetype = "")
        } else if (x_var_data() == "EPU") {
          p <- p +
            scale_linetype_manual(values = c("95% CI" = 3,
                                             "50% CI" = 1)) +
            scale_color_manual(values = c("Target" = "red")) +
            labs(linetype = "",
                 color = "")
        } else {
          p <- p +
            scale_linetype_manual(values = c(
              "95% CI" = 3,
              "50% CI" = 1,
              "Target" = 2
            )) +
            scale_color_manual(values = c(
              "95% CI" = "black",
              "50% CI" = "black",
              "Target" = "red"
            )) +
            labs(linetype = "",
                 color = "")
        }
      }
      ### final elements ----
      
      p <- p  +
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
      
      p
    })
})
  
    # Render plot
  observeEvent(input$replot_button, {
    output$plot <- renderPlot({
    isolate(results_plot())
  })
    
    # Create reactive plot height object
    plot_height <- isolate({
      req(input$method)
      if (input$year[1] != input$year[2] & input$EPU == "All") {
        paste0(ceiling((length(
          EPU_choices()
        ) - 1) / 3) * 25, "vh")
      } else {
        "80vh"
      }
    })
    # Render plotUI
    output$plotUI <- renderUI({
        plotOutput("plot", width = "100%", height = plot_height)
    })
  })

  
  ## Sidebar UI objects ----
  ### Year slider ----
  output$Year_select <- renderUI({
    sliderInput(
      "year",
      label = strong("Year"),
      min = min(year_choices()),
      max = max(year_choices()),
      value = if (is.null(selected_year())) {
        c(min(year_choices()), max(year_choices()))
      } else {
        selected_year()
      },
      step = 1,
      sep = ""
    )
  })
  
  observe({
    updateSliderInput(
      session,
      "year",
      min = min(year_choices()),
      max = max(year_choices()),
      value = selected_year()
    )
  })
  
  ### EPU_select ----
  output$EPU_select <- renderUI({
    selectInput(
      "EPU",
      label = strong("EPU"),
      choices = EPU_choices(),
      selected = selected_EPU()
    )
  })
  
  observe({
    updateSelectInput(session, "EPU", selected = selected_EPU())
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
  
  observe({
    updateSelectInput(session, "method", selected = selected_method())
  })
  
  ### CI_check ----
  output$CI_check <- renderUI ({
    req(results())
    checkboxGroupInput(
      "CI",
      strong("Confidence Intervals"),
      choices = list("95%" = 1, "50%" = 2),
      selected = selected_CI()
    )
  })
  
  observe({
    updateSelectInput(session, "CI", selected = selected_CI())
  })
  
  
  ### other_options ----
  output$other_options <- renderUI({
    req(results())
    if (input$year[1] != input$year[2]) {
      tagList(
        strong("Other Options"),
        checkboxInput("target", "Target population", value = selected_target()),
        checkboxInput("trend", "Trendline", value = selected_trend())
      )
    } else {
      tagList(
        strong("Other Options"),
        checkboxInput("target", "Target population", value = selected_target())
      )
    }
  })
  
  ### replot_button ----

  output$replot_button <- renderUI({
    req(results())
    actionButton("replot_button", "Plot", icon = icon("stats", lib = "glyphicon"))
  })
  
  
  ### Export table ----
  output$export_table <- downloadHandler(
    filename = function() {
      paste(input$year[1], "to", input$year[2],
            if (input$EPU == "All") {
              "AllEPUs"
            } else {
              input$EPU
            },
            ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(results_table() %>% select(-exclamation), file, row.names = FALSE)
    }
  )
  output$Export_table <- renderUI ({
    req(results())
    downloadButton("export_table",
                   "Export")
  })
  
  ### Export SBOT table ----
  output$export_SBOT_table <- downloadHandler(
    filename = function() {
      paste("SBOT_population_table_",
            input$year[1], "_to_", input$year[2],
            ".csv", 
            sep = "")
    },
    content = function(file) {
      write.csv(SBOT_table(), file, row.names = T)
    }
  )
  output$Export_SBOT_table <- renderUI ({
    req(results())
    downloadButton("export_SBOT_table",
                   "Export SBOT Table")
  })
  
  
  ### Export plot ----
  export_plot_height <- reactive({
    if (input$year[1] != input$year[2] & input$EPU == "All") {
      2.75 * (ceiling(length(EPU_choices()) - 1) / 3)
    } else {
      9
    }
    
  })
  
  output$export_plot <- downloadHandler(
    filename = function() {
      paste(
        input$year[1],
        "to",
        input$year[2],
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
        if (input$target == T) {
          "withTarget"
        },
        if (input$year[1] != input$year[2] & !is.null(input$trend)) {
          if (input$trend == T) {
            "withTrend"
          }
        },
        ".png",
        sep = "_"
      )
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
  
  
  output$table_tip <- renderUI({
    if(!is.null(results_path())) {
      text <- "Tip: A model estimate followed by '!!' indicates that the estimate is lower than the minimum count. Consider adjusting this number in your final reporting."
    } else {
      text <- ""
    }
    HTML(paste0("<div style='color:grey; font-size:14px;'>", text, "</div>"))
  })
  
  output$sidebarText <- renderUI({
    req(input$tables_plots)
    if (input$tables_plots == "Table") {
      tagList(
        br(),
        fileInput("results_file", "Upload Results CSV", accept = ".csv"),
        uiOutput("Year_select"),
        uiOutput("EPU_select"),
        br(),
        uiOutput("Export_table"),
        br(),
        uiOutput("Export_SBOT_table"),
        br(),
        htmlOutput("table_tip")
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
        uiOutput("replot_button"),
        br(),
        uiOutput("Export_plot")
      )
    }
  })
  
  
  ## Main panel UI objects -----
  # Text for when no results file is uploaded
  output$no_file <- renderText("No file selected")
  # Text for when no plot has been plotted yet
  output$no_plot <- renderText("Select from the options on the left, then press 'Plot'")
  
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
      tagList(br(),
              textOutput("no_file"))
    }
  })
  
  ### Plot tab ----
  
  # Render UI when plot button hasn't been pressed
  output$plotUI <- renderUI({
    if (!is.null(results_path())) {
      tagList(br(),
              textOutput("no_plot"))
    } else {
      tagList(br(),
              textOutput("no_file"))
    }
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
