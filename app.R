#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
list.of.packages <- c("bayesplot", "tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Define UI ----
ui <- fluidPage(
    titlePanel("Elk Population Estimator"),
    
    sidebarLayout(
        sidebarPanel(tabsetPanel(type = "tabs",
                                 tabPanel("Load",
                                          br(),
                                          fileInput("DataInput", "Survey Data Excel File"),
                                          submitButton("RUN MODEL")),
                                 tabPanel("Results",
                                          br(),
                                          selectInput("Year", label = "Year",
                                                      choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                                     "Choice 3" = 3), selected = 1),
                                          selectInput("EPU", label = "EPU",
                                                      choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                                     "Choice 3" = 3), selected = 1),
                                          selectInput("Method", label = "Method",
                                                      choices = list("All" = 1, "Model" = 2,
                                                                     "Standard" = 3), selected = 1),
                                          checkboxGroupInput("CI", 
                                                             h5(strong("Confidence Intervals")), 
                                                             choices = list("95%" = 1, 
                                                                            "50%" = 2),
                                                             selected = c(1,2)),
                                          br(),
                                          h5(strong("Other Options")),
                                          checkboxInput("Trend", "Trendline", value = TRUE),
                                          checkboxInput("Target", "Target population line", value = TRUE)),
                                          )),
        mainPanel(h3("Results", align = "center"),
                  textOutput("Loading"),
                  # p("p creates a paragraph of text."),
                  # p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
                  # strong("strong() makes bold text."),
                  # em("em() creates italicized (i.e, emphasized) text."),
                  # br(),
                  # code("code displays your text similar to computer code"),
                  # div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
                  # br(),
                  # p("span does the same thing as div, but it works with",
                  #   span("groups of words", style = "color:blue"),
                  #   "that appear inside a paragraph."),
        img(src = "SP36_scowling_bull.jpg", height = 500, width = 700)
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    output$Loading <- renderText({ 
            "Loading...this can take an hour or more."
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
