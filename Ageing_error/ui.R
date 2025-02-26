#
#

library(shiny)
library(AgeingError)
library(shinyFiles)
library(DT)
library(bslib)

# Define UI for application that draws a histogram
ui <- function(request) {
  shinyUI(fluidPage(

    # Application title
    titlePanel("Create ageing error matrix for use in Stock Synthesis"),
    h4(p(strong("This tool uses the ", tags$a(href = "https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html","ageing error code", target = "_blank"), "developed by Andre Punt"))),
    h4(p(strong(("The required input are double reads of an ageing structure.")))),
    h4(p(strong(("This program will convert that simple format into the format needed by the ageing error function and write the data file.")))),
    h4(p(strong(("Then new data file will then run in the ageing error function and produce output, model selection results, and plots and output objects.")))),
    h4(p(strong(("The bias and precision vectors can be retrieved from age_err_output.csv or age_err_output.rds.")))),
          # 
    sidebarLayout(
        sidebarPanel(
          h4(strong("Need an example data file? Click the below button")),
          downloadButton("downloadData", "Download Example data CSV"),
          br(),
          br(),
          
          h4(strong("Choose data file")),
          fluidRow(column(width = 12, fileInput("file", "Multiple reads file",
                                                accept = c(
                                                  "text/csv",
                                                  "text/comma-separated-values",
                                                  "text/tab-separated-values",
                                                  "text/plain",
                                                  ".csv"
                                                )
          ))),
          
          h5(strong("Choose folder to retain model results")),
          h5(em("")),
          shinyDirButton(
            id = "AgeErr_dir",
            label = "Select directory",
            title = "Choose folder to put results and plots"
          ),
          br(),
          br(),
          
          fluidRow(
            #column(width = 6, numericInput("min_age", "Minimum age", value = NA, min = 0, max = 10000, step = 0.001)),
            column(width = 6, numericInput("max_age", "Maximum age of vector calculations", value = 100, min = 0, max = 10000, step = 0.01))
          ),
          
          
          br(),
          br(),
          
          
          actionButton("run_ageerr", strong("Run Ageing Error"),
                       width = "50%",
                       icon("circle-play"),
                       style = "font-size:120%;border:2px solid;color:#FFFFFF;background:#005595"),
          
          ), 

        # Show a plot of the generated distribution
        mainPanel(
          layout_column_wrap(
            card(
            card_header("1:1 plot"),
            plotlyOutput("oneoneplot")
          ),
          card(
            card_header("Model selection table"),
            DTOutput("aic_table")
          ),
          col_widths = c(3/4,3/4)
          ),
          
          br(),
          br(),
          br(),
          br(),
          
          layout_column_wrap(
            card(
              card_header("Bias plot"),
              plotlyOutput("biasplot")
            ),
            card(
              card_header("CV plot"),
              plotlyOutput("CVplot")
            ),
            card(
              card_header("SD plot"),
              plotlyOutput("SDplot")
            ),
            col_widths = c(3/4,3/4,3/4)
          ),
        )
    )
))}
