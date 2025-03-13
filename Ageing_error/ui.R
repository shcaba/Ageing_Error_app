#
#

library(shiny)
library(AgeingError)
library(shinyFiles)
library(DT)
library(bslib)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- function(request) {
  shinyUI(fluidPage(
    
    setBackgroundImage(
      src = "sablefish_otolith.jpg"
    ),

    # Application title
    titlePanel("Create an ageing error matrix for use in Stock Synthesis"),
    h4(p(strong(("There are two modelling options to run the ageing error")))),
    tags$ul(
      tags$li("The original ADMB version written by ", tags$a(href = "https://cdnsciencepub.com/doi/10.1139/F08-111","Andre Punt", target = "_blank"), "and updated by ", tags$a(href = "https://github.com/pfmc-assessments/AgeingError","Jim Thorson", target = "_blank")),
      tags$li("A TMB version adapted by Kelli Johnson and described in this ", tags$a(href = "https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html","vignette.", target = "_blank")),
    ),
    h4(p(strong(("The required input are double reads of an ageing structure. See downloadble example data file for more details.")))),
    #h4(p(strong(("This program will convert that simple format into the format needed by the ageing error function and write the data file.")))),
    #h4(p(strong(("Then new data file will then run in the ageing error version of choice and produce output, model selection results, and plots and output objects.")))),
    #h4(p(strong(("Model selection criteria can be used to choose which model for bias and precision is best supported by the data.")))),
    h4(p(strong("Up to 27 models of different bias (B) and sigma (S, which provides precision) combinations for Readers 1 and 2 are run, and model selection criteria provided. The description of options are found in the ", tags$a(href = "https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html", "vignette.", target = "_blank")))),
    h4(p(strong(("Model naming convention example using B01_S12: B01 means bias option 0 for Reader 1 and 1 for Reader 2; S12 sigma option 1 for Reader 1 and 2 for Reader 2.")))),
    h4(p(strong(("Model output includes")))),
    tags$ul(
      tags$li("FormattedReads.csv is the formatted data file used in the ageing error functions"),
      tags$li("Bias and precision vectors can be retrieved in the ADMB folder (age_err_output.csv and .rds), or in the individual folders of the TMB models"),
      tags$li("Model selection tables (Model_select.csv and .rds) "),
      tags$li("Agreement (i.e., 1:1), bias and precision plots"),
    ),
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
          
          h5(strong("Choose ageing error approach")),
          awesomeRadio(
            inputId = "AgeErr_option",
            label = "",
            choices = c("ADMB", "TMB"),
            selected = "TMB",
            inline = TRUE,
            status = "success"
          ),
          
          br(),
          br(),
          
          fluidRow(
            #column(width = 6, numericInput("min_age", "Minimum age", value = NA, min = 0, max = 10000, step = 0.001)),
            column(width = 4, numericInput("max_age", "Maximum age of vector calculations", value = 100, min = 0, max = 10000, step = 0.01))
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
