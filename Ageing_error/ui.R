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
    
    tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff")),
                    
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
          
         fluidRow(column(width=6,h4(strong("Choose data file "))),
                 column(width=6,h4(strong("Choose folder to retain model results")))),
         
          fluidRow(column(width=6, fileInput("file", NULL,
                                                accept = c(
                                                  "text/csv",
                                                  "text/comma-separated-values",
                                                  "text/tab-separated-values",
                                                  "text/plain",
                                                  ".csv"
                                                ))),
                  column(width=6),          
                  shinyDirButton(
                    id = "AgeErr_dir",
                    label = "Select results directory",
                    title = "Results folder")
                   ),
          
          #h5(strong("Choose folder to retain model results")),
          #h5(em("")),
 #         shinyDirButton(
#            id = "AgeErr_dir",
#            label = "Select model results directory",
#            title = "Choose folder to retain results and plots"
#          ),
          
#          br(),
#          br(),
          
          #h5(strong("Choose ageing error approach")),
          fluidRow(column(width=6,
            awesomeRadio(
            inputId = "AgeErr_option",
            label = "Choose ageing error approach",
            choices = c("ADMB", "TMB"),
            selected = "TMB",
            inline = TRUE,
            status = "success"
          )),
          column(width=6,
          pickerInput(
            inputId = "myPicker",
            label = "Choose models to run",
            choices = c("B00_S11","B00_S12","B00_S13","B00_S21","B00_S22","B00_S23","B00_S31","B00_S32","B00_S33",
                        "B01_S11","B01_S12","B01_S13","B01_S21","B01_S22","B01_S23","B01_S31","B01_S32","B01_S33",
                        "B02_S11","B02_S12","B02_S13","B02_S21","B02_S22","B02_S23","B02_S31","B02_S32","B02_S33"),
            options = list(
              `actions-box` = TRUE,
              size = 12,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          ))),
        
          fluidRow(
            #column(width = 6, numericInput("min_age", "Minimum age", value = NA, min = 0, max = 10000, step = 0.001)),
            column(width = 5, numericInput("max_age", "Maximum age of vector calculations", value = 100, min = 0, max = 1000, step = 1))
          ),
          
         # br(),
        #  br(),
          
          actionButton("run_ageerr", strong("Run Ageing Error"),
                       width = "100%",
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
            DTOutput("aic_table"),
            tags$head(tags$style("#aic_table table {background-color: white; }", media="screen", type="text/css"))
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
