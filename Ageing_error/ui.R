#
#

library(shiny)
library(AgeingError)

# Define UI for application that draws a histogram
ui <- function(request) {
  shinyUI(fluidPage(

    # Application title
    titlePanel("Ageing error"),
    h4(p(strong("This tool uses the ", tags$a(href = "https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html","ageing error code", target = "_blank"), "developed by Andre Punt"))),
    h5(p("Any suggested changes or requests? Please submit an issue with the recommendation", tags$a(href = "https://github.com/shcaba/SS-DL-tool/issues", "here", target = "_blank"))),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
        ), 

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("oneoneplot")
        )
    )
))}
