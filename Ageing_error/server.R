#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)
library(AgeingError)
library(ggplot2)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #Data.in <- reactiveValues(data = NULL,clear = FALSE)
  reactive({
    req(input$file)
  })
  
  observeEvent(input$file,{
    Data.in<-read.csv(input$file$datapath)
    Nreaders <- dim(Data.in)[2]

    #Create data file needed by ageing error function
    #This is count of the read combinations by readers
    Reads2 = data.frame(count=1, Data.in[1,])
    
    for(RowI in 2:nrow(Data.in)){
      DupRow <- NA
      # loop over all previous rows looking for duplicates of the current row
      for(PreviousRowJ in 1:nrow(Reads2)){
        # if all values match, take note of previous row number
        if(all(Data.in[RowI,1:Nreaders]==
               Reads2[PreviousRowJ,1:Nreaders+1])){
          DupRow = PreviousRowJ
        }
      }
      # if no duplicate found, add new row
      if(is.na(DupRow)){
        # Add new row to ChinaReads2
        Reads2 <- rbind(Reads2, data.frame(count=1, Data.in[RowI,]))
      }
      # if duplicate found, increment count
      if(!is.na(DupRow)){
        # Increment number of samples for the previous duplicate
        Reads2[DupRow,1] <- Reads2[DupRow,1] + 1
      }
    }
    
    output$oneoneplot<-renderPlot(
    {
      ggplot(Reads2,aes(Reader1,Reader3))+
        geom_point(aes(size=count))+
        geom_abline(intercept=0,slope=1,col="red",lwd=1.1)
    })
    
  }
  ) #End ObserveEvent

  
}
