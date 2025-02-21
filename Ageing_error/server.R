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
library(shinyFiles)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #Data.in <- reactiveValues(data = NULL,clear = FALSE)
  reactive({
    req(input$file)
  })
  main.dir<-getwd()
  volumes <- c(Home = fs::path_home())
  shinyDirChoose(input, "AgeErr_dir", roots = volumes, session = session)

  # Store the selected directory path
  selected_dir <- reactiveVal(fs::path_home())  # Default to home directory
  
  # Update selected directory when user chooses one
  observeEvent(input$AgeErr_dir, {
    if (!is.null(input$AgeErr_dir)) {
      selected_dir(parseDirPath(volumes, input$AgeErr_dir))
    }
  })
    
  observeEvent(input$run_ageerr,{
    Data.in<-read.csv(input$file$datapath)
    browser()
    file.copy(from = file.path(paste0(main.dir,"/agemat.exe")), to =  file.path(paste0(selected_dir(),"/agemat.exe")), overwrite = TRUE)
    
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
    
    #Set up matrix dimensions
    MinAge <- 1
    MaxAge <- max(ceiling(max(Reads2[,2:(Nreaders+1)])/10)*10)
    KnotAges = list(NA, NA)  # Necessary for option 5 or 6
    #Set up the bias and precision options for each model
    BiasOpt.mat = SigOpt.mat =matrix(0,9,Nreaders)
    BiasOpt.mat[1,] =  c(0,0)
    BiasOpt.mat[2,] =  c(0,0)
    BiasOpt.mat[3,] =  c(0,0)
    BiasOpt.mat[4,] =  c(0,1)
    BiasOpt.mat[5,] =  c(0,1)
    BiasOpt.mat[6,] =  c(0,1)
    BiasOpt.mat[7,] =  c(0,2)
    BiasOpt.mat[8,] =  c(0,2)
    BiasOpt.mat[9,] =  c(0,2)
    
    SigOpt.mat[1,] =c(1,1)
    SigOpt.mat[2,] =c(2,1)
    SigOpt.mat[3,] =c(3,1)
    SigOpt.mat[4,] =c(1,2)
    SigOpt.mat[5,] =c(2,2)
    SigOpt.mat[6,] =c(3,2)
    SigOpt.mat[7,] =c(1,3)
    SigOpt.mat[8,] =c(2,3)
    SigOpt.mat[9,] =c(3,3)
    
    model.aic<-as.data.frame(matrix(NA,9,4))
    colnames(model.aic)<-c("Run","AIC","AICc","BIC")
    model.name<-c("B0_S1","B0_S2","B0_S3","B1_S1","B1_S2","B1_S3","B2_S1","B2_S2","B2_S3","B3_S1","B3_S2","B3_S3")
    
    #Run ageing error for each of the 9 models
    for(i in 1:9)
    {
      setwd(selected_dir())
      DateFile = paste(getwd(),"/",model.name[i],"/",sep="")
      dir.create(DateFile)
      BiasOpt =BiasOpt.mat[i,]
      SigOpt = SigOpt.mat[i,]
      RunFn(Data=Reads2, SigOpt=SigOpt,KnotAges=KnotAges, BiasOpt=BiasOpt,
            NDataSets=1, MinAge=MinAge, MaxAge=MaxAge, RefAge=10,
            MinusAge=2, PlusAge=25,
            SaveFile=DateFile,
            AdmbFile= main.dir, EffSampleSize=0, Intern=FALSE,
            JustWrite=FALSE) #,ExtraArgs=" -ams 2341577272 -est")
      Df = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[6])
      Nll = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[11])
      n = sum(ifelse(Reads2[,-1]==-999,0,1))
      Aic = 2*Nll + 2*Df
      Aicc = Aic + 2*Df*(Df+1)/(n-Df-1)
      Bic = 2*Nll + Df*log(n)
      browser()
      run.name<-model.name[i]
      model.aic[i,]<-c(run.name,Aic,Aicc,Bic)
    }
    
    #setwd(paste0(SourceFile,"/",folder.names[xx]))
    save(model.aic,file=paste(selected_dir(),"/","model_selection.dmp",sep=""))
  
    
    
    output$oneoneplot<-renderPlot(
    {
      ggplot(Reads2,aes(Reader1,Reader3))+
        geom_point(aes(size=count))+
        geom_abline(intercept=0,slope=1,col="red",lwd=1.1)
    })
    
    
    output$aic_table <- renderDT({
      datatable(model.aic,
                options = list(pageLength = 10,
                               scrollX = TRUE),
                rownames = TRUE)
    })
    
  }
  ) #End ObserveEvent

  
}
