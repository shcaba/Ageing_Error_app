#
#

library(shiny)
library(data.table)
library(AgeingError)
library(ggplot2)
library(shinyFiles)
library(shinybusy)
library(plotly)
library(shinyWidgets)


################################################################################
####################### FUNCTIONS ##############################################
################################################################################

AEdata_convert<-function(DataFile.path.in,DataFile.path.out)
{
  Data.in<-read.csv(DataFile.path.in)
  Nreaders <- dim(Data.in)[2]
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
  write.csv(Reads2,DataFile.path.out)
  Reads2  
}

#Write TMB data file
AEdata.tmbfile<-function(Reads2,max.age,data.tmb.path)
{
  data.file.in<-readLines(paste0(data.tmb.path,"/data_temp.dat"))
  end.data<-data.file.in[11]
  data.file.in[2]<-paste("1",max.age)
  data.file.in[5]<-as.character(dim(Reads2)[1])
  data.file.in[7]<-paste("1",max.age,max.age/4)
  
  data.file.in[10:(10+(dim(Reads2)[1]-1))]<-apply(Reads2,1,paste,collapse=" ")
  data.file.in[10+(dim(Reads2)[1])]<-end.data
  
  writeLines(data.file.in,con=paste0(data.tmb.path,"/data_used.dat"))
}

################################################################################
################################################################################

# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  output$downloadData <- downloadHandler(
    filename = function(){"Example_data.csv"},
    content = function(file) 
      {
      file.copy("Example_data.csv", file)
      }
  )
  
    #Data.in <- reactiveValues(data = NULL,clear = FALSE)
  reactive({
    req(input$file)
  })
  x<-1
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
    
    model.name.vec<-c("B00_S11","B00_S12","B00_S13","B00_S21","B00_S22","B00_S23","B00_S31","B00_S32","B00_S33",
                  "B01_S11","B01_S12","B01_S13","B01_S21","B01_S22","B01_S23","B01_S31","B01_S32","B01_S33",
                  "B02_S11","B02_S12","B02_S13","B02_S21","B02_S22","B02_S23","B02_S31","B02_S32","B02_S33")
    
#    output$Model_picks<-renderUI({
#      pickerInput(
#        inputId = "myPicker",
#        label = "Choose models to run",
#        choices = c("B00_S11","B00_S12","B00_S13","B00_S21","B00_S22","B00_S23","B00_S31","B00_S32","B00_S33",
#                    "B01_S11","B01_S12","B01_S13","B01_S21","B01_S22","B01_S23","B01_S31","B01_S32","B01_S33",
#                    "B02_S11","B02_S12","B02_S13","B02_S21","B02_S22","B02_S23","B02_S31","B02_S32","B02_S33"),
#        options = list(
#          `actions-box` = TRUE,
#          size = 12,
#          `selected-text-format` = "count > 3"
#        ),
#        multiple = TRUE
#      )
#    })

      model.name<-input$myPicker
      model.name.index<-which(model.name.vec%in%input$myPicker)

    #    Data.in<-read.csv(input$file$datapath)
    #Copy the agemat executable to chosen directory
    
    Reads2<-AEdata_convert(input$file$datapath,paste0(selected_dir(),"/FormattedReads.csv"))
    
    #Create 1:1 plot
    output$oneoneplot<-renderPlotly(
      {
        oneoneplot<-ggplot(Reads2,aes(Reads2[,2],Reads2[,3]))+
          geom_point(aes(size=count))+
          geom_abline(intercept=0,slope=1,col="red",lwd=1.1)+
          xlab(colnames(Reads2)[2])+
          ylab(colnames(Reads2)[3])
        ggsave(paste(selected_dir(),"/1_1_plot.png",sep=""),oneoneplot,width=10,height=10,units="in")
        ggplotly(oneoneplot)
      })
    
 
    ##################  
    ### ADMB Models ##
    ##################
    
    if(input$AgeErr_option=="ADMB")
    {  
      #if(file.exists(paste(selected_dir(),"/ADMB_files/",sep=""))){unlink(list.files(paste(selected_dir(),"/ADMB_files/",sep="")),recursive=TRUE,force=TRUE)}
      if(!file.exists(paste(selected_dir(),"/ADMB_files/",sep=""))){dir.create(paste(selected_dir(),"/ADMB_files/",sep=""))}
      file.copy(from = file.path(paste0(main.dir,"/ADMB_files/agemat.exe")), to =  file.path(paste0(selected_dir(),"/ADMB_files/agemat.exe")), overwrite = TRUE)
      #Set up matrix dimensions
      MinAge <- 1
      MaxAge <- input$max_age
      #MaxAge <- max(ceiling(max(Reads2[,2:(Nreaders+1)])/10)*10)
      KnotAges = list(NA, NA)  # Necessary for option 5 or 6
      #Set up the bias and precision options for each model
#      BiasOpt.mat = SigOpt.mat =matrix(0,27,Nreaders)
      BiasOpt.mat = SigOpt.mat =list()
      BiasOpt.mat[[1]] =  c(0,0)
      BiasOpt.mat[[2]] =  c(0,0)
      BiasOpt.mat[[3]] =  c(0,0)
      BiasOpt.mat[[4]] =  c(0,0)
      BiasOpt.mat[[5]] =  c(0,0)
      BiasOpt.mat[[6]] =  c(0,0)
      BiasOpt.mat[[7]] =  c(0,0)
      BiasOpt.mat[[8]] =  c(0,0)
      BiasOpt.mat[[9]] =  c(0,0)
      BiasOpt.mat[[10]] =  c(0,1)
      BiasOpt.mat[[11]] =  c(0,1)
      BiasOpt.mat[[12]] =  c(0,1)
      BiasOpt.mat[[13]] =  c(0,1)
      BiasOpt.mat[[14]] =  c(0,1)
      BiasOpt.mat[[15]] =  c(0,1)
      BiasOpt.mat[[16]] =  c(0,1)
      BiasOpt.mat[[17]] =  c(0,1)
      BiasOpt.mat[[18]] =  c(0,1)
      BiasOpt.mat[[19]] =  c(0,2)
      BiasOpt.mat[[20]] =  c(0,2)
      BiasOpt.mat[[21]] =  c(0,2)
      BiasOpt.mat[[22]] =  c(0,2)
      BiasOpt.mat[[23]] =  c(0,2)
      BiasOpt.mat[[24]] =  c(0,2)
      BiasOpt.mat[[25]] =  c(0,2)
      BiasOpt.mat[[26]] =  c(0,2)
      BiasOpt.mat[[27]] =  c(0,2)
      
      SigOpt.mat[[1]] =c(1,1)
      SigOpt.mat[[2]] =c(1,2)
      SigOpt.mat[[3]] =c(1,3)
      SigOpt.mat[[4]] =c(2,1)
      SigOpt.mat[[5]] =c(2,2)
      SigOpt.mat[[6]] =c(2,3)
      SigOpt.mat[[7]] =c(3,1)
      SigOpt.mat[[8]] =c(3,2)
      SigOpt.mat[[9]] =c(3,3)
      SigOpt.mat[[10]] =c(1,1)
      SigOpt.mat[[11]] =c(1,2)
      SigOpt.mat[[12]] =c(1,3)
      SigOpt.mat[[13]] =c(2,1)
      SigOpt.mat[[14]] =c(2,2)
      SigOpt.mat[[15]] =c(2,3)
      SigOpt.mat[[16]] =c(3,1)
      SigOpt.mat[[17]] =c(3,2)
      SigOpt.mat[[18]] =c(3,3)
      SigOpt.mat[[19]] =c(1,1)
      SigOpt.mat[[20]] =c(1,2)
      SigOpt.mat[[21]] =c(1,3)
      SigOpt.mat[[22]] =c(2,1)
      SigOpt.mat[[23]] =c(2,2)
      SigOpt.mat[[24]] =c(2,3)
      SigOpt.mat[[25]] =c(3,1)
      SigOpt.mat[[26]] =c(3,2)
      SigOpt.mat[[27]] =c(3,3)
      
      Model.select<-as.data.frame(matrix(NA,length(model.name),4))
      colnames(Model.select)<-c("Run","AIC","AICc","BIC")
      biasvar.output.list<-list()
      
      #Run ageing error for selected models
      for(i in 1:length(model.name))
      {
        show_modal_spinner(spin="fingerprint",color="#5D9741",text=paste0("Making ageing error calculations for ",i," of ",length(input$myPicker)," total model runs"))
                
        setwd(selected_dir())
        DateFile = paste(getwd(),"/ADMB_files/",model.name[i],"/",sep="")
        dir.create(DateFile)
        BiasOpt =BiasOpt.mat[[model.name.index[i]]]
        SigOpt = SigOpt.mat[[model.name.index[i]]]
        RunFn(Data=Reads2, SigOpt=SigOpt,KnotAges=KnotAges, BiasOpt=BiasOpt,
              NDataSets=1, MinAge=MinAge, MaxAge=MaxAge, RefAge=round(round(0.25*(MaxAge)),0),
              MinusAge=2, PlusAge=25,
              SaveFile=DateFile,
              AdmbFile= file.path(paste0(selected_dir(),"/ADMB_files")), EffSampleSize=0, Intern=FALSE,
              JustWrite=FALSE) #,ExtraArgs=" -ams 2341577272 -est")
        Df = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[6])
        Nll = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[11])
        n = sum(ifelse(Reads2[,-1]==-999,0,1))
        Aic = round(2*Nll + 2*Df,0)
        Aicc = round(Aic + 2*Df*(Df+1)/(n-Df-1),0)
        Bic = round(2*Nll + Df*log(n),0)
        run.name<-model.name[i]
        Model.select[i,]<-c(run.name,Aic,Aicc,Bic)
        #Capture bias and variance estimates
        bias.var.out<-readLines(paste0(DateFile,"/agemat.rep")) #read in rep file
        bias.var.out.df<-as.data.frame(bias.var.out[(grep("Reader Age CV SD Expected age",bias.var.out)+1):(grep("Estimated age-structure by data set",bias.var.out)-2)]) #find the bias/var output in rep and make a data file
        bias.var.out.df.list<-mapply(function(x) as.numeric(strsplit(bias.var.out.df[x,],split=" ")[[1]]),x=1:dim(bias.var.out.df)[1],SIMPLIFY=FALSE) #change each line to be a number from character
        bias.var.out.df.num<-data.frame(do.call("rbind", bias.var.out.df.list)) #convert from list into matrix
        colnames.temp<-strsplit(bias.var.out[grep("Reader Age CV SD Expected age",bias.var.out)],split=" ")[[1]] #add header names
        colnames(bias.var.out.df.num)<-colnames.temp[-length(colnames.temp)]
        colnames(bias.var.out.df.num)[length(colnames(bias.var.out.df.num))]<-"Exp_ages"
        bias.var.out.df.num$Model<-model.name[i] #add model name
        biasvar.output.list[[i]]<-bias.var.out.df.num #put into output list
      }
      
      biasvar.output.ggplot<-do.call("rbind", biasvar.output.list) #make results into an object for ggplot
      
      #setwd(paste0(SourceFile,"/",folder.names[xx]))
      save(biasvar.output.ggplot,file=paste0(selected_dir(),"/ADMB_files/age_err_output.rds"))
      write.csv(biasvar.output.ggplot,file=paste0(selected_dir(),"/ADMB_files/age_err_output.csv"))
      save(Model.select,file=paste0(selected_dir(),"/ADMB_files/model_selection.rds"))
      write.csv(Model.select,file=paste0(selected_dir(),"/ADMB_files/model_selection.csv"))
      
      #Create bias plot
      output$biasplot<-renderPlotly(
        {
          biasplot<-ggplot(biasvar.output.ggplot,aes(Age,Exp_ages,color=Model))+
            geom_line()+
            facet_wrap(vars(Reader))+
            ylab("Expected Age")+
            ylim(0,MaxAge+round(0.25*MaxAge,0))+
            theme_bw()+
            theme(legend.position="bottom")
          ggsave(paste(selected_dir(),"/ADMB_files/bias_plot.png",sep=""),biasplot,width=10,height=10,units="in")
          ggplotly(biasplot)
        }
      )
      
      #Create CV plot
      output$CVplot<-renderPlotly(
        {
          CVplot<-ggplot(biasvar.output.ggplot,aes(Age,CV,color=Model))+
            geom_line()+
            facet_wrap(vars(Reader))+
            ylab("Coefficient of Variation")+
            ylim(0,quantile(biasvar.output.ggplot$CV,0.95))+
            theme_bw()+
            theme(legend.position="bottom")
          ggsave(paste(selected_dir(),"/ADMB_files/CV_plot.png",sep=""),CVplot,width=10,height=10,units="in")
          ggplotly(CVplot)
        }
      )
      
      #Create SD plot
      output$SDplot<-renderPlotly(
        {
          SDplot<-ggplot(biasvar.output.ggplot,aes(Age,SD,color=Model))+
            geom_line()+
            facet_wrap(vars(Reader))+
            ylab("Standard Deviation")+
            ylim(0,quantile(biasvar.output.ggplot$SD,0.95))+
            theme_bw()+
            theme(legend.position="bottom")
          ggsave(paste(selected_dir(),"/ADMB_files/SD_plot.png",sep=""),SDplot,width=10,height=10,units="in")
          ggplotly(SDplot)
        }
      )
      
    }

  ##################  
  ### TMB Models ###
  ##################
    
    if(input$AgeErr_option=="TMB")
      {
      #browser()
      #Move TMB files over to selected folder
#      dir.create(paste(selected_dir(),"/TMB_files/",sep=""))
      file.copy(from = file.path(paste0(main.dir,"/TMB_files")), to =  file.path(paste0(selected_dir())), recursive=TRUE,overwrite = TRUE)
      
      Model.select<-data.frame(matrix(NA,length(model.name),4))
      colnames(Model.select)<-c("Model","AIC","AICc","BIC")
      
      #Create TMB data file
       setwd(paste0(selected_dir(),"/TMB_files"))
       AEdata.tmbfile(Reads2,input$max_age,getwd())
       run_dat <- AgeingError::CreateData(file.path(getwd(), "data_used.dat"),
                                          NDataSet = 1,
                                          verbose = TRUE, EchoFile = "testEcho.out")
      
        
        for(i in 1:length(model.name))
        {
          show_modal_spinner(spin="fingerprint",color="#5D9741",text=paste0("Making ageing error calculations for ",i," of ",length(input$myPicker)," total model runs"))
          run_spc <- AgeingError::CreateSpecs(file.path(getwd(), paste0("spc/data_",model.name[i],".spc")),
                                               DataSpecs = run_dat, verbose = TRUE)
          
          test_mod <- try(AgeingError::DoApplyAgeError(
            Species = "Species",
            DataSpecs = run_dat,
            ModelSpecsInp = run_spc,
            AprobWght = 1e-06,
            SlopeWght = 0.01,
            SaveDir = file.path(getwd(), paste0("TMB_Results_",model.name[i])),
            verbose = FALSE))
          
          if(class(test_mod)!="try-error")
          {
            results_out <- AgeingError::ProcessResults(Species = "Species", SaveDir = file.path(getwd(), paste0("TMB_Results_",model.name[i])), CalcEff = TRUE, verbose = FALSE)
            Model.select[i,2:4]<- round(as.numeric(results_out$ModelSelection),2) 
          }
          Model.select[i,1]<-model.name[i]
        }
        write.csv(Model.select,file.path(getwd(),"Model_select.csv"))
        save(Model.select,file=file.path(getwd(),"Model_select.rds"))
        
      }
   
    #Create model selection table
      output$aic_table <- renderDT({
      datatable(Model.select,
                     options = list(pageLength = 10,
                                    scrollX = TRUE),
                                    rownames = TRUE)
    })
    
    
    remove_modal_spinner()
  
  }
  ) #End ObserveEvent
 
  
}
