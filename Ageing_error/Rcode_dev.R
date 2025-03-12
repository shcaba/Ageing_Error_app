data_dir<-"C:/Users/Jason.Cope/Downloads"
Sable_dat <- AgeingError::CreateData(file.path(data_dir, "Sable.dat"),
                                     NDataSet = 1,
                                     verbose = TRUE, EchoFile = "SableEcho.out"
)

Sable_spc <- AgeingError::CreateSpecs(file.path(data_dir, "Sable.spc"),
                                      DataSpecs = Sable_dat, verbose = TRUE
)

Sable_mod <- AgeingError::DoApplyAgeError(
  Species = "Sable",
  DataSpecs = Sable_dat,
  ModelSpecsInp = Sable_spc,
  AprobWght = 1e-06,
  SlopeWght = 0.01,
  SaveDir = file.path(data_dir, "Results"),
  verbose = FALSE
)

Sable_out <- AgeingError::ProcessResults(Species = "Sable", SaveDir = file.path(data_dir, "Results"), CalcEff = TRUE, verbose = FALSE)













DataFile.path<-"C:/Users/Jason.Cope/Documents/Github/Ageing_Error_app/Ageing_error/Example_data.csv"
DataFile.path.out<-"C:/Users/Jason.Cope/Desktop/test/Reads2.csv"
#Create data pairings for data file
AEdata_convert<-function(DataFile.path.in,DataFile.path.out)
{
  Data.in<-read.csv(DataFile.path)
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



#ADMB version
MinAge <- 2
MaxAge <- max(ceiling(max(Reads2[,2:(Nreaders+1)])/10)*10)
KnotAges = list(NA, NA)  # Necessary for option 5 or 6

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
model.name<-c("B00_S11","B00_S12","B00_S13","B00_S21","B00_S22","B00_S23","B00_S31","B00_S32","B00_S33",
              "B01_S11","B01_S12","B01_S13","B01_S21","B01_S22","B01_S23","B01_S31","B01_S32","B01_S33",
              "B02_S11","B02_S12","B02_S13","B02_S21","B02_S22","B02_S23","B02_S31","B02_S32","B02_S33")












################
#Write TMB data file
AEdata.file<-function(Reads2,maxage,data.tmb.path)
{
  data.file.in<-readLines("C:/Users/Jason.Cope/Documents/Github/Ageing_Error_app/Ageing_error/data_temp.dat")
  end.data<-data.in[11]
  data.file.in[2]<-paste("1",maxage)
  data.file.in[5]<-as.character(dim(Reads2)[1])
  data.file.in[7]<-paste("1",maxage,maxage/4)
  
  data.file.in[10:(10+(dim(Reads2)[1]-1))]<-apply(Reads2,1,paste,collapse=" ")
  #data.file.in[10:97]<-apply(Reads2,1,paste,collapse=" ")[1:88]
  data.file.in[10+(dim(Reads2)[1])]<-end.data
  
  writeLines(data.file.in,con="C:/Users/Jason.Cope/Documents/Github/Ageing_Error_app/Ageing_error/update.dat")
  
}

###############


data.dir.in<-"C:/Users/Jason.Cope/Desktop/test2"

test_dat <- AgeingError::CreateData(file.path(data.dir.in, "update.dat"),
                                     NDataSet = 1,
                                     verbose = TRUE, EchoFile = "testEcho.out")

test_dat <- AgeingError::CreateData(file.path(data.dir.in, "data_test.dat"),
                                    NDataSet = 1,
                                    verbose = TRUE, EchoFile = "testEcho.out")

#test_dat <- AgeingError::CreateData(file.path(data.dir.in, "test.dat"),
#                                    NDataSet = 1,
#                                    verbose = TRUE, EchoFile = "testEcho.out"
#)



model.name<-c("B00_S11","B00_S12","B00_S13","B00_S21","B00_S22","B00_S23","B00_S31","B00_S32","B00_S33",
              "B01_S11","B01_S12","B01_S13","B01_S21","B01_S22","B01_S23","B01_S31","B01_S32","B01_S33",
              "B02_S11","B02_S12","B02_S13","B02_S21","B02_S22","B02_S23","B02_S31","B02_S32","B02_S33")

Model.select<-data.frame(matrix(NA,27,4))
colnames(Model.select)<-c("Model","AIC","AICc","BIC")

for(i in 1:length(model.name))
{
  test_spc <- AgeingError::CreateSpecs(file.path(data.dir.in, paste0("data_",model.name[i],".spc")),
                                       DataSpecs = test_dat, verbose = TRUE)

    test_mod <- try(AgeingError::DoApplyAgeError(
    Species = "test",
    DataSpecs = test_dat,
    ModelSpecsInp = test_spc,
    AprobWght = 1e-06,
    SlopeWght = 0.01,
    SaveDir = file.path(data.dir.in, paste0("TMB_Results_",model.name[i])),
    verbose = FALSE))
  
    if(class(test_mod)!="try-error")
    {
      test_out <- AgeingError::ProcessResults(Species = "test", SaveDir = file.path(data.dir.in, paste0("TMB_Results_",model.name[i])), CalcEff = TRUE, verbose = FALSE)
      Model.select[i,2:4]<- as.numeric(test_out$ModelSelection) 
    }
    Model.select[i,1]<-model.name[i]
}
write.csv(Model.select,file.path(data.dir.in,"Model_select.csv"))
save(Model.select,file.path(data.dir.in,"Model_select.rds"))






