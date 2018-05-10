setwd("C:/Users/1701642.RGU/Dropbox/Pat-Research/UPDATE/16Jan2018/Rcode")
#setwd("C:/Users/User/Dropbox/Pat-Research/UPDATE/16Jan2018/Rcode")
source("setup.R")    #setwd, load all files into dfAll
numDat <- 18
dfAll <- setup('Data/C', numDat)
fileName <- 'C'

source("loadfile.R")      
source("datPartition.R")
source("trainmodel2.R")
source("majorCluster2.R")
source("plotRoc2.R")
source("calAuc.R")
source("perfResults2.R")
#source('export.R') 
source('barPlot2.R')
source('writeAppend.R')
source('multiPlot.R')

algor <- c('rf','svmRadial') #'mlp', , 'svmLinear'
#algor <- c('J48', 'rf', 'mlp', 'svmRadial', 'svmLinear')
numAlg <- 1
for (i in 18:numDat){
  if(i ==33) next
  for (j in 1:numAlg){
    #load individual file
    dat <- loadIndFile(dfAll, i)
    dPart <- datPartition(dat, 0.8)
    train <- dPart[[1]]
    test <- dPart[[2]]

    trainminor <- subset(train, label == 'positive')
    set.seed(1987)
    cm <-cmeans(train[,-ncol(train)],2,100,verbose= FALSE,method="cmeans")
    traincm <- cbind(train, pred = cm$cluster, percent = cm$membership)
    trainmajor <- subset(traincm, label == 'negative')
    
    cond1 <- abs(trainmajor$pred - as.numeric(trainmajor$label)) # 1=wrong, 0=correct
    cond2 <- trainmajor$percent.1 > 0.45 #prob belong to class1(negative)
    cond3 <- trainmajor$percent.2 > 0.45
    
    select1 <- subset(trainmajor,((cond1 == 0) | (cond1 == 1 & cond2)))
    
    #for wrong cluster number
    select2 <- subset(trainmajor,((cond1 == 1) | (cond1 == 0 & cond3)))
     
    nMajLeft1 <- nrow(select1)
    nMajLeft2 <- nrow(select2)
    
    majorselect1 <- select1[,1:ncol(train)]
    majorselect2 <- select2[,1:ncol(train)]
    
    train1 <- rbind(majorselect1, trainminor)
    train2 <- rbind(majorselect2, trainminor)
    #train w/o undersampling
    fit1 <- trainmodel(train1, algor[j])
    fit2 <- trainmodel(train2, algor[j])
    
    #fit <- trainmodel(train, algor[j])
    
    result1 <- perfResults2(fit1, test,i)
    result2 <- perfResults2(fit2, test,i)
    
    perf1 <- rbind(Data = i, Alg = algor[j],nMajLeft1,result1)
    perf2 <- rbind(Data = i, Alg = algor[j],nMajLeft2,result2)
    perf <- cbind(perf1,perf2)
    
    filenum <- 99  
    writeAppend(perf, filenum)
  }
}
