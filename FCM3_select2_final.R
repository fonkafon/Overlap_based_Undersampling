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
source("selectNRun.R")
source('barPlot2.R')
source('writeAppend.R')
source('multiPlot.R')

algor <- c('rf','svmRadial') #'mlp', , 'svmLinear'
#algor <- c('J48', 'rf', 'mlp', 'svmRadial', 'svmLinear')
numAlg <- 1
threshold <- 45

for (i in 18:numDat){
  if(i ==33 | i == 22) next
  for (j in 1:numAlg){
    #load individual file
    dat <- loadIndFile(dfAll, i)
    dPart <- datPartition(dat, 0.8)
    train <- dPart[[1]]
    test <- dPart[[2]]
    
    trainminor <- train[train$label == 'positive',]
    
    set.seed(1987)
    cm <-cmeans(train[,-ncol(train)],2,100,verbose= TRUE,method="cmeans")
    traincm <- cbind(train, pred = cm$cluster, percent = cm$membership)
    trainmajor <- traincm[traincm$label == 'negative',]
    
    cond1 <- abs(trainmajor$pred - 1) # 1=wrong, 0=correct
    cond2 <- trainmajor$percent.1 > threshold/100 #prob belong to class1(negative)
    cond3 <- trainmajor$percent.2 > threshold/100 #prob belong to class1(negative)
    
    select1 <- trainmajor[(cond1 == 0) | (cond1 == 1 & cond2),]
    #for wrong cluster number
    select2 <- trainmajor[(cond1 == 1) | (cond1 == 0 & cond3),]
    
    result1 <- selectNRun(select1,trainmajor, trainminor, algor[j], test,i)
    result2 <- selectNRun(select2,trainmajor, trainminor, algor[j], test,i)
    
    #Compare sens+BA
    sens1 <- as.numeric(result1[4,])
    sens2 <- as.numeric(result2[4,])
    ba1 <- as.numeric(result1[7,])
    ba2 <- as.numeric(result2[7,])
    if(sens1 != sens2){ifelse(sens1 > sens2, best <- result1, best <- result2)}
    if(sens1 == sens2){ifelse(ba1 > ba2, best <- result1, best <- result2)}
    
    filenum <- threshold  
    writeAppend(best, filenum)
    #technique <- ('clusteringUS')
    #export(algor[j],cfAll,cfAll2, technique)
  }
}
