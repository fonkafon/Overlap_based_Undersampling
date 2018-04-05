#setup
library(ggplot2)
library(lattice)
library(caret)              #Classification And REgression Training
library(plyr)
library(dplyr)
library(e1071)
library(ROCR)
library(base)
library(randomForest)
seed <- 1987
set.seed(seed)
#setwd("C:/Users/1701642/Dropbox/Pat-Research/UPDATE/14Nov2017")
#setwd("C:/Users/User/Dropbox/Pat-Research/UPDATE/14Nov2017")

setup <- function(fldfile, setNo){         #folder/file w/o number 
  for (i in 1:setNo){
    if(i==1){dfAll <- data.frame()}
    fname <- paste0(fldfile, i,'.csv')
    df <- read.csv(fname,sep=',',header = FALSE)
    cat (fname,'<' ,'Nrow: ', nrow(df), '| Ncol: ',ncol(df),'>\n')
    df <- cbind(df, set = i)
    dfAll <- rbind.fill(dfAll,df)         #fill missing col w NA
  }
  write.csv(dfAll,file = 'allSets.csv')
  dfAll <- read.csv('allSets.csv')
  cat('Nrow: ', nrow(dfAll),' | Ncol: ',ncol(dfAll), '\n')
  table(dfAll$set)
  dfAll$X <- NULL
  return(dfAll)
}
