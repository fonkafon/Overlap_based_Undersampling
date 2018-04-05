#read files

###############################################################################################
#library(base)
#library(dplyr)
#for (i in 1:12){
  #dat <- dfAll[which(dfAll$set == i),, drop = FALSE]    #no drop = maintain dataframe class
#  dat <- dfAll[which(dfAll$set == 12),, drop = FALSE]
#  dat$set <- NULL
#  dat <- Filter(function(x)!all(is.na(x)),dat)    #removes all cells(columns) w NA
#  dat[,-k] = scale(dat[,-k])
#  names(dat)[k]<-"label"
#  dat$label <- as.factor(dat$label)
#}
###############################################################################################
loadIndFile <- function(dfAll,i){  
  dat <- dfAll[which(dfAll$set == i),, drop = FALSE]
  dat$set <- NULL
  dat <- Filter(function(x)!all(is.na(x)),dat)    #removes all cells(columns) w NA
  k <- length(dat)
  dat$label <- as.character(dat[,k])    #duplicate the label col
  dat[,k] <- NULL                       #remove the original label col
  if(dat$label[1] == 1||2){
    dat$label[which(dat$label == 1)] <- 'negative'
    dat$label[which(dat$label == 2)] <- 'positive'
  } 
  dat$label <- as.factor(dat$label)
  dat[,-k] = scale(as.numeric(unlist(dat[,-k])))
  return(dat)
}