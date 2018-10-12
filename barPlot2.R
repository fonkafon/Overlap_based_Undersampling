#plot each dataset 
barPlot2 <- function(cfAll,cfAll2,i, numDat,k){
  dat1 <- t(rbind(cfAll, resamp = 'no'))
  dat2 <- t(rbind(cfAll2, resamp = 'yes'))
  alldat <- as.data.frame(rbind(dat1,dat2))
  alldat[,1:5] <- lapply(alldat[,1:5], function(x) as.numeric(as.character(x)))
  alldat[,1:5]<- round(alldat[,1:5], digits = 4)
  
  #drawPlot('barAUC', 'AUC', i, alldat)
  
  fName <- paste0('bar', k, '.pdf')
  if(!file.exists(fName)) {pdf(file = fName)}
  #fName <- paste0('barAUC', i,'.jpg')
  p1 <- ggplot(alldat, aes(factor(Alg), AUC, fill = resamp)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") + ggtitle(paste0('barAUC of Data', i)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  
 # fName <- paste0('barSens', i,'.jpg')
  #jpeg(file = fName)
  p2 <- ggplot(alldat, aes(factor(Alg), Sensitivity, fill = resamp)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")+ ggtitle(paste0('barSens of Data', i)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  
  #fName <- paste0('barSpec', i,'.jpg')
  #jpeg(file = fName)
  p3 <-ggplot(alldat, aes(factor(Alg), Specificity, fill = resamp)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")+ ggtitle(paste0('barSpec of Data', i)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(hjust = 0.5, face = 'bold'))
  multiplot(p1,p2,p3)
  if(i == numDat) {dev.off()}
}