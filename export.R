#export results
export <- function(algor,results1, results2, technique){
  fname <- paste0(technique,'_',algor,'.csv')
  sink(fname)
  cat(technique, 'with ', algor, '.', '\n')
  cat('\n')
  cat('No sampling', '\n')
  write.csv(t(results1))
  cat('________________________________________________________')
  cat('\n')
  cat('\n')
  cat('With clustering-based undersampling', '\n')
  write.csv(t(results2))
  sink()
}