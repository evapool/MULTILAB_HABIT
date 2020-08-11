getChangeIndex <- function (database)
  { # this function compute the pre-post index
  
  DATABASE.means <- aggregate(database$pressFreq, by = list(database$ID, database$prepost, database$cue, database$group, database$averagePress), FUN='mean') # extract means
  colnames(DATABASE.means) <- c('ID','prepost','cue','group', 'averagePress','pressFreq')
  DATABASE.index <- ddply(DATABASE.means, .(ID, cue), transform, pressFreq  = pressFreq-pressFreq[prepost=="pre"])
  DATABASE.index <- subset(DATABASE.index, prepost!='pre')
  
  return (DATABASE.index)
  
}  