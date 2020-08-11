getClassicIndex <- function (database)
{ # this function compute the pre-post index
  
  DATABASE.means <- aggregate(database$pressFreq, by = list(database$ID, database$prepost, database$cue, database$group, database$averagePress), FUN='mean') # extract means
  colnames(DATABASE.means) <- c('ID','prepost','cue','group', 'averagePress','pressFreq')
  DATABASE.post <- subset(DATABASE.means, prepost == 'post')
  DATABASE.index <- ddply(DATABASE.post, .(ID), transform, pressFreq  = pressFreq-pressFreq[cue=="Devalued"])
  DATABASE.index <- subset(DATABASE.index, cue!='Devalued')
  
  return (DATABASE.index)
  
}