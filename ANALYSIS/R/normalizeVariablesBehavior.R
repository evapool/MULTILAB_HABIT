normalizeVariablesBehavior <- function (database)
{ # this function just normalize the different variables in so that we can apply fo 
# different datasets

  # measure of interest
  database$normPressFreq   <- scale(database$pressFreq)
  database$normLiking      <- scale(database$outcomeliking)
  
  
  return(database)
}

