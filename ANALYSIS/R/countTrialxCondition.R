countTrialxCondition <- function (database)
{# this function counts the number of trial per condition of interest
  
  # init counters
  count_val   = 0;
  count_deval = 0;
  
  for (i in 1:length(database$prepost)) {
    
    if (database$cue[i] == 'Valued') {
      count_val = count_val + 1
      database$itemxcondition[i] = count_val
      
    } else if (database$cue[i] == 'Devalued') {
      count_deval = count_deval + 1
      database$itemxcondition[i] = count_deval
      
    }
    
  }
  return (database)  
}