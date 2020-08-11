normalizeVariablesQuestionnaires <- function (database)
{ # this function just normalize the different variables in so that we can apply fo 
# different datasets

  # questionnaires of interest
  database$BIS_total       <- scale(database$BIS_total)
  database$TICS_CSSS       <- scale(database$TICS_CSSS)
  if (length(database$OCIR_total) > 0){ # if OCIR_total exist
  database$OCIR_total      <- scale(database$OCIR_total)}
  if (length(database$BDI_total) > 0){ # if BDI_total exist
    database$BDI_total      <- scale(database$BDI_total)}
  if (length(database$STAIS_total) > 0){ # if STAIS exist
  database$ANXIETY         <- scale(database$STAIS_total)}
  if (length(database$STAIT_total) > 0){ # if STAIT exist
    database$ANXIETY         <- scale(database$STAIT_total)}
  

  
  # subscales we wont use (BIS)
  database$BIS_attentional <- scale(database$BIS_attentional)
  database$BIS_motor       <- scale(database$BIS_motor)
  database$BIS_nonplanning <- scale(database$BIS_nonplanning)
  
  # subscales we wont use (TICS)
  database$TICS_WOOV      <- scale(database$TICS_WOOV)
  database$TICS_SOOV      <- scale(database$TICS_SOOV)
  database$TICS_PREPE     <- scale(database$TICS_PREPE)
  database$TICS_WODI      <- scale(database$TICS_WODI)
  database$TICS_EXWO      <- scale(database$TICS_EXWO)
  database$TICS_LACK      <- scale(database$TICS_LACK)
  database$TICS_SOTE      <- scale(database$TICS_SOTE)
  database$TICS_SOIS      <- scale(database$TICS_SOIS)
  database$TICS_WORY      <- scale(database$TICS_WORY)
  
  # subscales we wont use (STAI)
  if (length(database$STAIS_total) > 0){ # if STAIS exist
    database$STAIS_total         <- scale(database$STAIS_total)}
  if (length(database$STAIT_total) > 0){ # if STAIT exist
    database$STAIT_total         <- scale(database$STAIT_total)}
  
  return(database)
}

