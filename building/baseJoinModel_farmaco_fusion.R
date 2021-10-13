library(dplyr)
library(cohorteicc2)

print('baseJoinModel_farmaco_fusion OK')


#' Merge farmacos_traye and baseJoinModel by id
#'
#' @param df1 (data.frame)
#' @param df2 (data.frame)
#'
#' @return df (data.frame) merged df
merge_farmacos <- function(df1, df2){
  df <- df1 %>% dplyr::left_join(df2, by='id')
  return (df)
}


#' process resultant data frame from merging baseJoinModel and farmacos_traye
#'
#' @param df (data.frame)
#' @param duration (numeric) global param FOLLOW_UP
#'
#' @return df processed
process_baseJoinModel1 <- function(df, duration){

  # filter patients with drug prescriptions
  cols <- c('familia', 'end', 'dura', 'tip', 'estado_obje')
  df <- df[!rowSums(is.na(df[cols])), ]
  df$PATIENT_WITH_PRESCRIPTION <- TRUE
  
  # filter, for each patient, prescriptions happened during follow up
  df <- df %>%
    group_by(id) %>% 
    filter((end > falta_ing1) & (start < (falta_ing1 + duration)))
  
  # set prescription limits from falta_ing1 to analyzed_period (to MortOingIcc)/oneyear
  df <- df %>%
    mutate(start = if_else(start < falta_ing1, falta_ing1, start)) %>%
    mutate(end = if_else(end > (falta_ing1 + duration), falta_ing1 + duration, end)) %>%
    mutate(end = if_else(end > MortOingIcc, MortOingIcc, end, missing = end))
  
  # set time to event and set event
  df <- df %>%
    mutate(event = if_else((MortOingIcc - falta_ing1) <= 360,
                           TRUE,
                           FALSE,
                           missing = FALSE)) %>%
    mutate(time_to_event = if_else(as.numeric(MortOingIcc - falta_ing1) <= 360, 
                                   as.numeric(MortOingIcc - falta_ing1), 
                                   360))
  # filter prescription periods where end >= start
  df <- df[(df["end"] >= df["start"]) | is.na(df["end"]), ]
  
  # adjust duration
  df["duration"] <- df["end"] - df["start"]
  
  return(df)
}
