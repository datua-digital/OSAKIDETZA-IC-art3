library(dplyr)
library(cohorteicc2)


#' Merge farmacos_traye and baseJoinModel by id
#'
#' @param df1 (data.frame)
#' @param df2 (data.frame)
#'
#' @return df (data.frame) merged df
merge_farmacos <- function(df1, df2) {
  df <- df1 %>% dplyr::left_join(df2, by = "id")
  return(df)
}


#' process resultant data frame from merging baseJoinModel and farmacos_traye
#'
#' @param df (data.frame)
#' @param duration (numeric) global param FOLLOW_UP
#'
#' @return df processed
process_base_join_model <- function(df, duration) {
  # filter patients with drug prescriptions
  cols <- c("familia", "end", "dura", "tip", "estado_obje")
  df <- df[!rowSums(is.na(df[cols])), ]
  df$patient_with_prescription <- TRUE
  # filter, for each patient, prescriptions happened during follow up
  df <- df %>%
    group_by(id) %>% 
    filter((end > falta_ing1) & (start < (falta_ing1 + duration)))
  # set prescription limits from falta_ing1 to analyzed_period (to MortOingIcc)/oneyear
  df <- df %>%
    dplyr::mutate(start = if_else(start < falta_ing1, falta_ing1, start)) %>%
    dplyr::mutate(end = if_else(end > (falta_ing1 + duration), falta_ing1 + duration, end)) %>%
    dplyr::mutate(end = if_else(end > MortOingIcc, MortOingIcc, end, missing = end))
  # set time to event and set event
  df <- df %>%
    dplyr::mutate(event = if_else((MortOingIcc - falta_ing1) <= 360,
                           TRUE,
                           FALSE,
                           missing = FALSE)) %>%
    dplyr::mutate(time_to_event = if_else(as.numeric(MortOingIcc - falta_ing1) <= 360,
                                   as.numeric(MortOingIcc - falta_ing1) / 30 + 0.001,
                                   12.001))
  # filter prescription periods where end >= start
  df <- df[(df["end"] >= df["start"]) | is.na(df["end"]), ]
  # adjust duration
  df["duration"] <- df["end"] - df["start"]
  return(df)
}

reset_timeevent_vars <- function(df) {
  df <- df %>%
    dplyr::mutate(event = if_else((MortOingIcc - falta_ing1) <= 360,
                           TRUE,
                           FALSE,
                           missing = FALSE)) %>%
    dplyr::mutate(time_to_event = if_else(as.numeric(MortOingIcc - falta_ing1) <= 360,
                                   as.numeric(MortOingIcc - falta_ing1) / 30 + 0.001,
                                   12.001)) %>%
    dplyr::mutate(month = if_else(month >= time_to_event, time_to_event - 0.01, month)) %>%
    dplyr::distinct()
  return(df)
}

print("baseJoinModel_farmaco_fusion OK")