# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

library(dplyr)
library(cohorteicc2)


#' Merge farmacos_traye and baseJoinModel by id
#'
#' @param df1 (data.frame)
#' @param df2 (data.frame)
#'
#' @return df (data.frame) merged df
merge_byid <- function(df1, df2) {
  df <- df1 %>% dplyr::left_join(df2, by = "id")
  return(df)
}


#' process resultant data frame from merging baseJoinModel and farmacos_traye
#'
#' @param df (data.frame)
#' @param duration (numeric) global param FOLLOW_UP
#'
#' @return df processed

#TODO: Creo que la duración habría que cambiarlo a 360, aunque es posible que tenerlo
# en 365 no afecte en nada al código.
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
    dplyr::mutate(end = if_else(end > fecha_evento, fecha_evento, end, missing = end))
  
  # set time to event and event
  df <- set_event_time_to_event(df)
  
  # filter prescription periods where end > start. end == start is also discarded.
  df <- df[(df["end"] > df["start"]) | is.na(df["end"]), ]
  # adjust duration
  df["duration"] <- df["end"] - df["start"]
  return(df)
}

reset_timeevent_vars <- function(df) {
  adjusted_factor <- 0.001
  # set time to event and event for MortOingIcc
  df <- set_event_time_to_event(df)
  
  df <- df %>%
    dplyr::mutate(
      month = if_else(
        month >= time_to_event, 
        time_to_event - adjusted_factor,
        month
      )
    ) %>%
    dplyr::distinct()
  return(df)
}

set_event_time_to_event <- function(df) {
  adjusted_factor <- 0.001
  
  df <- df %>%
    dplyr::mutate(
      event = as.numeric(
        if_else(
          (fecha_evento - falta_ing1) <= 360,
          TRUE,
          FALSE,
          missing = FALSE
        )
      )
    ) %>%
    dplyr::mutate(
      time_to_event = if_else(
        as.numeric(fecha_evento - falta_ing1) <= 360,
        as.numeric(fecha_evento - falta_ing1) / 30 + adjusted_factor,
        12 + adjusted_factor,
        missing = 12 + adjusted_factor
      )
    )
  return(df)
}

print("baseJoinModel_farmaco_fusion OK")