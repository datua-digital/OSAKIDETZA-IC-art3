# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

library(dplyr)


#' add_months
#'
#' @param x subset of df. One of the patient of the cohort.
#' @param months_toadd (numeric vector) months should add for the patient
add_months <- function(x, months_toadd) {
  if (length(months_toadd) > 0) {
    df_row <- x[1, ]
    df_rows <- df_row[rep(1, length(months_toadd)), ]
    df_rows[c("perc_adh_ara2", "perc_adh_bbloq", "perc_adh_ieca",
              "perc_adh_guia", "perc_adh_doctor", "perc_adh_arm", 
              "perc_adh_ara2oieca", "perc_adh_guia_arm")] <- 0
    df_rows$month <- c(months_toadd)
    final_x <- rbind(x, df_rows)
  } else{
    final_x <- x
  }
  return(final_x)
}


#' input_patients_noprescriptions
#'
#' @param df (data.frame) Most update data frame of the workflow
#' @param allid_df (data.frame) data frame including all initial id-s (3188)
input_patients_noprescriptions <- function(df, allid_df) {
  # find ids without prescription
  ids_withoutprescription <- base::setdiff(allid_df$id, df$id)
  # complete druginfo_cols
  druginfo_cols <- base::setdiff(colnames(df), colnames(allid_df))
  allid_df[druginfo_cols] <- as.numeric(0)
  # build df-s of ids without prescription
  l_df_rows <- list()
  count <- 1
  for (id_withoutprescription in ids_withoutprescription) {
      df_row <- allid_df[allid_df$id == id_withoutprescription, ]
      df_rows <- df_row[rep(1, 12), ]
      df_rows$month <- c(1:12)
      df_rows$last_month <- NA
      df_rows$last_day <- NA
      df_rows$dura_in_months <- NA
      df_rows$patient_with_prescription <- FALSE
      l_df_rows[[count]] <- df_rows
      count <- count + 1
  }
  
  # bind df rows with ids without description
  df <- dplyr::bind_rows(df, l_df_rows)
  return(df)
}


#' input_initial_months
#'
#' @param x subset of df. One of the patient of the cohort.
#'
#' @return a patient with initial months inputed
input_initial_months <- function(x, months_toadd=c()) {
  if (min(x$month) > 1) {
    months_toadd <- c(1:(min(x$month) - 1))
  }
  final_x <- add_months(x, months_toadd)
  return(final_x)
}


#' input_patients_noiniprescriptions
#'
#' @param df (data.frame) Most update data frame of the workflow
#'
#' @return df with new rows added for patients without initial prescriptions
input_patients_noiniprescriptions <- function(df) {
  df <- df %>%
    group_by(id) %>%
    group_modify(~input_initial_months(.x))
  return(df)
}


#' input_final_months
#'
#' @param x subset of df. One of the patient of the cohort.
#' @param FOLLOWUP (numeric) global param FOLLOWUP
input_final_months <- function(x, FOLLOWUP, months_toadd=c()) {
  if (!any(is.na(x$fecha_evento))) {
    if (unique(x$fecha_evento) < unique(x$falta_ing1 + FOLLOWUP)) {
      max_month <- min(ceiling(as.numeric(unique(x$fecha_evento - x$falta_ing1 + 1), units = "days") / 30), 12)
    } else {
      max_month <- 12
    }
  } else{
    max_month <- 12
  }
  if (max(x$month) < max_month) {
    months_toadd <- c((max(x$month) + 1):max_month)
  }
  final_x <- add_months(x, months_toadd)
  return(final_x)
}


#' input_patients_nofinprescriptions
#'
#' @param df (data.frame) Most update data frame of the workflow
#'
#' @return df with new rows added for patients without final prescriptions
input_patients_nofinprescriptions <- function(df, FOLLOWUP) {
  df <- df %>%
    group_by(id) %>%
    group_modify(~input_final_months(.x, FOLLOWUP))
  return(df)
}


#' input_intermediate_months
#'
#' @param x subset of df. One of the patient of the cohort.
#'
#' @return a patient with intermediate months inputed
input_intermediate_months <- function(x, months_toadd=c()) {
  if (length(x$month) != (max(x$month) - min(x$month) + 1)) {
    for (n_month in c(min(x$month):max(x$month))) {
      if (!n_month %in% x$month) {
        months_toadd <- c(months_toadd, n_month)
      }
    }
  }
  final_x <- add_months(x, months_toadd)
  return(final_x)
}


#' input_patients_nointerprescriptions
#'
#' @param df (data.frame) Most update data frame of the workflow
#'
#' @return df with new rows added for patients without intermediate prescriptions
input_patients_nointerprescriptions <- function(df) {
  df <- df %>%
    group_by(id) %>%
    group_modify(~input_intermediate_months(.x))
  return(df)
}


input_adhvars <- function(df) {
  df[is.na(df$perc_adh_ara2), "perc_adh_ara2"] <- 0
  df[is.na(df$perc_adh_bbloq), "perc_adh_bbloq"] <- 0
  df[is.na(df$perc_adh_ieca), "perc_adh_ieca"] <- 0
  df[is.na(df$perc_adh_arm), "perc_adh_arm"] <- 0
  df[is.na(df$perc_adh_doctor), "perc_adh_doctor"] <- 0
  df[is.na(df$perc_adh_guia), "perc_adh_guia"] <- 0
  df[is.na(df$perc_adh_ara2oieca), "perc_adh_ara2oieca"] <- 0
  df[is.na(df$perc_adh_guia_arm), "perc_adh_guia_arm"] <- 0
  
  return(df)
}


print("input_months OK")
