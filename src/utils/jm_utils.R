# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

library(nlme)

longitudinal_process <- function(variable_longitudinal, data_, tipo = 'splines_cubicas') {
  if (tipo == 'splines_cubicas') {
    long_process <- nlme::lme(
      as.formula(paste(
        eval(variable_longitudinal), paste('ns(month, 4)', collapse = '+'), sep = '~')
      ),
      random = ~ ns(month, 4) | id,
      data = data_,
      control = lmeControl(opt = 'optim')
    )
  }
  return(long_process)
}

generate_coxdf <- function(df_jm, variables_cox) {
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  cox_df <- cox_df[variables_cox]
  cox_df <- cox_df %>% dplyr::arrange(id, month)
  return(cox_df)
}

preprocess_dfjm <- function(df, variables_jm) {
  df <- df[variables_jm]
  df <- df %>% dplyr::arrange(id, month)
  
  df$time_to_event <- round(df$time_to_event, 6)
  df$month <- round(df$month, 6)
  
  return(df)
}

filter_patients <- function(df, patients_conditions) {
  if (!is.null(patients_conditions$denovo_ic_paciente)) {
    df <- df[df$denovo_ic_paciente == patients_conditions$denovo_ic_paciente, ]
  }
  if (!is.null(patients_conditions$denovo_tt_paciente_fing)) {
    df <- df[df$denovo_tt_paciente_fing == patients_conditions$denovo_tt_paciente_fing, ]
  }
  if (!is.null(patients_conditions$denovo_tt_paciente_falta)) {
    df <- df[df$denovo_tt_paciente_falta == patients_conditions$denovo_tt_paciente_falta, ]
  }
  if (!is.null(patients_conditions$early_death_patient_30)) {
    df <- df[df$early_death_patient_30 == patients_conditions$early_death_patient_30, ]
  }
  if (!is.null(patients_conditions$patient_with_prescription)) {
    df <- df[df$patient_with_prescription == patients_conditions$patient_with_prescription, ]
  }
  print(dim(df))
  return(df)
}

get_variable_longitudinal_name <- function(variable_longitudinal) {
  if (variable_longitudinal == "cum_perc_adh_guia_arm") {
    return("guia")
  } else {
    return(variable_longitudinal)
  }
  return(df)
}

data_for_event <- function(event_var) {
  if (event_var == "MortOingIcc") {
    return("df_JM_MortOingIcc")
  } else if (event_var == "fmort2") {
    return("df_JM_fmort2")
  }
}