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

# plot_longitudinal_process <- function(data_, id, variable )
# long_ids <- names(which(table(df_td$id) > 6))
# ids <- sample(long_ids, 16)
# xyplot(as.formula(paste(variable_longitudinal, paste('dura_acumulada | id', collapse = '+'), sep='~')),
#        data = df_td, 
#        subset = id %in% ids, type = c("p", "smooth"), 
#        lwd = 2, layout = c(4, 4))