# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

library(rstanarm)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")

# functions ---------------------------------------------------------------
apply_stanJM <- function(df_jm, patients_conditions, covariables, variable_longitudinal) {
  
  # fixed variables: 
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  variable_longitudinal <<- variable_longitudinal  # es necesario para que no de un error en jointModelBayes
  
  # build and preprocess data
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(
    df_jm, 
    variables_jm = c(covariables, variable_longitudinal, variables_ids_eventos)
  )
  cox_df <- generate_coxdf(
    df_jm,
    variables_cox = c(covariables, variables_ids_eventos)
  )
  
  M <- stan_jm(
    formulaLong = as.formula(paste(paste(eval(variable_longitudinal), paste('ns(month, 4)', collapse = '+'), sep = '~'),'+ (~ ns(month, 4) | id )')),
    dataLong = df_jm,
    formulaEvent = as.formula(paste("Surv(time_to_event, event)", paste(covariables, collapse = "+"), sep = "~")),
    dataEvent = cox_df,
    time_var = "month",
  )
  return(M)

}

# test
apply_stanJM(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM.rds")), 
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  ), 
  covariables = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"), 
  variable_longitudinal = "cum_perc_adh_guia_arm"
)
