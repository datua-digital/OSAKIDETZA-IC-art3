#TODO: Replicar un modelo con la librería rstanarm

# load libraries and sources ----------------------------------------------------------
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
library(splines)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))

rm("M1", "M2", "M3")  # delete models

# functions ---------------------------------------------------------------
apply_JM <- function(df_jm, patients_conditions, covariables, variable_longitudinal, model_name_prefix = 'JM', save_model = FALSE) {
  # fixed variables: 
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  variable_longitudinal <<- variable_longitudinal  # es necesario para que no de un error en jointModelBayes
  variable_longitudinal_name <- get_variable_longitudinal_name(variable_longitudinal)
  
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
  
  # Modelización de la variable longitudinal y el evento
  long_proc <- longitudinal_process(
    variable_longitudinal = variable_longitudinal,
    data_ = df_jm,
    tipo = "splines_cubicas"
  )
  
  # Modelo de Cox
  coxFit.df_jm <- coxph(
    as.formula(paste("Surv(time_to_event, event)", paste(covariables, collapse = "+"), sep = "~")),
    data = cox_df,
    x = TRUE,
    model = TRUE
  )

  # M1: Fit JM with longitudinal process (4) and event process (6)
  M1 <- JMbayes::jointModelBayes(
    long_proc,
    coxFit.df_jm,
    timeVar = "month",
    n.iter = 30000,
    n.burnin = 3000
  )
  
  if (save_model) {
    saveRDS(M1, paste0(OUTPATH_UNIV, paste0(model_name_prefix, "_M1_", variable_longitudinal_name, ".rds")))
  }

  # M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
  dForm <- list(
    fixed = ~ 0 + dns(month, 4), 
    random = ~ 0 + dns(month, 4),
    indFixed = 2:5, 
    indRandom = 2:5
  )
  M2 <- update(
    M1, 
    param = "td-both", 
    extraForm = dForm
  )
  if (save_model) {
    saveRDS(M2, paste0(OUTPATH_UNIV, model_name_prefix, "_M2_", variable_longitudinal_name, ".rds"))
  }
  auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 1, Thoriz = 12)
  # M3: Fit JM with longitudinal process (4) y componente de tendencia
  M3 <- update(
    M1, 
    param = "td-extra", 
    extraForm = dForm
  )
  if (save_model) {
    saveRDS(M3, paste0(OUTPATH_UNIV, model_name_prefix, "_M3_", variable_longitudinal_name, ".rds"))
  }
  
  # Generar tabla resultados ----------------------------------------------------------------------------

  JM_table <- summary_table(
    m1 = M1,
    m2 = M2,
    m3 = M3,
    df_jm = df_jm,
    cox_vars = covariables
  )
  saveRDS(JM_table, paste0(OUTPATH_UNIV, model_name_prefix, "JM_table_", variable_longitudinal_name, ".rds"))
  rm("M1", "M2", "M3")
}

# JM para adherencia guia (con arm) ---------------------------------------------------------------------
apply_JM(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds")), 
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  ), 
  covariables = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa", 
                  "denovo_ic_paciente", "ptot"
                  ), 
  variable_longitudinal = "cum_perc_adh_guia_arm", 
  model_name_prefix = 'JM_ptot0', 
  save_model = FALSE
)

# Subset: Muestra sin tener en cuenta pacientes que fallecen los primeros 30 días 
# y filtrando pacientes de novo en fecha ingreso

apply_JM(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds")), 
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  ), 
  covariables = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"), 
  variable_longitudinal = "cum_perc_adh_guia_arm", 
  model_name_prefix = 'JM_1', 
  save_model = TRUE
)
