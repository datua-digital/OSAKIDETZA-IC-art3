# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

# load libraries and sources----------------------------------------------------------
library(nlme)
library(tidyverse)
library(splines)
library(lattice)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))


# Script ---------------------------------------------------------------

modelization_longproc <- function(df_jm, patients_conditions, variable_longitudinal) {
  # build and preprocess data
  df_jm <- filter_patients(df_jm, patients_conditions)
  variable_longitudinal <<- variable_longitudinal
  df_jm <- df_jm %>% dplyr::arrange(id, month)
  # Modelización de la variable longitudinal y el evento
  long_proc <- longitudinal_process(
    variable_longitudinal = variable_longitudinal,
    data_ = df_jm,
    tipo = "splines_cubicas"
  )
  return(long_proc)
}

result <- modelization_longproc(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds")),
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  ),
  variable_longitudinal
)


# standardized residuals versus fitted values by gender
plot(result, resid(., type = "p") ~ fitted(.), abline = 0)
# observed versus fitted values by Subject
plot(result, cum_perc_adh_ara2oieca ~ fitted(.))

obs_pred_plot(df = result$data, id_ = 14516937, longvar = LONGVAR)
obs_pred_plot(df = result$data, id_ = 764021079, longvar = LONGVAR)
obs_pred_plot(df = result$data, id_ = 844755579, longvar = LONGVAR)
obs_pred_plot(df = result$data, id_ = 1184371008, longvar = LONGVAR)
obs_pred_plot(df = result$data, id_ = 1185106353, longvar = LONGVAR)

result$data[result$data$id == 844755579, 'cum_perc_adh_guia_arm']
# TODO: Testear el shiny
# runDynPred("lme")