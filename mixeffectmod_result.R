# load libraries ----------------------------------------------------------
library(nlme)
library(tidyverse)
library(splines)
library(lattice)
# global environment variables --------------------------------------------
OUTPATH <- "out/"

# load sources ------------------------------------------------------------
source("utils/jm_utils.R")
source("utils/table_utils.R")
source("utils/plot_utils.R")
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESLONGS <- c("cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca", "cum_perc_adh_doctor", "cum_perc_adh_guia")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event","time_to_event", "month")
LONGVAR <- "cum_perc_adh_guia_arm"
df_jm <- readr::read_csv("data/out/df_JM.csv")
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)

modelization_longproc <- function(df_jm0, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX,
                     VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM') {
  
  # choose patients
  df_jm <- filter_patients(df_jm0, patients_conditions)

  # build data ---------------------------------------------------------------
  df_jm <- preprocess_dfjm(df = df_jm)
  print(df_jm)
  # Modelización de la variable longitudinal y el evento---------------------------------------------------------------
  long_proc <- longitudinal_process(LONGVAR = LONGVAR, data_ = df_jm, tipo = "splines_cubicas")
  return(long_proc)
}

result <- modelization_longproc(df_jm0 = df_jm,
                                patients_conditions,
                                VARIABLESCOX_IND,
                                VARIABLESCOX,
                                VARIABLESTODOS,
                                OUTPATH,
                                LONGVAR)

result$data$id
# standardized residuals versus fitted values by gender
plot(result, resid(., type = "p") ~ fitted(.), abline = 0)
# box-plots of residuals by Subject
plot(result, id ~ resid(.))
# observed versus fitted values by Subject
plot(result, cum_perc_adh_guia_arm ~ fitted(.))

obs_pred_plot(result$data, id_ = 14516937)
obs_pred_plot(result$data, id_ = 764021079)
obs_pred_plot(result$data, id_ = 844755579)

# TODO: Testear el shiny
# runDynPred("lme")