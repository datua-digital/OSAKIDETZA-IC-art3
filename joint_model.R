# load libraries ----------------------------------------------------------
# 
library(JM)
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
# global environment variables --------------------------------------------
OUTPATH <- "out/"

# load sources ------------------------------------------------------------
source("utils/jm_utils.R")
source("utils/table_utils.R")


# Selección de variables ---------------------------------------------------------------
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event", "time_to_event", "month", "cum_perc_adh_ara2", "last_month")
df_jm <- readr::read_csv("data/out/df_JM.csv")
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

# functions ---------------------------------------------------------------
generate_coxdf <- function(df_jm) {
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  cox_df <- cox_df[VARIABLESTODOS]
  cox_df <- cox_df %>% dplyr::arrange(id, month)
  return(cox_df)
}

preprocess_dfjm <- function(df) {
  df_jm <- df_jm[VARIABLESTODOS]
  df_jm <- df_jm %>% dplyr::arrange(id, month)
  return(df)
}

get_table <- function(OUTPATH, output, LONGVAR, save = FALSE) {
  M1 <- readRDS(paste0(OUTPATH, output, "_M1_", LONGVAR, ".rds"))
  M2 <- readRDS(paste0(OUTPATH, output, "_M2_", LONGVAR, ".rds"))
  M3 <- readRDS(paste0(OUTPATH, output, "_M3_", LONGVAR, ".rds"))
  JM_table <- summary_table(M1, M2, M3)
  if (save) {
    saveRDS(JM_table, paste0(OUTPATH, output, "JM_table_", LONGVAR, ".rds"))
  }
  rm(M1, M2, M3)
  return(JM_table)
}
saveRDS(JM_table, paste0(OUTPATH, output, "JM_table_", LONGVAR, ".rds"))
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
  return(df)
}

apply_JM <- function(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX,
                     VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM') {
  
  # choose patients
  df_jm <- filter_patients(df_jm, patients_conditions)
  # build data ---------------------------------------------------------------
  cox_df <- generate_coxdf(df_jm)
  df_jm <- preprocess_dfjm(df_jm)
  
  # Modelización de la variable longitudinal y el evento---------------------------------------------------------------
  long_proc <- longitudinal_process(LONGVAR = LONGVAR, data_ = df_jm, tipo = "splines_cubicas")
  
  surv_object <- Surv(time = cox_df$time_to_event,
                      event = as.numeric(cox_df$event))
  
  coxFit.df_jm <- coxph(as.formula(paste("surv_object", paste(VARIABLESCOX, collapse = "+"), sep = "~")), 
                        data = cox_df,
                        x = TRUE,
                        model = TRUE)
  
  # Modelización del proceso del evento ---------------------------------------------------------------
  # M1: Fit JM with longitudinal process (4) and event process (6)
  M1 <- JMbayes::jointModelBayes(
    long_proc,
    coxFit.df_jm,
    timeVar = "month",
    n.iter = 30000,
    n.burnin = 3000)
  saveRDS(M1, paste0(OUTPATH, paste0(output, "_M1_", LONGVAR, ".rds")))
  
  
  # M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
  dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                indFixed = 2:5, indRandom = 2:5)
  M2 <- update(M1, param = "td-both", extraForm = dForm)
  saveRDS(M2, paste0(OUTPATH, output, "_M2_", LONGVAR, ".rds"))
  
  
  # M3: Fit JM with longitudinal process (4) y componente de tendencia
  dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                indFixed = 2:5, indRandom = 2:5)
  M3 <- update(M1, param = "td-extra", extraForm = dForm)
  saveRDS(M3, paste0(OUTPATH, output, "_M3_", LONGVAR, ".rds"))
  
  # Generar tabla resultados ---------------------------------------------------------------
  JM_table <- summary_table(M1, M2, M3)
  saveRDS(JM_table, paste0(OUTPATH, output, "JM_table_", LONGVAR, ".rds"))
}

# JM para data.frame completo ---------------------------------------------

# JM para variable ara2:
LONGVAR <- "cum_perc_adh_ara2"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable ieca:
LONGVAR <- "cum_perc_adh_ieca"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable bbloq:
LONGVAR <- "cum_perc_adh_bbloq"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable adh_doctor:
LONGVAR <- "cum_perc_adh_doctor"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable adh_guia:
LONGVAR <- "cum_perc_adh_guia"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# jm_table1 <- get_table(OUTPATH, output, LONGVAR = "cum_perc_adh_ara2", save = FALSE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)


# JM para data.frame con prescripciones ---------------------------------------------
# sin pacientes que no tienen prescripciones
df_jm <- readr::read_csv("data/out/df_JM2.csv")
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = TRUE
)

# JM para variable ara2:
LONGVAR <- "cum_perc_adh_ara2"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')

# JM para variable ieca:
LONGVAR <- "cum_perc_adh_ieca"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')

# JM para variable bbloq:
LONGVAR <- "cum_perc_adh_bbloq"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')

# JM para variable adh_doctor:
LONGVAR <- "cum_perc_adh_doctor"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')

# JM para variable adh_guia:
LONGVAR <- "cum_perc_adh_guia"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')

output <- 'JM_reducido1'

get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)


# JM para data.frame con prescripciones y sin pacientes que fallecen los primeros 30 días JM_reducido2---------------------------------------------
# sin pacientes que no tienen prescripciones
df_jm <- readr::read_csv("data/out/df_JM2.csv")
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = TRUE
)

# JM para variable ara2:
LONGVAR <- "cum_perc_adh_ara2"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')

# JM para variable ieca:
LONGVAR <- "cum_perc_adh_ieca"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')

# JM para variable bbloq:
LONGVAR <- "cum_perc_adh_bbloq"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')

# JM para variable adh_doctor:
LONGVAR <- "cum_perc_adh_doctor"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')

# JM para variable adh_guia:
LONGVAR <- "cum_perc_adh_guia"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')

output <- 'JM_reducido2'
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)

# JM para data.frame con prescripciones, sin pacientes que fallecen los primeros 30 días JM_reducido3---------------------------------------------
# sin pacientes que no tienen prescripciones
df_jm <- readr::read_csv("data/out/df_JM2.csv")
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = TRUE
)

# JM para variable ara2:
LONGVAR <- "cum_perc_adh_ara2"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')

# JM para variable ieca:
LONGVAR <- "cum_perc_adh_ieca"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')

# JM para variable bbloq:
LONGVAR <- "cum_perc_adh_bbloq"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')

# JM para variable adh_doctor:
LONGVAR <- "cum_perc_adh_doctor"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')

# JM para variable adh_guia:
LONGVAR <- "cum_perc_adh_guia"
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')

output <- 'JM_reducido3'
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)