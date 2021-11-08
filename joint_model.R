#TODO: Replicar un modelo con la librería rstanarm

# load libraries ----------------------------------------------------------
# 
library(rstanarm)
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
library(splines)
# global environment variables --------------------------------------------
OUTPATH <- "out/"
rm(list = list("M1", "M2", "M3"))

# load sources ------------------------------------------------------------
source("utils/jm_utils.R")
source("utils/table_utils.R")

# Selección de variables ---------------------------------------------------------------
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESLONGS <- c("cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca", "cum_perc_adh_doctor", "cum_perc_adh_guia")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event","time_to_event", "month")

# functions ---------------------------------------------------------------
apply_JM <- function(df_jm0, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX,
                     VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM') {
  
  # choose patients
  df_jm <- filter_patients(df_jm0, patients_conditions)
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
  
  # Generar tabla resultados ----------------------------------------------------------------------------
  JM_table <- summary_table(M1, M2, M3)
  saveRDS(JM_table, paste0(OUTPATH, output, "JM_table_", LONGVAR, ".rds"))
}

# JM para adherencia guia (con arm) ---------------------------------------------------------------------
LONGVAR <- "cum_perc_adh_guia_arm"

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra teniendo en cuenta todos los id-s
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra filtrando pacientes con prescripciones:
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = TRUE
)
apply_JM(df_jm0 = df_jm, patients_conditions = patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM1')

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra sin tener en cuenta pacientes que fallecen los primeros 30 días
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)
apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM2')

# TODO Ejecutar joint model
# Subset: Muestra sin tener en cuenta pacientes que fallecen los primeros 30 días 
df_jm <- readr::read_csv("data/out/df_JM.csv")
# y filtrando pacientes de novo en fecha ingreso
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)
apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM3')


# Subset: Muestra filtrando pacientes con prescripciones, 
df_jm <- readr::read_csv("data/out/df_JM.csv")
# sin tener en cuenta pacientes que fallecen los primeros 30 días, y filtrando pacientes de novo en fecha ingreso
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = TRUE
)
apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM4')



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
apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducidotest2')

output <- 'JM_reducido3'
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)