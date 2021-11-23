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
# rm("M1", "M2", "M3")

# load sources ------------------------------------------------------------
source("utils/jm_utils.R")
source("utils/table_utils.R")

# Selección de variables ---------------------------------------------------------------
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESLONGS <- c("cum_perc_adh_arm","cum_perc_adh_ara2oieca","cum_perc_adh_guia_arm","cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca", "cum_perc_adh_doctor", "cum_perc_adh_guia")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event","time_to_event", "month")


# --------------------------------------------
# PRUEBAS
# sin tener en cuenta pacientes que fallecen los primeros 30 días, y filtrando pacientes de novo en fecha ingreso
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = TRUE
)

df_jm0 <- readr::read_csv("data/out/df_JM.csv")
# 
# 
# LONGVAR <- c("cum_perc_adh_guia_arm", "cum_perc_adh_bbloq")
# 
# 
# long_process <- nlme::lme(
#   as.formula(paste(
#     paste('ns(month, 4)', collapse = '+'), paste(eval(LONGVAR), collapse = '*') , sep = '~')
#   ),
#   random = ~ ns(month, 4) | id,
#   data = df_jm,
#   control = lmeControl(opt = 'optim')
# )
# --------------------------------------------

# functions ---------------------------------------------------------------
apply_MVJM <- function(df_jm0, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX,
                     VARIABLESTODOS, OUTPATH, output = 'JM') {
  
  # choose patients
  df_jm <- filter_patients(df_jm0, patients_conditions)
  # build data ---------------------------------------------------------------
  cox_df <- generate_coxdf(df_jm)
  df_jm <- preprocess_dfjm(df_jm)
  
  #PARA DOS VARIABLES LONGITUDINALES***********************************************************
  ecuaciones <- list(
    as.formula(paste(paste("cum_perc_adh_ara2oieca" ,paste('ns(month, 4)', collapse = '+'),  sep = '~'), "(ns(month, 4) | id)", sep = '+')),
    as.formula(paste(paste("cum_perc_adh_bbloq" ,paste('ns(month, 4)', collapse = '+'),  sep = '~'), "(ns(month, 4) | id)", sep = '+'))
  )
  family <- list(gaussian, gaussian)
  
  # M1
  set.seed(1000)
  MixedModelFit1 <- mvglmer(ecuaciones,
                            data = df_jm,
                            families = family)
  
  surv_object <- Surv(time = cox_df$time_to_event,
                      event = as.numeric(cox_df$event))
  
  coxFit.df_jm <- coxph(as.formula(paste("surv_object", paste(VARIABLESCOX, collapse = "+"), sep = "~")), 
                        data = cox_df,
                        x = TRUE,
                        model = TRUE)
  
  M1 <- mvJointModelBayes(MixedModelFit1, coxFit.df_jm, timeVar = "month")
  summary(M1)
  saveRDS(M1, paste0("out/", output, "_M1_MVJM_V2", ".rds"))
  
  
  # M2
  forms <- list(
    "cum_perc_adh_ara2oieca" = "value",
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_bbloq"     = "value",
    "cum_perc_adh_bbloq"     = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5 )
  )
  M2 <- update(M1, Formulas = forms)
  saveRDS(M2, paste0("out/", output, "_M2_MVJM_V2", ".rds"))
  
  
  # M3
  forms <- list(
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5),
    "cum_perc_adh_bbloq"    = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M3 <- update(M1, Formulas = forms)
  saveRDS(M3, paste0("out/", output, "_M3_MVJM_V2", ".rds"))
  
  #PARA TRES VARIABLES LONGITUDINALES***************************************************************
  rm("M1", "M2", "M3")
  # Lista de ecuaciones con su respectivas familiasa
  ecuaciones <- list(
    as.formula(paste(paste("cum_perc_adh_ara2oieca" ,paste('ns(month, 4)', collapse = '+'),  sep = '~'), "(ns(month, 4) | id)", sep = '+')),
    as.formula(paste(paste("cum_perc_adh_bbloq" ,paste('ns(month, 4)', collapse = '+'),  sep = '~'), "(ns(month, 4) | id)", sep = '+')),
    as.formula(paste(paste("cum_perc_adh_arm" ,paste('ns(month, 4)', collapse = '+'),  sep = '~'), "(ns(month, 4) | id)", sep = '+'))
  )
  family <- list(gaussian, gaussian, gaussian)
  
  # M1
  set.seed(1000)
  MixedModelFit1 <- mvglmer(ecuaciones,
                            data = df_jm,
                            families = family)
  
  surv_object <- Surv(time = cox_df$time_to_event,
                      event = as.numeric(cox_df$event))
  
  coxFit.df_jm <- coxph(as.formula(paste("surv_object", paste(VARIABLESCOX, collapse = "+"), sep = "~")), 
                        data = cox_df,
                        x = TRUE,
                        model = TRUE)

  M1 <- mvJointModelBayes(MixedModelFit1, coxFit.df_jm, timeVar = "month")
  summary(M1)
  saveRDS(M1, paste0("out/", output, "_M1_MVJM_V3", ".rds"))
  
  
  # M2
  forms <- list(
    "cum_perc_adh_ara2oieca" = "value",
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_bbloq"     = "value",
    "cum_perc_adh_bbloq"     = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_arm"       = "value",
    "cum_perc_adh_arm"       = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M2 <- update(M1, Formulas = forms)
  saveRDS(M2, paste0("out/", output, "_M2_MVJM_V3", ".rds"))
  
  
  # M3
  forms <- list(
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5),
    "cum_perc_adh_bbloq"    = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_arm"      = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M3 <- update(M1, Formulas = forms)
  saveRDS(M3, paste0("out/", output, "_M3_MVJM_V3", ".rds"))
}

# JM para adherencia guia (con arm) ---------------------------------------------------------------------
# LONGVAR <- c("cum_perc_adh_guia_arm", "cum_perc_adh_bbloq")

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra teniendo en cuenta todos los id-s
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

apply_MVJM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, output = 'JM')

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra filtrando pacientes con prescripciones:
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = TRUE
)
apply_JM(df_jm0 = df_jm, patients_conditions = patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, output = 'JM1')

df_jm <- readr::read_csv("data/out/df_JM.csv")
# Subset: Muestra sin tener en cuenta pacientes que fallecen los primeros 30 días
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)
apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, output = 'JM2')

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
apply_JM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, output = 'JM3')


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
apply_MVJM(df_jm0 = df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, output = 'JM4')



# # JM para data.frame completo ---------------------------------------------
# 
# # JM para variable ara2:
# LONGVAR <- "cum_perc_adh_ara2"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH)
# 
# # JM para variable ieca:
# LONGVAR <- "cum_perc_adh_ieca"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH)
# 
# # JM para variable bbloq:
# LONGVAR <- "cum_perc_adh_bbloq"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH)
# 
# # JM para variable adh_doctor:
# LONGVAR <- "cum_perc_adh_doctor"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH)
# 
# # JM para variable adh_guia:
# LONGVAR <- "cum_perc_adh_guia"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH)
# 
# # jm_table1 <- get_table(OUTPATH, output, LONGVAR = "cum_perc_adh_ara2", save = FALSE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)
# 
# 
# # JM para data.frame con prescripciones ---------------------------------------------
# # sin pacientes que no tienen prescripciones
# df_jm <- readr::read_csv("data/out/df_JM2.csv")
# patients_conditions <- list(
#   denovo_ic_paciente = NULL,
#   denovo_tt_paciente_fing = NULL,
#   denovo_tt_paciente_falta = NULL,
#   early_death_patient_30 = NULL,
#   patient_with_prescription = TRUE
# )
# 
# # JM para variable ara2:
# LONGVAR <- "cum_perc_adh_ara2"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')
# 
# # JM para variable ieca:
# LONGVAR <- "cum_perc_adh_ieca"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')
# 
# # JM para variable bbloq:
# LONGVAR <- "cum_perc_adh_bbloq"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')
# 
# # JM para variable adh_doctor:
# LONGVAR <- "cum_perc_adh_doctor"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')
# 
# # JM para variable adh_guia:
# LONGVAR <- "cum_perc_adh_guia"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido1')
# 
# output <- 'JM_reducido1'
# 
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)
# 
# 
# # JM para data.frame con prescripciones y sin pacientes que fallecen los primeros 30 días JM_reducido2---------------------------------------------
# # sin pacientes que no tienen prescripciones
# df_jm <- readr::read_csv("data/out/df_JM2.csv")
# patients_conditions <- list(
#   denovo_ic_paciente = NULL,
#   denovo_tt_paciente_fing = NULL,
#   denovo_tt_paciente_falta = NULL,
#   early_death_patient_30 = FALSE,
#   patient_with_prescription = TRUE
# )
# 
# # JM para variable ara2:
# LONGVAR <- "cum_perc_adh_ara2"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')
# 
# # JM para variable ieca:
# LONGVAR <- "cum_perc_adh_ieca"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')
# 
# # JM para variable bbloq:
# LONGVAR <- "cum_perc_adh_bbloq"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')
# 
# # JM para variable adh_doctor:
# LONGVAR <- "cum_perc_adh_doctor"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')
# 
# # JM para variable adh_guia:
# LONGVAR <- "cum_perc_adh_guia"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido2')
# 
# output <- 'JM_reducido2'
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)
# 
# # JM para data.frame con prescripciones, sin pacientes que fallecen los primeros 30 días JM_reducido3---------------------------------------------
# # sin pacientes que no tienen prescripciones
# df_jm <- readr::read_csv("data/out/df_JM2.csv")
# patients_conditions <- list(
#   denovo_ic_paciente = TRUE,
#   denovo_tt_paciente_fing = TRUE,
#   denovo_tt_paciente_falta = NULL,
#   early_death_patient_30 = FALSE,
#   patient_with_prescription = TRUE
# )
# 
# # JM para variable ara2:
# LONGVAR <- "cum_perc_adh_ara2"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')
# 
# # JM para variable ieca:
# LONGVAR <- "cum_perc_adh_ieca"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')
# 
# # JM para variable bbloq:
# LONGVAR <- "cum_perc_adh_bbloq"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')
# 
# # JM para variable adh_doctor:
# LONGVAR <- "cum_perc_adh_doctor"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducido3')
# 
# # JM para variable adh_guia:
# LONGVAR <- "cum_perc_adh_guia"
# apply_JM(df_jm, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM_reducidotest2')
# 
# output <- 'JM_reducido3'
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ara2", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_ieca", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_bbloq", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_doctor", save = TRUE)
# get_table(OUTPATH, output = 'JM', LONGVAR = "cum_perc_adh_guia", save = TRUE)