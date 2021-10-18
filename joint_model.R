# load libraries ----------------------------------------------------------
# 
library(JM)
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
# global environment variables --------------------------------------------
OUTPATH = "out/"

# load sources ------------------------------------------------------------
source("utils/jm_utils.R")
source("utils/table_utils.R")


# Selección de variables ---------------------------------------------------------------
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event", "time_to_event", "month", "cum_perc_adh_ara2", "last_month")
df_jm <- readr::read_csv("data/out/df_JM.csv")

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

apply_JM <- function(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR) {
  
  # build data ---------------------------------------------------------------
  cox_df <- generate_coxdf(df_jm)
  dfjm <- preprocess_dfjm(df_jm)
  
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
  saveRDS(M1, paste0(OUTPATH, paste0("JM_M1_", LONGVAR, ".rds")))
  
  
  # M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
  dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                indFixed = 2:5, indRandom = 2:5)
  M2 <- update(M1, param = "td-both", extraForm = dForm)
  saveRDS(M2, paste0(OUTPATH, "JM_M2_", LONGVAR, ".rds"))
  
  
  # M3: Fit JM with longitudinal process (4) y componente de tendencia
  dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                indFixed = 2:5, indRandom = 2:5)
  M3 <- update(M1, param = "td-extra", extraForm = dForm)
  saveRDS(M3, paste0(OUTPATH, "JM_M3_", LONGVAR, ".rds"))
  
  # Generar tabla resultados ---------------------------------------------------------------
  JM_table <- summary_table(M1, M2, M3)
  saveRDS(JM_table, paste0(OUTPATH, "JM_table_", LONGVAR, ".rds"))
  # Guardar datos
  # save(M1, M2, M3, JM_table, file = paste0("JM_", LONGVAR, ".RData"))
}
M1 <- readRDS(paste0(OUTPATH, paste0("JM_M1_", LONGVAR, ".rds")))
M2 <- readRDS(paste0(OUTPATH, "JM_M2_", LONGVAR, ".rds"))
M3 <- readRDS(paste0(OUTPATH, "JM_M3_", LONGVAR, ".rds"))

# JM para data.frame completo ---------------------------------------------

# JM para variable ara2:
LONGVAR <- "cum_perc_adh_ara2"
apply_JM(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable ieca:
LONGVAR <- "cum_perc_adh_ieca"
apply_JM(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable bbloq:
LONGVAR <- "cum_perc_adh_bbloq"
apply_JM(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable adh_doctor:
LONGVAR <- "cum_perc_adh_doctor"
apply_JM(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)

# JM para variable adh_guia:
LONGVAR <- "cum_perc_adh_guia"
apply_JM(df_jm, VARIABLESCOX_IND, VARIABLESCOX, VARIABLESTODOS, OUTPATH, LONGVAR)


# JM para data.frame sin prescripciones ---------------------------------------------
# sin pacientes que no tienen prescripciones


# JM para data.frame sin prescripciones ---------------------------------------------
# sin pacientes que no tienen prescripciones