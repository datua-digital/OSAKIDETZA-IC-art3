# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

# load libraries and sources ----------------------------------------------------------
library(JMbayes)
library(survival)
library(tidyverse)
library(rms)
library(nlme)
library(splines)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))
source(paste0(COMPARABLEEMETRICSSCRIPTSPATH, "comparable_metrics_cox.R"))
source(paste0(COMPARABLEEMETRICSSCRIPTSPATH, "comparable_metrics_JM.R"))
source(paste0(COMPARABLEEMETRICSSCRIPTSPATH, "utils.R"))

# script variables and functions ---------------------------------------------------------------

df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))
M1 <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M1_.rds', sep = '/'))
M2 <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M2_.rds', sep = '/'))
patients_conditions_ <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

preprocess_dfjm_paper <- function(df_jm) {
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  variables_longitudinales <- c("cum_perc_adh_ara2oieca", "cum_perc_adh_bbloq", "cum_perc_adh_arm")
  
  patients_conditions <- list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  )
  covariables <- c("sexo", "edad_ing1", "charlson", "fe.reducida.severa", 
                   "denovo_ic_paciente", "ptot")
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(
    df_jm, 
    variables_jm = c(covariables, variables_longitudinales, variables_ids_eventos)
  )
  df_jm$ptot <- factor(df_jm$ptot)
  df_jm <- as.data.frame(df_jm)
  
  return(df_jm)
}

# Cox model Results ------------------------------------------------------------

cox_df <- get_cox_data(
  df_jm,
  patients_conditions = patients_conditions_
)


# Cox with our selection:
tprescribed_drugs_denovo_interac <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, 
  cox_df,
  x = TRUE
)

summary(tprescribed_drugs_denovo_interac)
confint(tprescribed_drugs_denovo_interac) # intervalos de confianza

# auc casero
auc_cox <- c()
for (i in c(1:11)) {
  auc_cox <- c(auc_cox, 
               auc_casero_cox(
                 df_model = cox_df[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'fe.reducida.severa', 'denovo_ic_paciente', 'ptot')], 
                 model = tprescribed_drugs_denovo_interac, 
                 Tstart = i, 
                 Thorizon = 12
               )
  )
}


auc_cox_ <- c()
for (i in c(1:11)) {
  auc_cox_ <- c(auc_cox_, 
                auc_casero_cox(
                  cox_df[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'fe.reducida.severa', 'denovo_ic_paciente', 'ptot')], 
                  tprescribed_drugs_denovo_interac, 
                  Tstart = i, 
                  Thorizon = i + 1
                )
  )
}

# precision casero
precision_cox_ <- c()
for (i in c(1:11)) {
  precision_cox_ <- c(
    precision_cox_, 
    precision_casero_cox(
      df_model = cox_df[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'fe.reducida.severa', 'denovo_ic_paciente', 'ptot')], 
      model = tprescribed_drugs_denovo_interac, 
      Tstart = i, 
      Thorizon = 12
    )
  )
}

# Cox with lasso selection:
# sexo, edad_ing1, charlson, denovo_ic_paciente, InsufRenal.C, Valvulopatia, ptot
cox_autoselected_harrelsc <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + denovo_ic_paciente + InsufRenal.C + Valvulopatia + ptot,
  cox_df
)
summary(cox_autoselected_harrelsc)
confint(cox_autoselected_harrelsc)

# auc casero
auc_cox <- c()
for (i in c(1:11)) {
  auc_cox <- c(auc_cox, 
               auc_casero_cox(
                 df_model = cox_df[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'InsufRenal.C', 'Valvulopatia', 'denovo_ic_paciente', 'ptot')], 
                 model = cox_autoselected_harrelsc, 
                 Tstart = i, 
                 Thorizon = 12
               )
  )
}


auc_cox_ <- c()
for (i in c(1:11)) {
  auc_cox_ <- c(auc_cox_, 
                auc_casero_cox(
                  df_model = cox_df[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'InsufRenal.C', 'Valvulopatia', 'denovo_ic_paciente', 'ptot')], 
                  model = cox_autoselected_harrelsc, 
                  Tstart = i, 
                  Thorizon = i + 1
                )
  )
}

# JM model Results -------------------------------------------------------------
# Metrics for M1 model

df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))
df_jm <- preprocess_dfjm_paper(df_jm)

# performance metrics for M1
summary(M1)

# auc
auc1_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 1, Thorizon = 12)
auc2_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 2, Thorizon = 12)
auc3_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 3, Thorizon = 12)
auc4_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 4, Thorizon = 12)
auc5_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 5, Thorizon = 12)
auc6_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 6, Thorizon = 12)
auc7_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 7, Thorizon = 12)
auc8_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 8, Thorizon = 12)
auc9_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 9, Thorizon = 12)
auc10_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 10, Thorizon = 12)
auc11_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 11, Thorizon = 12)

auc_jm <- c(
  auc1_12_casero_M1, auc2_12_casero_M1, auc3_12_casero_M1, auc4_12_casero_M1,
  auc5_12_casero_M1, auc6_12_casero_M1, auc7_12_casero_M1, auc8_12_casero_M1, 
  auc9_12_casero_M1, auc10_12_casero_M1, auc11_12_casero_M1
)

auc1_2_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 1, Thorizon = 2)
auc2_3_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 2, Thorizon = 3)
auc3_4_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 3, Thorizon = 4)
auc4_5_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 4, Thorizon = 5)
auc5_6_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 5, Thorizon = 6)
auc6_7_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 6, Thorizon = 7)
auc7_8_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 7, Thorizon = 8)
auc8_9_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 8, Thorizon = 9)
auc9_10_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 9, Thorizon = 10)
auc10_11_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 10, Thorizon = 11)
auc11_12_casero_M1 <- auc_casero(df_model = df_jm, model = M1, Tstart = 11, Thorizon = 1)
auc_jm2_casero_M1 <- c(
  auc1_2_casero_M1, auc2_3_casero_M1, auc3_4_casero_M1, auc4_5_casero_M1,  
  auc5_6_casero_M1, auc6_7_casero_M1, auc7_8_casero_M1, auc8_9_casero_M1, 
  auc9_10_casero_M1, auc10_11_casero_M1, auc11_12_casero_M1
)

# brier score
pe_1_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 1, Thorizon = 12)
pe_2_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 2, Thorizon = 12)
pe_3_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 3, Thorizon = 12)
pe_4_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 4, Thorizon = 12)
pe_5_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 5, Thorizon = 12)
pe_6_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 6, Thorizon = 12)
pe_7_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 7, Thorizon = 12)
pe_8_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 8, Thorizon = 12)
pe_9_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 9, Thorizon = 12)
pe_10_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 10, Thorizon = 12)
pe_11_12_casero_M1 <- precision_casero(df_model = df_jm, model = M1, Tstart = 11, Thorizon = 12)

pe_casero_M1 <- c(
  pe_1_12_casero_M1, pe_2_12_casero_M1, pe_3_12_casero_M1, pe_4_12_casero_M1,
  pe_5_12_casero_M1, pe_6_12_casero_M1, pe_7_12_casero_M1, pe_8_12_casero_M1, 
  pe_9_12_casero_M1, pe_10_12_casero_M1, pe_11_12_casero_M1
)

# prediction error
pe_1_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 1, Thoriz = 12)
pe_2_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 2, Thoriz = 12)
pe_3_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 3, Thoriz = 12)
pe_4_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 4, Thoriz = 12)
pe_5_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 5, Thoriz = 12)
pe_6_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 6, Thoriz = 12)
pe_7_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 7, Thoriz = 12)
pe_8_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 8, Thoriz = 12)
pe_9_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 9, Thoriz = 12)
pe_10_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 10, Thoriz = 12)
pe_11_12_M1 <- JMbayes::prederrJM(M1, df_jm, Tstart = 11, Thoriz = 12)


pe_M1 <- c(
  pe_1_12_M1$prederr, pe_2_12_M1$prederr, pe_3_12_M1$prederr, pe_4_12_M1$prederr,
  pe_5_12_M1$prederr, pe_6_12_M1$prederr, pe_7_12_M1$prederr, pe_8_12_M1$prederr, 
  pe_9_12_M1$prederr, pe_10_12_M1$prederr, pe_11_12_M1$prederr
)

# performance metrics for M2
summary(M2)

# auc
auc1_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 1, Thorizon = 12)
auc2_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 2, Thorizon = 12)
auc3_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 3, Thorizon = 12)
auc4_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 4, Thorizon = 12)
auc5_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 5, Thorizon = 12)
auc6_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 6, Thorizon = 12)
auc7_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 7, Thorizon = 12)
auc8_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 8, Thorizon = 12)
auc9_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 9, Thorizon = 12)
auc10_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 10, Thorizon = 12)
auc11_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 11, Thorizon = 12)

auc_jm_ <- c(
  auc1_12_casero_M2, auc2_12_casero_M2, auc3_12_casero_M2, auc4_12_casero_M2,
  auc5_12_casero_M2, auc6_12_casero_M2, auc7_12_casero_M2, auc8_12_casero_M2, 
  auc9_12_casero_M2, auc10_12_casero_M2, auc11_12_casero_M2
)

auc1_2_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 1, Thorizon = 2)
auc2_3_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 2, Thorizon = 3)
auc3_4_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 3, Thorizon = 4)
auc4_5_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 4, Thorizon = 5)
auc5_6_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 5, Thorizon = 6)
auc6_7_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 6, Thorizon = 7)
auc7_8_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 7, Thorizon = 8)
auc8_9_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 8, Thorizon = 9)
auc9_10_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 9, Thorizon = 10)
auc10_11_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 10, Thorizon = 11)
auc11_12_casero_M2 <- auc_casero(df_model = df_jm, model = M2, Tstart = 11, Thorizon = 12)
auc_jm2_casero <- c(
  auc1_2_casero_M2, auc2_3_casero_M2, auc3_4_casero_M2, auc4_5_casero_M2,
  auc5_6_casero_M2, auc6_7_casero_M2, auc7_8_casero_M2, auc8_9_casero_M2, 
  auc9_10_casero_M2, auc10_11_casero_M2, auc11_12_casero_M2
)

# brier score
pe_1_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 1, Thorizon = 12)
pe_2_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 2, Thorizon = 12)
pe_3_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 3, Thorizon = 12)
pe_4_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 4, Thorizon = 12)
pe_5_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 5, Thorizon = 12)
pe_6_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 6, Thorizon = 12)
pe_7_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 7, Thorizon = 12)
pe_8_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 8, Thorizon = 12)
pe_9_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 9, Thorizon = 12)
pe_10_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 10, Thorizon = 12)
pe_11_12_casero_M2 <- precision_casero(df_model = df_jm, model = M2, Tstart = 11, Thorizon = 12)

pe_casero_M2 <- c(
  pe_1_12_casero_M2, pe_2_12_casero_M2, pe_3_12_casero_M2, pe_4_12_casero_M2,
  pe_5_12_casero_M2, pe_6_12_casero_M2, pe_7_12_casero_M2, pe_8_12_casero_M2, 
  pe_9_12_casero_M2, pe_10_12_casero_M2, pe_11_12_casero_M2
)

# prediction error
pe_1_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 1, Thoriz = 12)
pe_2_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 2, Thoriz = 12)
pe_3_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 3, Thoriz = 12)
pe_4_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 4, Thoriz = 12)
pe_5_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 5, Thoriz = 12)
pe_6_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 6, Thoriz = 12)
pe_7_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 7, Thoriz = 12)
pe_8_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 8, Thoriz = 12)
pe_9_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 9, Thoriz = 12)
pe_10_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 10, Thoriz = 12)
pe_11_12_M2 <- JMbayes::prederrJM(M2, df_jm, Tstart = 11, Thoriz = 12)

pe_ <- c(
  pe_1_12_M2$prederr, pe_2_12_M2$prederr, pe_3_12_M2$prederr, pe_4_12_M2$prederr,
  pe_5_12_M2$prederr, pe_6_12_M2$prederr, pe_7_12_M2$prederr, pe_8_12_M2$prederr,
  pe_9_12_M2$prederr, pe_10_12_M2$prederr, pe_11_12_M2$prederr
)
