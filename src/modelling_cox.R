# load libraries and sources----------------------------------------------------------
library(JMbayes)
library(survival)
library(tidyverse)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))

# script variables----------------------------------------------------------
df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))

# funciones

get_cox_data <- function(patients_conditions) {
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(df_jm, variables_jm = colnames(df_jm))
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  return(cox_df)
}

# Cox univariante: Subset de todos los pacientes -------------------------------------------

# choose patients

cox_df <- get_cox_data(
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  )
)

t1 <- coxph(Surv(time_to_event, event) ~ sexo, cluster = id, cox_df)
t2 <- coxph(Surv(time_to_event, event) ~ edad_ing1, cluster = id, cox_df)
t3 <- coxph(Surv(time_to_event, event) ~ charlson, cluster = id, cox_df)
t4 <- coxph(Surv(time_to_event, event) ~ fe.reducida.severa, cluster = id, cox_df)


# Cox univariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------

cox_df <- get_cox_data(
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  )
)

coxph(Surv(time_to_event, event) ~ sexo, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ fe.reducida.severa, cluster = id, cox_df)

# Cox multivariante: Subset de todos los pacientes -------------------------------------------
# choose patients
cox_df <- get_cox_data(
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  )
)

t1 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, cox_df)
t2 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson, cox_df)
t3 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + fe.reducida.severa, cox_df)
t4 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cox_df)
t5 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*edad_ing1, cox_df)
t6 <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*charlson, cox_df)

tprescribed_guia <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + prescribedtoguia_fechaalta, cox_df)
tadherenced <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + adherencedtoguia_fechaalta, cox_df)
tprescribed_drugs <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta,
  cox_df
)
tprescribed_drugs_novoic <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta + denovo_ic_paciente,
  cox_df
)

saveRDS(tprescribed_drugs, 'Cox_0td_4bs_3rx_mortoicc.rds')
saveRDS(tprescribed_drugs_novoic, 'Cox_0td_5bs_3rx_mortoicc.rds')
# Cox multivariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------
# choose patients
cox_df <- get_cox_data(
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  )
)

coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*charlson, cluster = id, cox_df)


# test different AUC calculations -----------------------------------------
# Se tiene que ejecutar primero tprescribed_drugs
library(survAUC)
library(dynpred)
Surv
tp <- predict(tprescribed_drugs)
tp2 <- predict(tprescribed_drugs)

cindex(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta,
       cox_df)
AUCt <- dynpred::AUC(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta,
                     cox_df)

AUC_CD <- AUC.cd(
  Surv(cox_df$time_to_event, cox_df$event),
  Surv(cox_df$time_to_event, cox_df$event),
  tp,
  tp2,
  seq(5, 365, 5)
)
AUC_CD
