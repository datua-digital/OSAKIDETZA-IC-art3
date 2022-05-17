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

get_cox_data <- function(df_jm, patients_conditions) {
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(df_jm, variables_jm = colnames(df_jm))
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  return(cox_df)
}

anyadir_edad_categorizada <- function(df) {
  df$edadcat <- NA
  df$edadcat[df$edad_ing1 < 55] <- "40-54"
  df$edadcat[df$edad_ing1 >= 55 & df$edad_ing1 < 65] <- "55-64"
  df$edadcat[df$edad_ing1 >= 65 & df$edad_ing1 < 75] <- "65-74"
  df$edadcat[df$edad_ing1 >= 75 & df$edad_ing1 < 85] <- "75-84"
  df$edadcat[df$edad_ing1 >= 85] <- "85+"
  return(df)
}

anyadir_charlson_categorizado <- function(df) {
  df$charlsoncat <- NA
  df$charlsoncat[df$charlson == 1] <- "1"
  df$charlsoncat[df$charlson == 2] <- "2"
  df$charlsoncat[df$charlson == 3] <- "3"
  df$charlsoncat[df$charlson == 4] <- "4"
  df$charlsoncat[df$charlson >= 5] <- "5+"
  return(df)
}

# Cox univariante: Subset de todos los pacientes -------------------------------------------

# choose patients

cox_df <- get_cox_data(
  df_jm,
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
  df_jm,
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
  df_jm,
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

cox_df$pieca <- as.numeric(cox_df$prescribediecaara2_fechaalta)
cox_df$pbb <- as.numeric(cox_df$bbloq_prescribed_fechaalta)
cox_df$parm <- as.numeric(cox_df$arm_prescribed_fechaalta)
cox_df$ptot <- paste0(cox_df$pbb, cox_df$pieca, cox_df$parm)
tprescribed_drugs_denovo_interac <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, cox_df)

# Cox multivariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------
# choose patients
cox_df <- get_cox_data(
  df_jm,
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

# Variables continuas Vs categóricas --------------------------------------
cox_df <- get_cox_data(
  df_jm,
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  )
)

tprescribed_drugs_novoic_continua <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta + denovo_ic_paciente,
  cox_df
)
summary(tprescribed_drugs_novoic_continua)


table(cox_df$edadcat)
hist(cox_df$edad_ing1)
cox_df$charlsoncat[cox_df$charlson == 1] <- "1"
cox_df <- anyadir_edad_categorizada(cox_df)
tprescribed_drugs_novoic_edadcat <- coxph(
  Surv(time_to_event, event) ~ sexo + edadcat  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta + denovo_ic_paciente,
  cox_df
)
summary(tprescribed_drugs_novoic_edadcat)

cox_df <- anyadir_charlson_categorizado(cox_df)
tprescribed_drugs_novoic_charlsoncat <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlsoncat + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta + denovo_ic_paciente,
  cox_df
)
summary(tprescribed_drugs_novoic_charlsoncat)

tprescribed_drugs_novoic_charlsoedadcat <- coxph(
  Surv(time_to_event, event) ~ sexo + edadcat  + charlsoncat + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta + denovo_ic_paciente,
  cox_df
)
summary(tprescribed_drugs_novoic_charlsoedadcat)
