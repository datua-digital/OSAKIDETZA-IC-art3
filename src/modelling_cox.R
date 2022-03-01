# load libraries and sources----------------------------------------------------------
library(JMbayes)
library(survival)
library(tidyverse)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))

# script variables----------------------------------------------------------
LONGVAR <- "cum_perc_adh_guia_arm"
VARIABLESCOX_IND <- c("sexo", "edad_ing1", "charlson", "fe.reducida.severa")
VARIABLESCOX <- c("sexo", "edad_ing1")
VARIABLESLONGS <- c("cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca", "cum_perc_adh_doctor", "cum_perc_adh_guia",
                    "cum_perc_adh_guia_arm")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event", "time_to_event", "month")


# Cox univariante: Subset de todos los pacientes -------------------------------------------

df_jm <- readr::read_csv("src/data/out/df_JM.csv")
# choose patients
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

df_jm$sexo <- as.factor(df_jm$sexo)
df_jm$id <- as.factor(df_jm$id)
df_jm$event <- as.numeric(df_jm$event)

df_jm <- filter_patients(df_jm, patients_conditions)
df_jm <- preprocess_dfjm(df_jm)
cox_df <- df_jm[!duplicated(df_jm$id), ]


coxph(Surv(time_to_event, event) ~ sexo, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ fe.reducida.severa, cluster = id, cox_df)

# Cox univariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------
df_jm <- readr::read_csv("data/out/df_JM.csv")
# choose patients
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)

df_jm$sexo <- as.factor(df_jm$sexo)
df_jm$id <- as.factor(df_jm$id)
df_jm$event <- as.numeric(df_jm$event)

df_jm <- filter_patients(df_jm, patients_conditions)
df_jm <- preprocess_dfjm(df_jm)
cox_df <- df_jm[!duplicated(df_jm$id), ]

coxph(Surv(time_to_event, event) ~ sexo, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ fe.reducida.severa, cluster = id, cox_df)

# Cox multivariante: Subset de todos los pacientes -------------------------------------------
df_jm <- readr::read_csv("data/out/df_JM.csv")
# choose patients
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)

df_jm$sexo <- as.factor(df_jm$sexo)
df_jm$id <- as.factor(df_jm$id)
df_jm$event <- as.numeric(df_jm$event)

df_jm <- filter_patients(df_jm, patients_conditions)
df_jm <- preprocess_dfjm(df_jm)
cox_df <- df_jm[!duplicated(df_jm$id), ]


coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*charlson, cluster = id, cox_df)


# Cox multivariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------
df_jm <- readr::read_csv("data/out/df_JM.csv")
# choose patients
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = NULL
)

df_jm$sexo <- as.factor(df_jm$sexo)
df_jm$id <- as.factor(df_jm$id)
df_jm$event <- as.numeric(df_jm$event)

df_jm <- filter_patients(df_jm, patients_conditions)
df_jm <- preprocess_dfjm(df_jm)
cox_df <- df_jm[!duplicated(df_jm$id), ]

coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*charlson, cluster = id, cox_df)

