# load libraries and sources----------------------------------------------------------
library(JMbayes)
library(survival)
library(tidyverse)
library(rms)
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

tprescribed_drugs_denovo_interac <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, cox_df)
summary(tprescribed_drugs_denovo_interac)


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

# Selección de variables en Cox -------------------------------------------

library(glmnet)
library(survival)

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
surv_object_variables <- c("time_to_event", "event")

# En caso de que entren en los modelos LASSO, valorar incluir estos calculos en JM.csv.
# hipertension
cox_df$hta <- "Sin diagnostico"
cox_df$hta[cox_df$HTN == "HTN" | cox_df$HtnComplicn == "Htn-Complicn"] <- "hta"
# diabetes
cox_df$dm <- "Sin diagnostico"
cox_df$dm[cox_df$DiabeteSin == "Diabete-Sin" | cox_df$DiabetesCon == "Diabetes-Con"] <- "dm"

cox_df$pieca <- as.numeric(cox_df$prescribediecaara2_fechaalta)
cox_df$pbb <- as.numeric(cox_df$bbloq_prescribed_fechaalta)
cox_df$parm <- as.numeric(cox_df$arm_prescribed_fechaalta)
cox_df$ptot <- paste0(cox_df$pbb, cox_df$pieca, cox_df$parm)

initial_variables <- c(
  "sexo", "fe.reducida.severa", "charlson", "edad_ing1", "denovo_ic_paciente", 
  "obesidad", "Infarto", "Coronariopatia", "ConduccCardiaco",
  "Arritmia", "all_cerebrovasc", "Arterosclerosis", "all_neoplasia",
  "EPOC", "Asma", "InsufRenal.C", "Tiroides", "DeficNutri", "Hyperlipidem",
  "all_anemia", "MoodDisorders", "Valvulopatia", "DiabetesCon",
  "HtnComplicn", "digital", "estat", "diu", "ivab", "hta", "dm", "ptot"
)
cox_df$event <- as.numeric(cox_df$event)
y <- as.matrix(cox_df[, surv_object_variables])
colnames(y) <- c('time', 'status')

x <- cox_df[, initial_variables]

fit <- glmnet(x, y, family = "cox", alpha = 1)

plot(fit)
plot(fit, xvar = "lambda", label = TRUE)

# selección de variables en función de lambda. A medida que lambda aumenta, las variables que van a 0 decrecen.
coef(fit, s = 0.01)
coef(fit, s = 0.025)
coef(fit, s = 0.05)

# Validación cruzada con Harrels C-index y 
model_matrix <- model.matrix(y~., data = cbind(x, y))
x <- model_matrix[, c(-1, -(ncol(model_matrix) - 1), -ncol(model_matrix))]
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C", alpha = 1)

plot(cvfit)
C_coefs <- coef(fit, s = cvfit$lambda.1se)

cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "deviance", alpha = 1)
cvfit
plot(cvfit)
pld_coefs <- coef(fit, s = cvfit$lambda.1se)


# vamos a analizar el resultado con coxph, se mantiene la variable sexo:
# Tras la selección con partial-likelihood (deviance)
cox_autoselected_deviance <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + denovo_ic_paciente + InsufRenal.C + Valvulopatia + ptot + Coronariopatia,
  cox_df
)
summary(cox_autoselected_deviance)

# Tras la selección con Harrel's C-index:
cox_autoselected_harrelsc <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + denovo_ic_paciente + InsufRenal.C + Valvulopatia + ptot,
  cox_df
)
summary(cox_autoselected_harrelsc)


# Cox cross validation ----------------------------------------------------


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

# se tiene que utilizar la función cph del paquete rms. Se ha comprobado que es equivalente a coxph.

tprescribed_drugs_denovo_interac <- rms::cph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, 
  cox_df, 
  x = TRUE, 
  y = TRUE
)

# cross validation
validation_matrix <- rms::validate(tprescribed_drugs_denovo_interac, method = 'crossvalidation', B = 10, dxy = TRUE)
validation_matrix
cindex_train <- validation_matrix[1,2] / 2 + 0.5
cindex_test <- validation_matrix[1, 3] / 2 + 0.5
# bootstrap
validation_matrix <- rms::validate(tprescribed_drugs_denovo_interac, B = 150, dxy = TRUE)
validation_matrix
cindex_train <- validation_matrix[1,2] / 2 + 0.5
cindex_test <- validation_matrix[1, 3] / 2 + 0.5
