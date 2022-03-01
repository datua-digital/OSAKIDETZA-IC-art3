library(JMbayes)
library(tidyverse)
library(nlme)
library(splines)

source("src/utils/jm_utils.R")
source("src/utils/table_utils.R")

DATAOUTPATH <- "data/out/"

VARIABLESCOX_IND <- c("sexo", "edad_ing1", "charlson", "fe.reducida.severa")
VARIABLESCOX <- c("sexo", "edad_ing1")
VARIABLESLONGS <- c("cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca",
                    "cum_perc_adh_doctor", "cum_perc_adh_guia", "cum_perc_adh_ara2oieca",
                    "cum_perc_adh_guia_arm")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event", "time_to_event", "month")
### all results and values for the report
LONGVAR <- "cum_perc_adh_guia_arm"
# Univariante; cum_perc_adh_guia_arm; all patient; sexo, edad; value; slope-value; slope -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)

# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3, cox_vars = c("sexo", "edad_ing1"))
JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 

saveRDS(JM_table, file = 'out/informe/JMtable_all_sexoedad.rds')
rm("M1", "M2", "M3")
# Univariante; cum_perc_adh_guia_arm; all patient; sexo, edad, charlson -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)
M1$DIC
# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3, 
                          cox_vars = c("sexo", "edad_ing1", "charlson"))
JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 
saveRDS(JM_table, file = 'out/informe/JMtable_all_sexoedadcharlson.rds')
rm("M1", "M2", "M3")

# Univariante; cum_perc_adh_guia_arm; all patient; sexo, edad, charlson, fe reducida -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson + fe.reducida.severa, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
# auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)

# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
# auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
# auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3,
                          cox_vars = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"))
# JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 
saveRDS(JM_table, file = 'out/informe/JMtable_all_sexoedadcharlsonfereducida.rds')
rm("M1", "M2", "M3")

# Univariante; cum_perc_adh_guia_arm; de Novo + no30dayscensured; sexo, edad; value; slope-value; slope -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
# auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)

# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
# auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
# auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3, cox_vars = c("sexo", "edad_ing1"))
# JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 
JM_table$summaryJM
saveRDS(JM_table, file = 'out/informe/JMtable_DenovoNo30days_sexoedad.rds')
rm("M1", "M2", "M3")
# Univariante; cum_perc_adh_guia_arm; de Novo + no30dayscensured; sexo, edad, charlson -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
# auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)
# M1$DIC
# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
# auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
# auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3, 
                          cox_vars = c("sexo", "edad_ing1", "charlson"))
# JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 
saveRDS(JM_table, file = 'out/informe/JMtable_DenovoNo30days_sexoedadcharlson.rds')
rm("M1", "M2", "M3")

# Univariante; cum_perc_adh_guia_arm; de Novo + no30dayscensured; sexo, edad, charlson, fe reducida -------------------------------------------------
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

lmeFit <- lme(cum_perc_adh_guia_arm ~ ns(month, 4), 
              random = ~ ns(month, 4) | id,
              data = df_jm,
              control = lmeControl(opt = 'optim'))

survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson + fe.reducida.severa, data = cox_df, x = TRUE, model = TRUE)
M1 <- jointModelBayes(lmeFit, survFit, timeVar = "month")
# auc1 <- JMbayes::aucJM(M1, df_jm, Tstart = 6, Thoriz = 12)
M1$DIC
# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = "td-both", extraForm = dForm)
# auc2 <- JMbayes::aucJM(M2, df_jm, Tstart = 6, Thoriz = 12)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = "td-extra", extraForm = dForm)
# auc3 <- JMbayes::aucJM(M3, df_jm, Tstart = 6, Thoriz = 12)

JM_table <- summary_table(m1 = M1, m2 = M2, m3 = M3,
                          cox_vars = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"))
# JM_table$auc <- c(m1 = auc1$auc, m2 = auc2$auc, m3 = auc3$auc) 
saveRDS(JM_table, file = 'out/informe/JMtable_DenovoNo30days_sexoedadcharlsonfereducida.rds')
rm("M1", "M2", "M3")













# multivariate, 2 variables; cum_perc_adh_ara2oieca y cum_perc_adh_bbloqM; de Novo + no30dayscensured; sexo, edad, charlson, fe reducida   ------------------------------------------------------------

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

# M1
set.seed(1000)
lmeFit <- mvglmer(
  formulas = list(
    cum_perc_adh_ara2oieca ~ ns(month, 4) + (ns(month, 4) | id),
    cum_perc_adh_bbloq ~ ns(month, 4) + (ns(month, 4) | id)
  ),
  data = df_jm,
  families = list(gaussian, gaussian))


survFit <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson + fe.reducida.severa, data = cox_df, x = TRUE, model = TRUE)

M1 <- mvJointModelBayes(lmeFit, survFit, timeVar = "month")
saveRDS(JM_table, file = 'out/informe/mv2_2variables_sexoedadcharlsonfereducida.rds')


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
saveRDS(M2, paste0(OUTPATH, output, "_M2", ".rds"))


# M3
forms <- list(
  "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                   indFixed = 2:5, indRandom = 2:5),
  "cum_perc_adh_bbloq"    = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                  indFixed = 2:5, indRandom = 2:5 )
)
M3 <- update(M1, Formulas = forms)
saveRDS(M3, paste0(OUTPATH, output, "_M3", ".rds"))

