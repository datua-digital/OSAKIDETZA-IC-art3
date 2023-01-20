library(JMbayes)
library(survival)
library(tidyverse)
library(rms)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))


df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))
get_cox_data <- function(df_jm, patients_conditions) {
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(df_jm, variables_jm = colnames(df_jm))
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  return(cox_df)
}

auc_casero_cox <- function(df_model, model, Tstart = 3, Thorizon = 12) {
  
  df_predictions <- get_df_prediciones_cox_(df_model, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_cox_(df_predictions, model, Thorizon)
  # print(id_scores_actuals)
  auc <- calcular_auc_(id_scores_actuals$scores, id_scores_actuals$actuals)
  
  return(auc)
}


get_df_prediciones_cox_ <- function(df_model, Tstart) {
  return(df_model[(df_model$time_to_event > Tstart), ])
}


get_id_scores_actuals_cox_ <- function(df, model, Thorizon) {
  scores <- -predict(model, newdata = df, type = 'risk', times = Thorizon)
  actuals <- c(df$time_to_event > Thorizon)
  return(list(scores = scores, actuals = actuals))
}


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

# Cox with our selection:
# sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot
tprescribed_drugs_denovo_interac <- coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, cox_df, x = TRUE)
cox_df$time_to_event
library(SurvMetrics)
Brier(tprescribed_drugs_denovo_interac, cox_df, t_star = cox_df$time_to_event)
#calculate the Brier Score
med_index = median(1:length(dis_time))
surv_obj = Surv(test_data$time, test_data$status)
t_star = median(fitrsf$time.interest)

#Brier Score for Cox
metrics_cox[i] = Brier(surv_obj, pre_sp = mat_cox[, med_index], t_star)


runif(1)







summary(tprescribed_drugs_denovo_interac)
confint(tprescribed_drugs_denovo_interac) # intervalos de confianza

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



# Cox with lasso selection
# sexo + edad_ing1  + charlson + denovo_ic_paciente + InsufRenal.C + Valvulopatia + ptot
cox_autoselected_harrelsc <- coxph(
  Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + denovo_ic_paciente + InsufRenal.C + Valvulopatia + ptot,
  cox_df
)
summary(cox_autoselected_harrelsc)
confint(cox_autoselected_harrelsc)

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


# Metrics for M1 model
auc_casero <- function(model = readRDS(paste(OUTPATH, 'JM_3td_5bs_1rx_mortoicc_M1_.rds', sep = '/')), Tstart = 3, Thorizon = 12, df_jm = NULL) {
  if (is.null(df_jm)) {
    df_jm <- model$Data$data
  }
  
  df_predictions <- get_df_prediciones_(df_jm, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_(df = df_predictions, model, Thorizon)
  
  auc <- calcular_auc_(id_scores_actuals$scores, id_scores_actuals$actuals)
  
  return(auc)
}


get_df_prediciones_ <- function(df_model, Tstart) {
  ids_cumplen_condicion_Tstart <- unique(df_model[(df_model$month > Tstart), 'id']) # $id erabili det momentu batzutan
  df_predictions <- df_model[(df_model$id %in% ids_cumplen_condicion_Tstart) & (df_model$month <= Tstart), ]
  return(df_predictions)
}


get_id_scores_actuals_ <- function(df, model, Thorizon) {
  scores <- c()
  actuals <- c()
  ids <- as.character(unique(df$id))
  contador <- 1
  for (id in ids) {
    print(contador / length(ids))
    contador = contador + 1
    df_predictions_i <- df[df$id == id, ]
    score_array_i <- survfitJM(model, newdata = df_predictions_i, idVar = 'id', survTimes = c(Thorizon))
    scores <- c(scores, score_array_i$summaries[[1]][2])
    actuals <- c(actuals, unique(df_predictions_i$time_to_event) > Thorizon)
  }
  return(list(scores = scores, actuals = actuals))
}


calcular_auc_ <- function(scores, actuals, plot_roc = TRUE) {
  actuals <- actuals[order(scores)]
  sens <- (sum(actuals) - cumsum(actuals))/sum(actuals)
  spec <- cumsum(!actuals) / sum(!actuals)
  auc <- sum(spec * diff(c(0, 1 - sens)))
  
  if (plot_roc) {
    plot(1 - spec, sens, type = "l", col = "red", 
         ylab = "Sensitivity", xlab = "1 - Specificity")
    abline(c(0,0),c(1,1))
  }
  
  return(auc)
}


# build and preprocess data
variables_ids_eventos <- c("id", "event", "time_to_event", "month")
variables_longitudinales <- c("cum_perc_adh_ara2oieca", "cum_perc_adh_bbloq", "cum_perc_adh_arm")
df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))
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

# performance metrics for M1
M1 <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M1_.rds', sep = '/'))
summary(M1)

# auc
auc1_12 <- auc_casero(model = M1, Tstart = 1, Thorizon = 12, df_jm = df_jm)
auc2_12 <- auc_casero(model = M1, Tstart = 2, Thorizon = 12, df_jm = df_jm)
auc3_12 <- auc_casero(model = M1, Tstart = 3, Thorizon = 12, df_jm = df_jm)
auc4_12 <- auc_casero(model = M1, Tstart = 4, Thorizon = 12, df_jm = df_jm)
auc5_12 <- auc_casero(model = M1, Tstart = 5, Thorizon = 12, df_jm = df_jm)
auc6_12 <- auc_casero(model = M1, Tstart = 6, Thorizon = 12, df_jm = df_jm)
auc7_12 <- auc_casero(model = M1, Tstart = 7, Thorizon = 12, df_jm = df_jm)
auc8_12 <- auc_casero(model = M1, Tstart = 8, Thorizon = 12, df_jm = df_jm)
auc9_12 <- auc_casero(model = M1, Tstart = 9, Thorizon = 12, df_jm = df_jm)
auc10_12 <- auc_casero(model = M1, Tstart = 10, Thorizon = 12, df_jm = df_jm)
auc11_12 <- auc_casero(model = M1, Tstart = 11, Thorizon = 12, df_jm = df_jm)

auc_jm <- c(auc1_12, auc2_12, auc3_12, auc4_12, auc5_12, auc6_12, auc7_12, auc8_12, auc9_12, auc10_12, auc11_12)

auc1_2 <- auc_casero(model = M1, Tstart = 1, Thorizon = 2, df_jm)
auc2_3 <- auc_casero(model = M1, Tstart = 2, Thorizon = 3, df_jm)
auc3_4 <- auc_casero(model = M1, Tstart = 3, Thorizon = 4, df_jm)
auc4_5 <- auc_casero(model = M1, Tstart = 4, Thorizon = 5, df_jm)
auc5_6 <- auc_casero(model = M1, Tstart = 5, Thorizon = 6, df_jm)
auc6_7 <- auc_casero(model = M1, Tstart = 6, Thorizon = 7, df_jm)
auc7_8 <- auc_casero(model = M1, Tstart = 7, Thorizon = 8, df_jm)
auc8_9 <- auc_casero(model = M1, Tstart = 8, Thorizon = 9, df_jm)
auc9_10 <- auc_casero(model = M1, Tstart = 9, Thorizon = 10, df_jm)
auc10_11 <- auc_casero(model = M1, Tstart = 10, Thorizon = 11, df_jm)
auc11_12 <- auc_casero(model = M1, Tstart = 11, Thorizon = 12, df_jm)
auc_jm2 <- c(auc1_2, auc2_3, auc3_4, auc4_5, auc5_6, auc6_7, auc7_8, auc8_9, auc9_10, auc10_11, auc11_12)

# prediction error
pe_1_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 1, Thoriz = 12)
pe_2_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 2, Thoriz = 12)
pe_3_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 3, Thoriz = 12)
pe_4_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 4, Thoriz = 12)
pe_5_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 5, Thoriz = 12)
pe_6_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 6, Thoriz = 12)
pe_7_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 7, Thoriz = 12)
pe_8_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 8, Thoriz = 12)
pe_9_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 9, Thoriz = 12)
pe_10_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 10, Thoriz = 12)
pe_11_12 <- JMbayes::prederrJM(M1, df_jm, Tstart = 11, Thoriz = 12)


pe <- c(pe_1_12$prederr, pe_2_12$prederr, pe_3_12$prederr, pe_4_12$prederr, pe_5_12$prederr, pe_6_12$prederr, 
        pe_7_12$prederr, pe_8_12$prederr, pe_9_12$prederr, pe_10_12$prederr, pe_11_12$prederr)

# performance metrics for M2

M2 <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M2_.rds', sep = '/'))
summary(M2)

# auc
auc1_12_ <- auc_casero(model = M2, Tstart = 1, Thorizon = 12, df_jm = df_jm)
auc2_12_ <- auc_casero(model = M2, Tstart = 2, Thorizon = 12, df_jm = df_jm)
auc3_12_ <- auc_casero(model = M2, Tstart = 3, Thorizon = 12, df_jm = df_jm)
auc4_12_ <- auc_casero(model = M2, Tstart = 4, Thorizon = 12, df_jm = df_jm)
auc5_12_ <- auc_casero(model = M2, Tstart = 5, Thorizon = 12, df_jm = df_jm)
auc6_12_ <- auc_casero(model = M2, Tstart = 6, Thorizon = 12, df_jm = df_jm)
auc7_12_ <- auc_casero(model = M2, Tstart = 7, Thorizon = 12, df_jm = df_jm)
auc8_12_ <- auc_casero(model = M2, Tstart = 8, Thorizon = 12, df_jm = df_jm)
auc9_12_ <- auc_casero(model = M2, Tstart = 9, Thorizon = 12, df_jm = df_jm)
auc10_12_ <- auc_casero(model = M2, Tstart = 10, Thorizon = 12, df_jm = df_jm)
auc11_12_ <- auc_casero(model = M2, Tstart = 11, Thorizon = 12, df_jm = df_jm)

auc_jm_ <- c(auc1_12_, auc2_12_, auc3_12_, auc4_12_, auc5_12_, auc6_12_, auc7_12_, auc8_12_, auc9_12_, auc10_12_, auc11_12_)

auc1_2_ <- auc_casero(model = M2, Tstart = 1, Thorizon = 2, df_jm)
auc2_3_ <- auc_casero(model = M2, Tstart = 2, Thorizon = 3, df_jm)
auc3_4_ <- auc_casero(model = M2, Tstart = 3, Thorizon = 4, df_jm)
auc4_5_ <- auc_casero(model = M2, Tstart = 4, Thorizon = 5, df_jm)
auc5_6_ <- auc_casero(model = M2, Tstart = 5, Thorizon = 6, df_jm)
auc6_7_ <- auc_casero(model = M2, Tstart = 6, Thorizon = 7, df_jm)
auc7_8_ <- auc_casero(model = M2, Tstart = 7, Thorizon = 8, df_jm)
auc8_9_ <- auc_casero(model = M2, Tstart = 8, Thorizon = 9, df_jm)
auc9_10_ <- auc_casero(model = M2, Tstart = 9, Thorizon = 10, df_jm)
auc10_11_ <- auc_casero(model = M2, Tstart = 10, Thorizon = 11, df_jm)
auc11_12_ <- auc_casero(model = M2, Tstart = 11, Thorizon = 12, df_jm)
auc_jm2_ <- c(auc1_2_, auc2_3_, auc3_4_, auc4_5_, auc5_6_, auc6_7_, auc7_8_, auc8_9_, auc9_10_, auc10_11_, auc11_12_)

# prediction error
pe_1_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 1, Thoriz = 12)
pe_2_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 2, Thoriz = 12)
pe_3_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 3, Thoriz = 12)
pe_4_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 4, Thoriz = 12)
pe_5_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 5, Thoriz = 12)
pe_6_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 6, Thoriz = 12)
pe_7_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 7, Thoriz = 12)
pe_8_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 8, Thoriz = 12)
pe_9_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 9, Thoriz = 12)
pe_10_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 10, Thoriz = 12)
pe_11_12_ <- JMbayes::prederrJM(M2, df_jm, Tstart = 11, Thoriz = 12)

pe_ <- c(pe_1_12_$prederr, pe_2_12_$prederr, pe_3_12_$prederr, pe_4_12_$prederr, pe_5_12_$prederr, pe_6_12_$prederr, 
        pe_7_12_$prederr, pe_8_12_$prederr, pe_9_12_$prederr, pe_10_12_$prederr, pe_11_12_$prederr)
