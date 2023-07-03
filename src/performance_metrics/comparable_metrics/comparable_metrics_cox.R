library(DescTools)
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")


# Function calculation ----------------------------------------------------

auc_casero_cox <- function(df_model, model, Tstart = 3, Thorizon = 12) {
  
  df_predictions <- get_df_predicciones_cox_(df_model, Tstart)
  browser()
  id_scores_actuals <- get_id_scores_actuals_cox_(df_predictions, model, Thorizon)
  
  auc <- calcular_auc_(id_scores_actuals$scores, id_scores_actuals$actuals)
  
  return(auc)
}


precision_casero_cox <- function(df_model, model, Tstart = 3, Thorizon = 12) {
  
  df_predictions <- get_df_predicciones_cox_(df_model, Tstart)
  id_scores_actuals <- get_id_scores_actuals_cox_(df_predictions, model, Thorizon)
  # print(id_scores_actuals)
  brier_score <- DescTools::BrierScore(id_scores_actuals$actuals, id_scores_actuals$probability)
  return(brier_score)
}

get_confusion_matrix_cox <- function(df_model, model, Tstart, Thorizon) {
  
  df_predictions <- get_df_predicciones_cox_(df_model, Tstart)
  id_scores_actuals <- get_id_scores_actuals_cox_(df_predictions, model, Thorizon)
  
  # invertir probabilidad y el positivo (de aqui adelante: IC o fallecer)
  id_scores_actuals$probability <- 1 - id_scores_actuals$probability
  id_scores_actuals$actuals <- id_scores_actuals$actuals == FALSE
  
  threshold <- 0.4
  confusion_matrix <- confusionMatrix(
    factor(id_scores_actuals$probability >= threshold), 
    factor(id_scores_actuals$actuals)
  )
  
  return(confusion_matrix)
}

get_df_predicciones_cox_ <- function(df_model, Tstart) {
  return(df_model[(df_model$time_to_event > Tstart), ])
}


get_id_scores_actuals_cox_ <- function(df, model, Thorizon) {
  scores <- -predict(model, newdata = df, type = 'risk', times = Thorizon)
  expected <- predict(model, newdata = df, type = 'expected', times = Thorizon)
  probability <- exp(-expected)
  actuals <- c(df$time_to_event > Thorizon)
  return(list(scores = scores, actuals = actuals, probability = probability))
}


# tests -------------------------------------------------------------------
run_tests_comparable_metrics_cox <- function(){
  
  cox_df <- get_cox_data(
    df_jm = readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds")),
    patients_conditions = list(
      denovo_ic_paciente = NULL,
      denovo_tt_paciente_fing = NULL,
      denovo_tt_paciente_falta = NULL,
      early_death_patient_30 = NULL,
      patient_with_prescription = NULL
    )
  )
  
  tprescribed_drugs_denovo_interac <- coxph(
    Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + denovo_ic_paciente + ptot, 
    cox_df, 
    x = TRUE, 
    y = TRUE
  )
  
  
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
}
