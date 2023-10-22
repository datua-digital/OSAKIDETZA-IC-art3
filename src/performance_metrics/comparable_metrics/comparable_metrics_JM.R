library(DescTools)
library(caret)

# Calculation functions ---------------------------------------------------

auc_casero <- function(df_model, model, Tstart, Thorizon) {
  # browser()
  if (is.null(df_model)) {
    df_model <- model$Data$data
  }
  
  df_predictions <- get_df_prediciones_jm(df_model, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_jm(df = df_predictions, model, Thorizon)
  
  auc <- calcular_auc_(id_scores_actuals$probability, id_scores_actuals$actuals)
  
  return(auc)
}

precision_casero <- function(df_model, model, Tstart, Thorizon) {
  
  df_predictions <- get_df_prediciones_jm(df_model, Tstart)
  id_scores_actuals <- get_id_scores_actuals_jm(df = df_predictions, model, Thorizon)
  # print(id_scores_actuals)
  brier_score <- BrierScore(id_scores_actuals$actuals, id_scores_actuals$probability)
  return(brier_score)
}

get_confusion_matrix <- function(df_model, model, Tstart, Thorizon) {
  
  if (is.null(df_model)) {
    df_model <- model$Data$data
  }
  
  df_predictions <- get_df_prediciones_jm(df_model, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_jm(df = df_predictions, model, Thorizon)
  
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



get_df_prediciones_jm <- function(df_model, Tstart) {
  ids_cumplen_condicion_Tstart <- unique(df_model[(df_model$month > Tstart), 'id']) # $id erabili det momentu batzutan
  df_predictions <- df_model[(df_model$id %in% ids_cumplen_condicion_Tstart) & (df_model$month <= Tstart), ]
  return(df_predictions)
}


get_id_scores_actuals_jm <- function(df, model, Thorizon) {
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
  return(list(probability = scores, actuals = actuals))
}


# tests -------------------------------------------------------------------

run_test_comparable_metrics_JM <- function(){
  auc1_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 1, Thorizon = 12)
  auc2_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 2, Thorizon = 12)
  auc3_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 3, Thorizon = 12)
  auc4_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 4, Thorizon = 12)
  auc5_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 5, Thorizon = 12)
  auc6_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 6, Thorizon = 12)
  auc7_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 7, Thorizon = 12)
  auc8_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 8, Thorizon = 12)
  auc9_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 9, Thorizon = 12)
  auc10_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 10, Thorizon = 12)
  auc11_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 11, Thorizon = 12)
  
  auc_jm <- c(
    auc1_12, auc2_12, auc3_12, auc4_12,
    auc5_12, auc6_12, auc7_12, auc8_12,
    auc9_12, auc10_12, auc11_12
  )
  
  auc1_2 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 1, Thorizon = 2)
  auc2_3 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 2, Thorizon = 3)
  auc3_4 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 3, Thorizon = 4)
  auc4_5 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 4, Thorizon = 5)
  auc5_6 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 5, Thorizon = 6)
  auc6_7 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 6, Thorizon = 7)
  auc7_8 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 7, Thorizon = 8)
  auc8_9 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 8, Thorizon = 9)
  auc9_10 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 9, Thorizon = 10)
  auc10_11 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 10, Thorizon = 11)
  auc11_12 <- auc_casero(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 11, Thorizon = 12)
  auc_jm2 <- c(
    auc1_2, auc2_3, auc3_4, auc4_5,
    auc5_6, auc6_7, auc7_8, auc8_9,
    auc9_10, auc10_11, auc11_12
  )
}
