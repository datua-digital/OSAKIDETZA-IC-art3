
# Calculation functions ---------------------------------------------------

auc_casero <- function(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 3, Thorizon = 12) {
  
  df_predictions <- get_df_prediciones_(model$Data$data, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_(df_predictions, model, Thorizon)
  
  auc <- calcular_auc_(id_scores_actuals$scores, id_scores_actuals$actuals)
  
  return(auc)
}


get_df_prediciones_ <- function(df_model, Tstart) {
  ids_cumplen_condicion_Tstart <- unique(df_model[(df_model$month > Tstart), 'id'])$id
  df_predictions <- df_model[(df_model$id %in% ids_cumplen_condicion_Tstart) & (df_model$month <= Tstart), ]
  return(df_predictions)
}


get_id_scores_actuals_ <- function(df, model, Thorizon) {
  scores <- c()
  actuals <- c()
  ids <- as.character(unique(df$id))
  
  for (id in ids) {
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


# tests -------------------------------------------------------------------


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

auc_jm <- c(0.7028645, 0.6990075, 0.6961159, 0.6916279, 0.6845271, 0.6906954, 0.6901310, 0.6778196, 0.6576343, 0.6808355, 0.6405302)

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
auc_jm2 <- c(auc1_, auc2_, auc3_, auc4_, auc5_, auc6_, auc7_, auc8_, auc9_, auc10_, auc11_)


# df_predictions_i <- df_predictions[df_predictions$id == df_predictions$id[1], ]
# score_array_i <- survfitJM(model, newdata = df_predictions_i, idVar = 'id', survTimes = c(Thorizon))
# predict(model, newdata = df_predictions, type = 'risk', times = Thorizon)[1:4]
# predict(model, newdata = df_predictions, type = 'survival', times = Thorizon)[1:4]
# predict(model, newdata = df_predictions, type = 'lp', times = Thorizon)[1:4]
# predict(model, newdata = df_predictions, type = 'expected', times = Thorizon)[1:4]
# tprescribed_drugs_denovo_interac$residuals ^ 2
# df_predictions
# df_predictions_i[c('event', 'time_to_event', 'sexo', 'edad_ing1', 'charlson', 'fe.reducida.severa', 'denovo_ic_paciente', 'ptot')]
# residuals(tprescribed_drugs_denovo_interac, type = 'martingale')