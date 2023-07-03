# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

# load libraries and sources----------------------------------------------------------
library(data.table)
library(ggplot2)
library(JMbayes)
library(splines)
library(tidyverse)
library(gridExtra)
library(tidyr)
library(hrbrthemes)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))

# Logic----------------------------------------------------------

M2 <- readRDS(paste(OUTPATH, 'example.rds', sep = '/'))
M1_multivariant <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M1_.rds', sep = '/'))
M2_multivariant <- readRDS(paste(OUTPATH, 'mv', 'JM_3td_5bs_1rx_mortoicc_M2_.rds', sep = '/'))

summary(M1_multivariant)
summary(M2_multivariant)

colnames(M1_multivariant$model_info$mvglmer_components$data)
# functions ---------------------------------------------------------------

read_prepare <- function(name, DATAINPATH){
  ndata <- read.csv(paste(DATAINPATH, name, sep = '/'))
  ndata$id <- factor(ndata$id)
  ndata$sexo <- factor(ndata$sexo, levels = c('Mujer', 'Hombre'))
  return(ndata)
}

read_prepare_multivariant <- function(name, DATAINPATH){
  ndata <- read.csv(paste(DATAINPATH, name, sep = '/'))
  ndata$id <- factor(ndata$id)
  ndata$sexo <- factor(ndata$sexo, levels = c('Mujer', 'Hombre'))
  ndata$fe.reducida.severa <- factor(ndata$fe.reducida.severa, levels = c('Fe reducida', 'Fe reducida severa'))
  ndata$ptot <- factor(ndata$ptot, levels = c('000', '001', '010', '011', '100', '101', '110', '111'))
  return(ndata)
}


compare_pred_plots <- function(model, ndata, ndata2, name1=NULL, name2=NULL){
  probs1 <- survfitJM(model, newdata = ndata, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  probs2 <- survfitJM(model, newdata = ndata2, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  myJMsurvfit(probs1, estimator = "mean", include.y = TRUE, conf.int = TRUE,
              fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 12),
              xlab = 'Time [months]', ylab = 'Survival Probability',
              ylab2 = 'Cumulative months at time t', main = name1,
              ymin = 0, ylim.y = c(0, 12), col = 'black',
              cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
              cex.lab.z = 1.2, cex.axis.z = 1.05)
  
  myJMsurvfit(probs2, estimator = "mean", include.y = TRUE, conf.int = TRUE,
              fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 12),
              xlab = 'Time [months]', ylab = 'Survival Probability',
              ylab2 = 'Cumulative months at time t', main = name2,
              ymin = 0, ylim.y = c(0, 12), col = 'black',
              cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
              cex.lab.z = 1.2, cex.axis.z = 1.05)
}

compare_pred_plots_multivariant <- function(model, ndata, ndata2, name1=NULL, name2=NULL){
  probs1 <- survfitJM(model, newdata = ndata, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  probs2 <- survfitJM(model, newdata = ndata2, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  plot(probs1, surv_in_all = TRUE, 
       lty_lines_CI = 3, col_lines = "blue", col_fill_CI = "pink2", 
       main = name1, ylab = c(""),
       col_points = "red", pch_points = 16, 
       cex_xlab = 0.8, cex_ylab = 0.8, cex_zlab = 0.8, 
       cex_main = 0.8, cex_axis = 0.7)
  
  plot(probs2, surv_in_all = TRUE, 
       lty_lines_CI = 3, col_lines = "blue", col_fill_CI = "pink2", 
       main = name2, ylab = c(""),
       col_points = "red", pch_points = 16, 
       cex_xlab = 0.8, cex_ylab = 0.8, cex_zlab = 0.8, 
       cex_main = 0.8, cex_axis = 0.7)
}

pred_plots <- function(ndata, name_v = NULL, evol_window = c(2, 12)){
  evol_window_ <- c(NULL, NULL)
  evol_window_[1] <- max(evol_window[1], 2)
  evol_window_[2] <- max(nrow(ndata), evol_window[2])
  
  contador <- 1
  for (p in c(evol_window_[1]:evol_window_[2])) {
    ndata_test <- ndata[1:p, ]
    probs <- survfitJM(M2, newdata = ndata_test, idVar = 'id', survTimes = seq(1, 48, 1)/4)
    myJMsurvfit(probs, estimator = "mean", include.y = TRUE, conf.int = TRUE,
                fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 12),
                xlab = 'Time [months]', ylab = 'Survival Probability',
                ylab2 = 'Cumulative months at time t', main = name_v[contador],
                ymin = 0, ylim.y = c(0, 12), col = 'black',
                cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
                cex.lab.z = 1.2, cex.axis.z = 1.05)
    contador <- contador + 1
  }
}


# predictions example univariant model-----------------------------------------------------

# slope effect
ndata <- read_prepare('mockdata_slope_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_slope_id2.csv', DATAINPATH)
compare_pred_plots(M2, ndata, ndata2, name1 = "Patient - slope0", name2 = "Patient - slope1")

# value effect
ndata <- read_prepare('mockdata_value_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_value_id2.csv', DATAINPATH)
compare_pred_plots(M2, ndata, ndata2, name1 = "Patient - value6", name2 = "Patient - value1")

# age effect
ndata <- read_prepare('mockdata_edad_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_edad_id2.csv', DATAINPATH)
compare_pred_plots(M2, ndata, ndata2, name1 = "Patient - edad50", name2 = "Patient - edad89")

# sex effect
ndata <- read_prepare('mockdata_sexo_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_sexo_id2.csv', DATAINPATH)
compare_pred_plots(M2, ndata, ndata2, name1 = "Patient - sexoMujer", name2 = "Patient - sexoHombre")

# evolution

# slope
ndata <- read_prepare('mockdata_slope_id1.csv', DATAINPATH)
pred_plots(ndata, name_v = paste0('pred_in_obs', c(2:6)), evol_window = c(1, 6))

ndata <- read_prepare('mockdata_slope_id2.csv', DATAINPATH)
pred_plots(ndata, name_v = paste0('pred_in_obs', c(2:6)), evol_window = c(1, 6))

# value
ndata <- read_prepare('mockdata_value_id1.csv', DATAINPATH)
pred_plots(ndata, name_v = paste0('pred_in_obs', c(2:6)), evol_window = c(1, 6))

ndata <- read_prepare('mockdata_value_id2.csv', DATAINPATH)
pred_plots(ndata, name_v = paste0('pred_in_obs', c(2:6)), evol_window = c(1, 6))

# Shiny-----------------------------------------------------

# runDynPred()

# predictions example multivariant model-----------------------------------------------------

# age effect
ndata <- read_prepare_multivariant('mockdata_edad_id1_multivariant.csv', DATAINPATH)
ndata2 <- read_prepare_multivariant('mockdata_edad_id2_multivariant.csv', DATAINPATH)
# compare_pred_plots_multivariant(M1_multivariant, ndata, ndata2, name1 = "Patient - edad50", name2 = "Patient - edad89")

probs1 <- survfitJM(M1_multivariant, newdata = ndata, idVar = 'id', survTimes = seq(1, 48, 1)/4)
probs2 <- survfitJM(M1_multivariant, newdata = ndata2, idVar = 'id', survTimes = seq(1, 48, 1)/4)

prepare_multivariant_data <- function(probs) {
  predicted_risk_wide <- as.data.frame(probs$summaries$A) %>% select(times, Mean, Lower, Upper)
  predicted_risk_long <- gather(
    predicted_risk_wide,
    serie,
    measurement,
    Mean:Upper,
    factor_key = TRUE
  )
  
  observed_dependent_vars_wide <- data.frame(
    bbloq = probs$y$A$cum_perc_adh_bbloq,
    iecaara2 = probs$y$A$cum_perc_adh_ara2oieca,
    mra = probs$y$A$cum_perc_adh_arm,
    times = probs$fitted.times$A
  ) 
  observed_dependent_vars_long <- gather(
    observed_dependent_vars_wide,
    serie,
    measurement,
    bbloq:mra,
    factor_key = TRUE
  )
  
  fitted_dependent_vars_wide <- data.frame(
    bbloq = probs$fitted.y$A$cum_perc_adh_bbloq,
    iecaara2 = probs$fitted.y$A$cum_perc_adh_ara2oieca,
    mra = probs$fitted.y$A$cum_perc_adh_arm,
    times = probs$fitted.times$A
  ) 
  fitted_dependent_vars_long <- gather(
    fitted_dependent_vars_wide,
    serie,
    measurement,
    bbloq:mra,
    factor_key = TRUE
  )
  
  return(
    list(
      "predicted_risk_wide" = predicted_risk_wide,
      "fitted_dependent_vars_long" = fitted_dependent_vars_long,
      "observed_dependent_vars_long" = observed_dependent_vars_long,
      "predicted_risk_long" = predicted_risk_long
    )
  )
}
multivariant_data <- prepare_multivariant_data(probs1)
grafica <- ggplot() + 
  geom_line(
    size = 0.5,
    data = multivariant_data$fitted_dependent_vars_long, 
    mapping = aes(x = times, y = measurement, color = serie)
  ) + 
  geom_point(
    size = 1, 
    data = multivariant_data$observed_dependent_vars_long,
    mapping = aes(x = times, y = measurement, color = serie)
  ) + 
  geom_ribbon(
    data = multivariant_data$predicted_risk_wide,
    mapping = aes(
      x = times,
      ymin = Lower * 6,
      ymax = Upper * 6),
    fill = "steelblue2"
  ) +
  geom_line(
    size = 0.7,
    data = subset(multivariant_data$predicted_risk_long, serie == "Mean"), 
    mapping = aes(x = times, y = measurement * 6), 
    show.legend = FALSE
  ) +
  scale_y_continuous(
    "Adherence", 
    sec.axis = sec_axis(~. / 6 , name = "Event-Free Probability")
  ) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  geom_vline(xintercept = 6, linetype = "dotted", size = 1) +
  theme_bw() +
  theme() 
