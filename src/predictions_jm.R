# load libraries and sources----------------------------------------------------------
library(data.table)
library(ggplot2)
library(JMbayes)
library(splines)
library(tidyverse)
library(gridExtra)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))

# Logic----------------------------------------------------------

M1 <- readRDS(paste(OUTPATH, 'JM3_M1_cum_perc_adh_guia_arm.rds', sep = '/'))
M2 <- readRDS(paste(OUTPATH, 'JM3_M2_cum_perc_adh_guia_arm.rds', sep = '/'))
M3 <- readRDS(paste(OUTPATH, 'JM3_M3_cum_perc_adh_guia_arm.rds', sep = '/'))

# functions
read_prepare <- function(name, DATAINPATH){
  ndata <- read.csv(paste(DATAINPATH, name, sep = '/'))
  ndata$id <- factor(ndata$id)
  ndata$sexo <- factor(ndata$sexo, levels = c('Mujer', 'Hombre'))
  return(ndata)
}

compare_pred_plots <- function(ndata, ndata2, name1=NULL, name2=NULL){
  probs1 <- survfitJM(M2, newdata = ndata, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  probs2 <- survfitJM(M2, newdata = ndata2, idVar = 'id', survTimes = seq(1, 48, 1)/4)
  
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

# slope effect
ndata <- read_prepare('mockdata_slope_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_slope_id2.csv', DATAINPATH)
compare_pred_plots(ndata, ndata2, name1 = "Patient - slope0", name2 = "Patient - slope1")

# value effect
ndata <- read_prepare('mockdata_value_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_value_id2.csv', DATAINPATH)
compare_pred_plots(ndata, ndata2, name1 = "Patient - value6", name2 = "Patient - value1")

# age effect
ndata <- read_prepare('mockdata_edad_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_edad_id2.csv', DATAINPATH)
compare_pred_plots(ndata, ndata2, name1 = "Patient - edad50", name2 = "Patient - edad89")

# sex effect
ndata <- read_prepare('mockdata_sexo_id1.csv', DATAINPATH)
ndata2 <- read_prepare('mockdata_sexo_id2.csv', DATAINPATH)
compare_pred_plots(ndata, ndata2, name1 = "Patient - sexoMujer", name2 = "Patient - sexoHombre")

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

# TODO: Testear el shiny
# runDynPred()
