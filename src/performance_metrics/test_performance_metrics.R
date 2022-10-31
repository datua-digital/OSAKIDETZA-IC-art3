# load libraries and sources----------------------------------------------------------
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
library(splines)
library(ggplot2)
library(plotly)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))


M1 <- readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/'))
summary(M1)
# AUC ---------------------------------------------------------------------


JMbayes::aucJM(M1, M1$Data$data, Tstart = 1, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 2, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 3, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 4, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 5, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 6, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 7, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 8, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 9, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 10, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 11, Thoriz = 12)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 12, Thoriz = 12)


JMbayes::aucJM(M1, M1$Data$data, Tstart = 1, Thoriz = 2)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 2, Thoriz = 3)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 3, Thoriz = 4)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 4, Thoriz = 5)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 5, Thoriz = 6)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 6, Thoriz = 7)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 7, Thoriz = 8)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 8, Thoriz = 9)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 9, Thoriz = 10)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 10, Thoriz = 11)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 11, Thoriz = 12)

JMbayes::aucJM(M1, M1$Data$data, Tstart = 0, Thoriz = 11)
JMbayes::aucJM(M1, M1$Data$data, Tstart = 0.5, Thoriz = 12)

# organizando resultados:

AUC_banda <- c('1-12', '2-12', '3-12', '4-12', '5-12', '6-12', '7-12', '8-12', '9-12', '10-12', '11-12')
AUC_banda_res <- c(0.7033, 0.6999, 0.6943, 0.6929, 0.6836, 0.6917, 0.6899, 0.6758, 0.6579, 0.6784, 0.6349)

AUC_inc <- c('1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', '9-10', '10-11', '11-12')
AUC_inc_res <- c(0.6611, 0.6889, 0.6663, 0.702, 0.6183, 0.6814, 0.7198, 0.7168, 0.6164, 0.7112, 0.6349)

AUC_experiment <- c('0-11', '0.5-11')
AUC_experiment_res <- c(NaN, NaN)

AUC_data <- data.frame(AUC_inc, AUC_inc_res)
AUC_data$AUC_inc <- factor(AUC_inc, levels = c('1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', '9-10', '10-11', '11-12'))
AUC_data$AUC_banda <- factor(AUC_banda, levels = c('1-12', '2-12', '3-12', '4-12', '5-12', '6-12', '7-12', '8-12', '9-12', '10-12', '11-12'))

ggplotly(
  ggplot(AUC_data, mapping = aes(x = AUC_banda, y = AUC_banda_res, group = 1)) + 
    geom_line() + 
    geom_point() +
    theme_bw() +
    ylim(0.5, 1) + 
    xlab('JMBayes AUC JM_1td_5bs_3rx_mortoicc.rds')
)


ggplotly(
  ggplot(AUC_data, mapping = aes(x = AUC_inc, y = AUC_inc_res, group = 1)) + 
    geom_line() + 
    geom_point() +
    theme_bw() +
    ylim(0.5, 1) + 
    xlab('JMBayes AUC JM_1td_5bs_3rx_mortoicc.rds')
)

# Curva ROC:

roc <- JMbayes::rocJM(M1, M1$Data$data, Tstart = 1, Thoriz = 12)

roc <- readRDS('roc.rds')
plot(roc)

# C-index dinámico:
dynCJM_delta1 <- JMbayes::dynCJM(M1, newdata = pbc2, Dt = 1, t.max = 12)
dynCJM_delta6 <- JMbayes::dynCJM(M1, newdata = pbc2, Dt = 6, t.max = 12)

# Calibration ------------------------------------------------------------

# PE

prediction_error <- JMbayes::prederrJM(M1, M1$Data$data, Tstart = 1, Thoriz = 12)
prediction_error
# saveRDS(prediction_error, 'prediction_error.rds')  # guardado para enseñar a Eduardo en la demo.
prediction_error <- readRDS('prediction_error.rds')

# IPE: Interval prediction error
interval_prediction_error <- JMbayes::prederrJM(M1, M1$Data$data, Tstart = 1, Thoriz = 12, interval = TRUE)
interval_prediction_error
saveRDS(interval_prediction_error, 'interval_prediction_error.rds')  # guardado para enseñar a Eduardo en la demo.
interval_prediction_error <- readRDS('interval_prediction_error.rds')

