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

# saveRDS(roc, 'roc.rds')  # guardado para enseñar a Eduardo en la demo.
roc <- readRDS('roc.rds')
plot(roc)

# C-index dinámico:
dynCJM_delta1 <- JMbayes::dynCJM(M1, newdata = pbc2, Dt = 1, t.max = 12)
dynCJM_delta6 <- JMbayes::dynCJM(M1, newdata = pbc2, Dt = 6, t.max = 12)
# Calibration -------------------------------------------------------------

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

# Cross validation --------------------------------------------------------


# cargar librería paralel, para que haga cálculos en paralelo
library("parallel")
set.seed(123)

# construir df_JM y Cox_JM:
df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))
patients_conditions <- list(
  denovo_ic_paciente = NULL,
  denovo_tt_paciente_fing = NULL,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = NULL,
  patient_with_prescription = NULL
)
covariables <- c("sexo", "edad_ing1", "charlson", "fe.reducida.severa", "denovo_ic_paciente") 
variable_longitudinal <- "cum_perc_adh_guia_arm"
variables_ids_eventos <- c("id", "event", "time_to_event", "month")

# build and preprocess data
variable_longitudinal_name <- get_variable_longitudinal_name(variable_longitudinal)
df_jm <- filter_patients(df_jm, patients_conditions)
df_jm <- preprocess_dfjm(
  df_jm, 
  variables_jm = c(covariables, variable_longitudinal, variables_ids_eventos)
)
cox_df <- generate_coxdf(
  df_jm,
  variables_cox = c(covariables, variables_ids_eventos)
)

# crear splits:
V <- 5  # cross validation 5-fold
n <- nrow(cox_df) # analizar los id-s que genera
splits <- split(cox_df$id, sample(rep(seq_len(V), length.out = n)))
# función de cross validation:
CrossValJM <- function(i) {
  library("JMbayes")
  library("nlme")
  library("tidyverse")
  library("splines")
  
  get_variable_longitudinal_name <- function(variable_longitudinal) {
    if (variable_longitudinal == "cum_perc_adh_guia_arm") {
      return("guia")
    } else {
      return(variable_longitudinal)
    }
    return(df)
  }
  filter_patients <- function(df, patients_conditions) {
    if (!is.null(patients_conditions$denovo_ic_paciente)) {
      df <- df[df$denovo_ic_paciente == patients_conditions$denovo_ic_paciente, ]
    }
    if (!is.null(patients_conditions$denovo_tt_paciente_fing)) {
      df <- df[df$denovo_tt_paciente_fing == patients_conditions$denovo_tt_paciente_fing, ]
    }
    if (!is.null(patients_conditions$denovo_tt_paciente_falta)) {
      df <- df[df$denovo_tt_paciente_falta == patients_conditions$denovo_tt_paciente_falta, ]
    }
    if (!is.null(patients_conditions$early_death_patient_30)) {
      df <- df[df$early_death_patient_30 == patients_conditions$early_death_patient_30, ]
    }
    if (!is.null(patients_conditions$patient_with_prescription)) {
      df <- df[df$patient_with_prescription == patients_conditions$patient_with_prescription, ]
    }
    print(dim(df))
    return(df)
  }
  generate_coxdf <- function(df_jm, variables_cox) {
    cox_df <- df_jm[!duplicated(df_jm$id), ]
    cox_df <- cox_df[variables_cox]
    cox_df <- cox_df %>% dplyr::arrange(id, month)
    return(cox_df)
  }
  
  preprocess_dfjm <- function(df, variables_jm) {
    df <- df[variables_jm]
    df <- df %>% dplyr::arrange(id, month)
    
    df$time_to_event <- round(df$time_to_event, 6)
    df$month <- round(df$month, 6)
    
    return(df)
  }
  
  longitudinal_process <- function(variable_longitudinal, data_, tipo = 'splines_cubicas') {
    if (tipo == 'splines_cubicas') {
      long_process <- nlme::lme(
        as.formula(paste(
          eval(variable_longitudinal), paste('ns(month, 4)', collapse = '+'), sep = '~')
        ),
        random = ~ ns(month, 4) | id,
        data = data_,
        control = lmeControl(opt = 'optim')
      )
    }
    return(long_process)
  }
  
  df_jm <- readRDS(paste0("src/data/out/", "df_JM_MortOingIcc.rds"))
  patients_conditions <- list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  )
  covariables <- c("sexo", "edad_ing1", "charlson", "fe.reducida.severa", "denovo_ic_paciente") 
  variable_longitudinal <<- "cum_perc_adh_guia_arm"
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  
  # build and preprocess data
  variable_longitudinal_name <- get_variable_longitudinal_name(variable_longitudinal)
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(
    df_jm, 
    variables_jm = c(covariables, variable_longitudinal, variables_ids_eventos)
  )
  cox_df <- generate_coxdf(
    df_jm,
    variables_cox = c(covariables, variables_ids_eventos)
  )
  
  trainingData <- df_jm[!df_jm$id %in% i, ]
  trainingData.id <- trainingData[!duplicated(trainingData$id), ]
  testingData <- df_jm[df_jm$id %in% i, ]
  
  
  # Modelización de la variable longitudinal y el evento
  long_proc <- longitudinal_process(
    variable_longitudinal = variable_longitudinal,
    data_ = trainingData,
    tipo = "splines_cubicas"
  )
  
  # Modelo de Cox
  coxFit.df_jm <- coxph(
    as.formula(paste("Surv(time_to_event, event)", paste(covariables, collapse = "+"), sep = "~")),
    data = trainingData.id,
    x = TRUE,
    model = TRUE
  )
  
  # M1: Fit JM with longitudinal process (4) and event process (6)
  M1 <- JMbayes::jointModelBayes(
    long_proc,
    coxFit.df_jm,
    timeVar = "month",
    n.iter = 30000,
    n.burnin = 3000
  )
  
  auc <- aucJM(M1, newdata = testingData, Tstart = 1, Thoriz = 12)
  pe <- prederrJM(M1, newdata = testingData, Tstart = 1, Thoriz = 12)
  list(auc = auc, pe = pe)
}

# Creación de cluster y ejecución de la función de cross validation:
cl <- makeCluster(4)  # creo que en mi ordenador hay 4 cores.
res <- parLapply(cl, splits, CrossValJM)
stopCluster(cl)

# analizar resultados de AUC y PE:
mean(sapply(res, function(x) x$auc$auc))

mean(sapply(res, function(x) x$pe$prederr))

cross_validated_auc <- sapply(res, function(x) x$auc$auc)
data_cv <- data.frame(cv_sample = c('1', '2', '3', '4', '5'), PE = sapply(res, function(x) x$pe$prederr), AUC = sapply(res, function(x) x$auc$auc))

auc_alldata <- 0.7033
ggplot(data = data_cv, mapping = aes(x = cv_sample, y = AUC, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = auc_alldata, linetype = "dashed", color = "red") +
  theme_bw() +
  ylim(0.5, 1) 

pe_alldata <- 0.1897
ggplot(data = data_cv, mapping = aes(x = cv_sample, y = PE, group = 1)) +
  geom_line() +
  geom_hline(yintercept = pe_alldata, linetype = "dashed", color = "red", show.legend = TRUE) +
  theme_bw()

# AUC casero --------------------------------------------------------------

auc_casero <- function(model = readRDS(paste(OUTPATH, 'JM_1td_5bs_3rx_mortoicc.rds', sep = '/')), Tstart = 3, Thorizon = 12) {
  
  df_predictions <- get_df_prediciones_(model, Tstart)
  
  id_scores_actuals <- get_id_scores_actuals_(model, Thorizon)

  auc <- calcular_auc_(id_scores_actuals$scores, id_scores_actuals$actuals)
  
  return(auc)
}


get_df_prediciones_ <- function(model, Tstart) {
  df_model <- M1$Data$data
  ids_cumplen_condicion_Tstart <- unique(df_model[(df_model$month > Tstart), 'id'])$id
  df_predictions <- df_model[(df_model$id %in% ids_cumplen_condicion) & (df_model$month <= Tstart), ]
  return(df_predictions)
}


get_id_scores_actuals_ <- function(model, Thorizon) {
  scores <- c()
  actuals <- c()
  ids <- as.character(unique(df_predictions$id))
  
  for (id in ids) {
    df_predictions_i <- df_predictions[df_predictions$id == id, ]
    score_array_i <- survfitJM(M1, newdata = df_predictions_i, idVar = 'id', survTimes = c(Thorizon))
    scores <- c(scores, score_array_i$summaries[[1]][2])
    actuals <- c(actuals, unique(df_predictions_i$time_to_event) > Thorizon)
  }
  return(list(scores = scores, actuals = actuals))
}

calcular_auc_ <- function(scores, actuals, plot_roc = TRUE) {
  actuals <- actuals[order(scores)]
  sens <- (sum(actuals) - cumsum(actuals))/sum(actuals)
  spec <- cumsum(!actuals)/sum(!actuals)
  auc <- sum(spec*diff(c(0, 1 - sens)))
  
  if (plot_roc) {
    plot(1 - spec, sens, type = "l", col = "red", 
         ylab = "Sensitivity", xlab = "1 - Specificity")
    abline(c(0,0),c(1,1))
  }
  
  return(auc)
}

