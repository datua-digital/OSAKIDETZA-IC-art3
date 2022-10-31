# Cross validation --------------------------------------------------------
# Univariant model

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