source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")


# Function calculation ----------------------------------------------------

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


# tests -------------------------------------------------------------------

df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))


# auc casero for cox
get_cox_data <- function(df_jm, patients_conditions) {
  df_jm <- filter_patients(df_jm, patients_conditions)
  df_jm <- preprocess_dfjm(df_jm, variables_jm = colnames(df_jm))
  cox_df <- df_jm[!duplicated(df_jm$id), ]
  return(cox_df)
}

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
