# load libraries and sources----------------------------------------------------------
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
library(splines)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "table_utils.R"))


# functions ---------------------------------------------------------------
apply_MV2JM <- function(
    df_jm, 
    patients_conditions, 
    covariables, 
    variables_longitudinales, 
    model_name_prefix = 'JMMV', 
    save_model = FALSE
) {
  # fixed variables: 
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  variables_longitudinales <<- variables_longitudinales  # es necesario para que no de un error en jointModelBayes
  
  # build and preprocess data
  df_jm <- filter_patients(df_jm, patients_conditions)
  # df_jm <- df_jm[df_jm$id %in% unique(df_jm$id)[1:300], ]
  df_jm <- preprocess_dfjm(
    df_jm, 
    variables_jm = c(covariables, variables_longitudinales, variables_ids_eventos)
  )
  df_jm <- as.data.frame(df_jm)
  cox_df <- generate_coxdf(
    df_jm,
    variables_cox = c(covariables, variables_ids_eventos)
  )
  
  ecuaciones <- list(
    as.formula(
      paste(
        paste(
          "cum_perc_adh_ara2oieca", paste('ns(month, 4)', collapse = '+'),  sep = '~'
        ),
        "(ns(month, 4) | id)", sep = '+'
      )
    ),
    as.formula(
      paste(
        paste(
          "cum_perc_adh_bbloq", paste('ns(month, 4)', collapse = '+'),  sep = '~'
        ), 
        "(ns(month, 4) | id)", sep = '+'
      )
    )
  )
  family <- list(gaussian, gaussian)
  
  # M1
  set.seed(1000)
  MixedModelFit1 <- mvglmer(
    ecuaciones,
    data = df_jm,
    families = family
  )
  
  survFit <- coxph(
    Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson + fe.reducida.severa, 
    data = cox_df, 
    model = TRUE
  )
  

  M1 <- mvJointModelBayes(MixedModelFit1, survFit, timeVar = "month")
  summary(M1)
  
  if (save_model) {
    saveRDS(M1, paste0(OUTPATH, paste0(model_name_prefix, "_M1_", ".rds")))
  }
  

  saveRDS(M1, paste0(OUTPATH, output, "_M1", ".rds"))
  
  
  # M2
  forms <- list(
    "cum_perc_adh_ara2oieca" = "value",
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_bbloq"     = "value",
    "cum_perc_adh_bbloq"     = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5 )
  )
  M2 <- update(M1, Formulas = forms)
  if (save_model) {
    saveRDS(M2, paste0(OUTPATH, paste0(model_name_prefix, "_M1_", ".rds")))
  }
  
  
  
  # M3
  forms <- list(
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                     indFixed = 2:5, indRandom = 2:5),
    "cum_perc_adh_bbloq"    = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M3 <- update(M1, Formulas = forms)
  if (save_model) {
    saveRDS(M3, paste0(OUTPATH, paste0(model_name_prefix, "_M1_", ".rds")))
  }
  
}


apply_MV2JM(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM.rds")), 
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  ), 
  covariables = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"),
  variables_longitudinales = c("cum_perc_adh_ara2oieca", "cum_perc_adh_bbloq"),
  model_name_prefix = 'JMMV2', 
  save_model = TRUE
)


# Joint model multivariante, 3 variables longitudinales -------------------

apply_MV3JM <- function(
    df_jm, 
    patients_conditions, 
    covariables, 
    model_name_prefix = 'JMMV3', 
    save_model = FALSE
) {
  
  # fixed variables: 
  variables_ids_eventos <- c("id", "event", "time_to_event", "month")
  variables_longitudinales <- c("cum_perc_adh_ara2oieca", "cum_perc_adh_bbloq", "cum_perc_adh_arm")  # es necesario para que no de un error en jointModelBayes
  
  # build and preprocess data
  df_jm <- filter_patients(df_jm, patients_conditions)
  # df_jm <- df_jm[df_jm$id %in% unique(df_jm$id)[1:300], ]
  df_jm <- preprocess_dfjm(
    df_jm, 
    variables_jm = c(covariables, variables_longitudinales, variables_ids_eventos)
  )
  df_jm <- as.data.frame(df_jm)
  cox_df <- generate_coxdf(
    df_jm,
    variables_cox = c(covariables, variables_ids_eventos)
  )
  
  # Lista de ecuaciones con su respectivas familiasa
  ecuaciones <- list(
    as.formula(
      paste(
        paste(
          "cum_perc_adh_ara2oieca", paste('ns(month, 4)', collapse = '+'),  sep = '~'
        ),
        "(ns(month, 4) | id)", sep = '+'
      )
    ),
    as.formula(
      paste(
        paste(
          "cum_perc_adh_bbloq", paste('ns(month, 4)', collapse = '+'),  sep = '~'
        ), "(ns(month, 4) | id)", sep = '+'
      )
    ),
    as.formula(
      paste(
        paste("cum_perc_adh_arm", paste('ns(month, 4)', collapse = '+'),  sep = '~'
              ), "(ns(month, 4) | id)", sep = '+'
        )
      )
  )
  
  family <- list(gaussian, gaussian,gaussian)
  
  # M1
  set.seed(1000)
  MixedModelFit1 <- mvglmer(
    ecuaciones,
    data = df_jm,
    families = family
  )
  
  survFit <- coxph(
    Surv(time_to_event, event) ~ sexo + edad_ing1 + charlson + fe.reducida.severa, 
    data = cox_df, 
    model = TRUE
  )
  
  
  M1 <- mvJointModelBayes(MixedModelFit1, survFit, timeVar = "month")
  summary(M1)
  
  saveRDS(M1, paste0(OUTPATH, output, "_M1", ".rds"))
  
  
  # M2
  forms <- list(
    "cum_perc_adh_ara2oieca" = "value",
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_bbloq"     = "value",
    "cum_perc_adh_bbloq"     = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_arm"       = "value",
    "cum_perc_adh_arm"       = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M2 <- update(M1, Formulas = forms)
  saveRDS(M2, paste0(OUTPATH, output, "_M2", ".rds"))
  
  
  # M3
  forms <- list(
    "cum_perc_adh_ara2oieca" = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5),
    "cum_perc_adh_bbloq"    = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 ),
    "cum_perc_adh_arm"      = list( fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
                                    indFixed = 2:5, indRandom = 2:5 )
  )
  M3 <- update(M1, Formulas = forms)
  saveRDS(M3, paste0(OUTPATH, output, "_M3", ".rds"))
}


apply_MV3JM(
  df_jm = readRDS(paste0(DATAOUTPATH, "df_JM.rds")), 
  patients_conditions = list(
    denovo_ic_paciente = NULL,
    denovo_tt_paciente_fing = NULL,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = NULL,
    patient_with_prescription = NULL
  ), 
  covariables = c("sexo", "edad_ing1", "charlson", "fe.reducida.severa"),
  model_name_prefix = 'JMMV3', 
  save_model = TRUE
)
