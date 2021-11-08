
# functions ---------------------------------------------------------------
apply_JM <- function(df_jm0, patients_conditions, VARIABLESCOX_IND, VARIABLESCOX,
                     VARIABLESTODOS, OUTPATH, LONGVAR, output = 'JM') {
  
  # choose patients
  df_jm <- filter_patients(df_jm0, patients_conditions)
  # build data ---------------------------------------------------------------
  cox_df <- generate_coxdf(df_jm)
  df_jm <- preprocess_dfjm(df_jm)
  
  # Modelización de la variable longitudinal y el evento---------------------------------------------------------------
  # long_proc <- longitudinal_process(LONGVAR = LONGVAR, data_ = df_jm, tipo = "splines_cubicas")
  
  surv_object <- Surv(time = cox_df$time_to_event,
                      event = as.numeric(cox_df$event))
  
  coxFit.df_jm <- coxph(as.formula(paste("surv_object", paste(VARIABLESCOX, collapse = "+"), sep = "~")), 
                        data = cox_df,
                        x = TRUE,
                        model = TRUE)
  
  # Modelización del proceso del evento ---------------------------------------------------------------
  stan_jm(
    formulaLong = as.formula(paste(paste(eval(LONGVAR), paste('ns(month, 4)', collapse = '+'), sep = '~'),'+ (~ ns(month, 4) | id )')),
    dataLong = df_jm,
    formulaEvent = as.formula(paste("surv_object", paste(VARIABLESCOX, collapse = "+"), sep = "~")),
    dataEvent = cox_df,
    time_var = "month",
  )
  
  # 
  # # M1: Fit JM with longitudinal process (4) and event process (6)
  # M1 <- JMbayes::jointModelBayes(
  #   long_proc,
  #   coxFit.df_jm,
  #   timeVar = "month",
  #   n.iter = 30000,
  #   n.burnin = 3000)
  # saveRDS(M1, paste0(OUTPATH, paste0(output, "_M1_", LONGVAR, ".rds")))
  
  # 
  # # M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
  # dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
  #               indFixed = 2:5, indRandom = 2:5)
  # M2 <- update(M1, param = "td-both", extraForm = dForm)
  # saveRDS(M2, paste0(OUTPATH, output, "_M2_", LONGVAR, ".rds"))
  # 
  # 
  # # M3: Fit JM with longitudinal process (4) y componente de tendencia
  # dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4),
  #               indFixed = 2:5, indRandom = 2:5)
  # M3 <- update(M1, param = "td-extra", extraForm = dForm)
  # saveRDS(M3, paste0(OUTPATH, output, "_M3_", LONGVAR, ".rds"))
  # 
  # # Generar tabla resultados ----------------------------------------------------------------------------
  # JM_table <- summary_table(M1, M2, M3)
  # saveRDS(JM_table, paste0(OUTPATH, output, "JM_table_", LONGVAR, ".rds"))
}