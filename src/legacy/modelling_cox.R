# Cox multivariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------
# choose patients
cox_df <- get_cox_data(
  df_jm,
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  )
)

coxph(Surv(time_to_event, event) ~ sexo + edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + fe.reducida.severa*charlson, cluster = id, cox_df)

# test different AUC calculations -----------------------------------------
# Se tiene que ejecutar primero tprescribed_drugs
library(survAUC)
library(dynpred)
tp <- predict(tprescribed_drugs)
tp2 <- predict(tprescribed_drugs)

cindex(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta,
       cox_df)
AUCt <- dynpred::AUC(Surv(time_to_event, event) ~ sexo + edad_ing1  + charlson + fe.reducida.severa + arm_prescribed_fechaalta + prescribediecaara2_fechaalta + bbloq_prescribed_fechaalta,
                     cox_df)
AUCt
AUC_CD <- AUC.cd(
  Surv(cox_df$time_to_event, cox_df$event),
  Surv(cox_df$time_to_event, cox_df$event),
  tp,
  tp2,
  seq(5, 365, 5)
)

# Cox univariante: Subset de todos pacientes de novo y no cesurados en 30 días -------------------------------------------

cox_df <- get_cox_data(
  df_jm,
  patients_conditions = list(
    denovo_ic_paciente = TRUE,
    denovo_tt_paciente_fing = TRUE,
    denovo_tt_paciente_falta = NULL,
    early_death_patient_30 = FALSE,
    patient_with_prescription = NULL
  )
)

coxph(Surv(time_to_event, event) ~ sexo, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ edad_ing1, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ charlson, cluster = id, cox_df)
coxph(Surv(time_to_event, event) ~ fe.reducida.severa, cluster = id, cox_df)