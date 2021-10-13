library(plyr)
library(dplyr)
library(cohorteicc2)

print("case identification OK")

#' case_identification
#'
#' @param df: (data.frame) baseJoinModel
#' @param early_death_patients_days: (numeric) days to consider patients die too quickly
#' @param drugs: (character vector) Global variable drugs
#'
#' @return df (data.frame): baseJoinModel with three new boolean variables:
#' denovo_ic_paciente, denovo_tt_paciente, early_death_patient
case_identification <- function(df, early_death_patients_days, drugs){
  df <- identify_ic_denovopatients(df, ap = cohorteicc2::ap_sinanot, ing = cohorteicc2::ingresos_sec)
  df <- identify_tt_denovopatients(df, presc = cohorteicc2::presc, ing_pri = cohorteicc2::ingresos_pri, drugs=drugs)
  df <- identify_early_death_patients(df, early_death_patients_days, outcome = cohorteicc2::outcome)
  return(df)
}


#' identify_early_death_patients
#'
#' @param df_: (data.frame) baseJoinModel
#' @param early_death_patients_days: (numeric) days to consider patients die too quickly
#' @param outcome: (data.frame) data with events subsequent to discharge
#'
#' @return df_: (data.frame) baseJoinModel with the bool variable early_death_patient
identify_early_death_patients <- function(df_, early_death_patients_days, outcome) {
  fallecidos <- outcome %>% filter(dias_hasta_muerte <= early_death_patients_days)
  df_ <- df_ %>% mutate(early_death_patient = dplyr::if_else(id %in% fallecidos$id, TRUE, FALSE))
  colnames(df_)[which(colnames(df_) %in% "early_death_patient")] <- paste0("early_death_patient_", early_death_patients_days)
  return(df_)
}


#' identify_ic_denovopatients: Identify patients that had not suffered IC before being entered
#'
#' @param df_: (data.frame) baseJoinModel
#' @param ap: (data.frame) Diagnoses with annotations in episodes of Primary Care
#' @param ing: (data.frame) Diagnoses noted in hospital discharges
#'
#' @return df_: (data.frame) baseJoinModel with the bool variable denovo_ic_paciente
identify_ic_denovopatients <- function(df_, ap, ing){
  # pacientes con diagnostico de IC previo en atención primaria
  # aquellos que iniciaron episodio de IC en atencion primaria antes del ingreso indice
  # en atencion primaria usamos CIE 9
  ap <- ap %>% 
    filter(fing_ing1>finicio) %>% 
    filter(grepl('^39891|^40211|^40291|^40401|^40403|^40411|^40413|^40491|^40493|^428', cie))
  
  # pacientes con diagnostico de IC previo en hospitalizaciones
  # en hospitalizacion se coge tanto en CIE 9 como en CIE10 para identificar el diagnostico de IC
  ing <- ing %>% 
    filter(fing_ing1>fing) %>%
    filter(grepl('^39891|^40211|^40291|^40401|^40403|^40411|^40413|^40491|^40493|^428|^I0981|^I110|^I130|^I132|^I50', cie))
  
  df_ <- df_ %>% 
    mutate(denovo_ic_paciente = dplyr::if_else((!id %in% ap$id) & (!id %in% ing$id), TRUE, FALSE))
  
  return (df_)
}


#' get_principio_activo: get active principle of some families
#'
#' @param drugs: (character) Global variable DRUGS
#'
#' @return ppa active principles of dlobal variable DRUGS
get_principio_activo <- function(drugs){
  
  ppa <- c()
  if (c('arm') %in% drugs) {
    ppa <- c(ppa, c("espironolactona", "eplerenona"))
  }
  if (c('arm') %in% drugs) {
    ppa <- c(ppa, c("dapagliflozina", "empagliflozina"))
  }
  
  return(ppa)
}


#' Identify patients that had not prescribed any DRUGS before being entered
#'
#' @param df_: (data.frame) baseJoinModel
#' @param presc: (data.frame) Prescriptions: family of drugs
#' @param ing_pri: (data.frame) Hospitalizations of patients in the cohort
#' @param drugs: (character) Global variable drugs
#'
#' @return df_ with new boolean variables denovo_tt_paciente_fing and denovo_tt_paciente_falta
identify_tt_denovopatients <- function(df_, presc, ing_pri, drugs){
  
  ppa <- get_principio_activo(drugs)
  
  
  # Ver cuántos no tenían los fármacos con los que trabajamos al ingreso
  # añado la fecha de ingreso y alta
  ing_pri <- ing_pri %>% 
    distinct(id, .keep_all = TRUE) %>%
    filter(id %in% df_$id) %>% 
    select(id, fing_ing1, falta_ing1)
  
  presc_ing <- presc %>%
    dplyr::rename(id = id_paciente) %>% 
    filter((familia %in% drugs) | (principio %in% ppa)) %>%
    left_join(ing_pri, by = 'id')
  
  #prescripcion antes del alta 
  presc_ing2 <- presc_ing %>% filter(fecha_inicio<falta_ing1)
  
  #prescripcion antes del ingreso 
  presc_ing3 <- presc_ing %>% filter(fecha_inicio<fing_ing1)
  
  df_ <- df_ %>% 
    mutate(denovo_tt_paciente_fing = dplyr::if_else((!id %in% presc_ing3$id), TRUE, FALSE),
           denovo_tt_paciente_falta = dplyr::if_else((!id %in% presc_ing2$id), TRUE, FALSE),)
  return(df_)
}


