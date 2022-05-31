library(plyr)
library(dplyr)
library(cohorteicc2)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "drug_utils.R"))

# prescribed_to_guia ------------------------------------------------------
get_guia_prescribed_infechaalta <- function(df, drugs){
  df_fechaalta_prescribed <- 
    get_prescribed_drugs_infechaalta(
      ids = df$id,
      drugs = drugs
  )

  df <- assign_drugs_infechaalta(df, df_fechaalta_prescribed, drugs, mode = 'prescribed')
  
  df <- assign_prescribedtoguia(df, mode = 'prescribed')
  
  df <- assign_prescribediecaoara2(df, mode = 'prescribed')
  
  df <- assign_precribedptot(df, mode = 'prescribed')
  
  return(df)
}

get_prescribed_drugs_infechaalta <- function(ids, drugs) {
  
  ppa <- get_principio_activo(drugs)
  
  ing_pri <- cohorteicc2::ingresos_pri %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(id %in% ids) %>%
    dplyr::select(id, falta_ing1)
  presc_ing <- cohorteicc2::presc %>% 
    dplyr::select(id_paciente, familia, principio, fecha_inicio, fecha_fin) %>%
    dplyr::filter(id_paciente %in% ids) %>%
    dplyr::rename(id = id_paciente) %>%
    dplyr::filter((familia %in% drugs) | (principio %in% ppa)) %>%
    dplyr::left_join(ing_pri, by = "id")
  presc_ing[presc_ing$principio %in% ppa, 'familia'] <- 'arm'
  
  # prescripción con inicio antes del alta y finalización después del ingreso
  df_fechaalta_prescribed <- presc_ing %>% 
    filter(
      (fecha_inicio <= falta_ing1) 
      & (fecha_fin > falta_ing1)
    )
  return(df_fechaalta_prescribed)
}

assign_drugs_infechaalta <- function(df, df_fechaalta_prescribed, drugs, mode){
  drug <- drugs[4]
  for (drug in drugs) {
    drug_prescribed_ids <- unique(
      df_fechaalta_prescribed[(df_fechaalta_prescribed$familia %in% drug), 'id']
    )
    df[paste0(drug, '_', mode, '_fechaalta')] <- df$id %in% drug_prescribed_ids
  }
  
  return(df)
}

assign_precribedptot <- function(df, mode) {
  if (mode == 'prescribed') {
    df$pieca <- as.numeric(df$prescribediecaara2_fechaalta)
    df$pbb <- as.numeric(df$bbloq_prescribed_fechaalta)
    df$parm <- as.numeric(df$arm_prescribed_fechaalta)
    df$ptot <- paste0(df$pbb, df$pieca, df$parm)
  }
  
  return(df)
}

assign_prescribediecaoara2 <- function(df, mode) {
  if (mode == 'prescribed') {
    df$prescribediecaara2_fechaalta <- (df$ara2_prescribed_fechaalta) | (df$ieca_prescribed_fechaalta)
  } else {
    df$adherencediecaara2_fechaalta <- (df$ara2_adherenced_fechaalta) | (df$ieca_adherenced_fechaalta)
  }
  
  return(df)
}

assign_prescribedtoguia <- function(df, mode) {
  if (mode == 'prescribed') {
    df$prescribedtoguia_fechaalta <- 
      ((df$ara2_prescribed_fechaalta) | (df$ieca_prescribed_fechaalta)) & 
      (df$bbloq_prescribed_fechaalta) & 
      (df$arm_prescribed_fechaalta)
  } else {
    df$adherencedtoguia_fechaalta <- 
      ((df$ara2_adherenced_fechaalta) | (df$ieca_adherenced_fechaalta)) & 
      (df$bbloq_adherenced_fechaalta) & 
      (df$arm_adherenced_fechaalta)
  }
  
  return(df)
}


# adherenced_to_guia ------------------------------------------------------

get_guia_adherenced_infechaalta <- function(df, drugs){
  df_fechaalta_adherenced <- 
    get_adherenced_drugs_infechaalta(
      ids = df$id,
      drugs = drugs
    )
  
  df <- assign_drugs_infechaalta(
    df,
    df_fechaalta_adherenced,
    drugs,
    mode = 'adherenced'
  )

  df <- assign_prescribedtoguia(df, mode = 'adherenced')
  
  df <- assign_prescribediecaoara2(df, mode = 'adherenced')
  
  return(df)
}

get_adherenced_drugs_infechaalta <- function(ids, drugs) {

  ing_pri <- cohorteicc2::ingresos_pri %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(id %in% ids) %>%
    dplyr::select(id, falta_ing1)
  ing_pri$id <- factor(ing_pri$id)
  farmacos <- constructedBases::farmacos_traye %>% 
    dplyr::select(id, familia, tip, start, end) %>%
    dplyr::filter(id %in% ids) %>%
    dplyr::filter(tip == "2a") %>%
    dplyr::filter((familia %in% drugs)) %>%
    dplyr::left_join(ing_pri, by = "id")
  # adherencia con inicio antes del alta y finalización después del ingreso
  df_fechaalta_adherenced <- farmacos %>% 
    filter(
      (start <= falta_ing1) 
      & (end > falta_ing1)
    )
  
  return(df_fechaalta_adherenced)
}
