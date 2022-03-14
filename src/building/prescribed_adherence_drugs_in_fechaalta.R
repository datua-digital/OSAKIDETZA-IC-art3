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

  df <- assign_drugs_prescibed_infechaalta(df, df_fechaalta_prescribed, drugs)
  
  df <- assign_prescribedtoguia(df)
  return(df)
}

assign_drugs_prescibed_infechaalta <- function(df, df_fechaalta_prescribed, drugs){
  for (drug in drugs) {
    drug_prescribed_ids <- unique(
      df_fechaalta_prescribed[(df_fechaalta_prescribed$familia %in% drug), 'id']
    )
    df[paste0(drug, '_prescribed')] <- df$id %in% drug_prescribed_ids
  }
  
  return(df)
}

get_prescribed_drugs_infechaalta <- function(ids, drugs) {
  # Ver cuántos no tenían los fármacos con los que trabajamos al ingreso
  # añado la fecha de ingreso y alta
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
  # prescripcion con inicio antes del alta y finalización después del ingreso
  df_fechaalta_prescribed <- presc_ing %>% 
    filter(
      (fecha_inicio < falta_ing1) 
      & (fecha_fin > falta_ing1)
    )
  return(df_fechaalta_prescribed)
}

assign_prescribedtoguia <- function(df) {
  df$prescribedtoguia <- 
    ((df$ara2_prescribed) | (df$ieca_prescribed)) & 
    (df$bbloq_prescribed) & 
    (df$arm_prescribed)
  return(df)
}




# adherenced_to_guia ------------------------------------------------------


