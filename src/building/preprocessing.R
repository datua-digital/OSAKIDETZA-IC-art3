#' preprocess baseJoinModel before manipulating it
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) with columns types set
preprocess_base_join_model <- function(df, event) {
  # Establecer tipo de datos
  df$id <- as.character(df$id)
  df$sexo <- factor(df$sexo)
  df$fecha_evento <- df[[event]]
  return(df)
}


#' preprocess farmacos_traye before manipulating it
#'
#' @param df (data.frame)
#' @param drugs (character vector) DRUG global parameter
#'
#' @return df (data.frame) data with selected drugs
preprocess_farmacos <- function(df, drugs) {
  # seleccionar fármacos
  df <- df[df$familia %in% drugs, ]
  df$id <- as.character(df$id)
  return(df)
}

#' preprocess basal_ch before manipulating it
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) with columns types set
preprocess_basal_ch <- function(df) {
  # Establecer tipo de datos
  df$id <- as.character(df$id)
  df <- df[c('id', 'charlson')]
  return(df)
}


print("preprocessing OK")