#' preprocess baseJoinModel before manipulating it
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) with columns types set
preprocess_base_join_model <- function(df) {
  # Establecer tipo de datos
  df$id <- as.character(df$id)
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
  return(df)
}

print("preprocessing OK")