print('preprocessing OK')

#' preprocess baseJoinModel before manipulating it
#'
#' @param df
#'
#' @return df
#' @export
#'
#' @examples
preprocess_baseJoinModel <- function(df){
  # Establecer tipo de datos:
  df$id <- as.character(df$id)
  return (df)
}

#' preprocess farmacos_traye before manipulating it 
#'
#' @param df
#' @param drugs 
#'
#' @return df
#' @export
#'
#' @examples
preprocess_farmacos <- function(df, drugs){
  # seleccionar fármacos:
  df <- df[df$familia %in% drugs,]
  return (df)
}