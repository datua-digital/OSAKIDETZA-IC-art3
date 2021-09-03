library(plyr)
print('baseJoinModel_farmaco_fusion OK')


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


#' Merge farmacos_traye and baseJoinModel by id
#'
#' @param df1
#' @param df2 
#'
#' @return df
#' @export
#'
#' @examples
merge_farmacos <- function(df1, df2){
  df <- join(df1, df2, by='id', type='left')
  return (df)
}


#' process resultant data frame from merging baseJoinModel and farmacos_traye
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
process_baseJoinModel1 <- function(df){
  browser()
  # Input Not available data if proceed
  
  # filter patients that last in the cohort analyzed_period time
  
  # filter prescriptions happened during analyzed_period
  
  # set prescription limits from falta_ing1 to analyzed_period/oneyear
  
}






  