library(plyr)
library(dplyr)

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
  df <- plyr::join(df1, df2, by='id', type='left')
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
process_baseJoinModel1 <- function(df, duration){
  # filter patients with drug prescriptions
  cols <- c('familia', 'end', 'dura', 'tip', 'estado_obje')
  df <- df[!rowSums(is.na(df[cols])), ]
  
  # filter, for each patient, prescriptions happened during follow up
  ## id
  df <- df %>%
    group_by(id) %>% 
    filter((end > falta_ing1) & (start < (falta_ing1 + duration)))
  
  # set prescription limits from falta_ing1 to analyzed_period/oneyear
  df <- df %>%
    mutate(start = if_else(start < falta_ing1, falta_ing1, start)) %>%
    mutate(end = if_else(end > (falta_ing1 + duration), falta_ing1 + duration, end)) %>%
    mutate(end = if_else(end > fmort2, fmort2, end))
  
  # filter prescription periods where end > start
  df <- df[(df['end'] > df['start']) | is.na(df['end']),]
}



  