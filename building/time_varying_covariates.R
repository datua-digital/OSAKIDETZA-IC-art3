print('time_varying_covariates.R OK')

# load sources ------------------------------------------------------------
source('utils/df_utils.R')



#' reshape_towideformat
#'
#' @param df 
#'
#' @return df_reshaped (data.frame) 
#' @export
#'
#' @examples
reshape_towideformat <- function(df){
  df$start <- NULL
  df$end <- NULL
  df$days <- NULL
  df$group_id <- NULL
  df$dura <- NULL
  df$duration <- NULL
  df$tip <- NULL
  df_reshaped <- longtowide(as.data.frame(df), 
                            idvar_=c('id', 'month'),
                            timevar_=c("familia"),
                            v.names=c("perc_adh", "estado_obje"),
                            direction="wide")
  return(df_reshaped)
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
adherencia_farmacos <- function(df){
  df <- df %>% 
    mutate(perc_adh=dplyr::if_else(tip=='2a', length(collapsedstring_tovector(days))/30 * 100 , 0))
  df <- reshape_towideformat(df)
  
  return(df)
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_adherence_guia <- function(x){
  list(unlist(strsplit(x$days, split=',')))
  Reduce(intersect, list(c(1,2,3), c(1,3,4), c(1,3,4)))
  
}

#' adherencia_farmacos_guia
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
adherencia_farmacos_guia <- function(df, drugs){
  # filter adherent patients: tip 2a
  PAUTA <- data.frame(bbloq='always', ieca='optional', ara2='optional')
  df <- df %>% 
    filter(tip='2a') %>%
    group_by(id, month) %>% 
    group_modify(~get_adherence_guia(.x))
  df <- df %>% group_by(id, month) %>% group_modify(~get_adherence_guia(.x))
  
  # get adh_guia
  
  
  # fill empty months
  
  return (df)
}


#' adherencia_farmacos_medico
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
adherencia_farmacos_medico <- function(df, drugs){
  # filtrar adherentes + 1c?
  
  # get adh_medico
  
  return (df)
}

