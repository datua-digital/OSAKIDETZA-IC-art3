print('time_varying_covariates.R OK')

# load sources ------------------------------------------------------------
source('utils/df_utils.R')


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
  
  df$start <- NULL
  df$end <- NULL
  df$days <- NULL
  df$group_id <- NULL
  df$dura <- NULL
  df$duration <- NULL
  df$tip <- NULL
  
  df <- longtowide(as.data.frame(df), 
                   idvar_=c('id', 'month'),
                   timevar_=c("familia"),
                   v.names=c("perc_adh", "estado_obje"),
                   direction="wide")
  
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
  final_x <- x[1, ]
  days_guia <- c()
  if ((c('bbloq') %in% x$familia) & any(c('ieca', 'ara2') %in% x$familia)){
    if (all(c('ieca', 'ara2') %in% x$familia)){
      ieca_bbloq_adherent <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c('ieca')),'days'])),
                                       collapsedstring_tovector(as.character(x[which(x$familia %in% c('bbloq')),'days'])))
      ara2_bbloq_adherent <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c('ara2')),'days'])),
                                       collapsedstring_tovector(as.character(x[which(x$familia %in% c('bbloq')),'days'])))
      days_guia <- union(ieca_bbloq_adherent, ara2_bbloq_adherent)
    } else if(c('ieca') %in% x$familia){
      days_guia <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c('ieca')),'days'])),
                             collapsedstring_tovector(as.character(x[which(x$familia %in% c('bbloq')),'days'])))
    } else{ # ara2 in x$familia
      days_guia <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c('ara2')),'days'])),
                             collapsedstring_tovector(as.character(x[which(x$familia %in% c('bbloq')),'days'])))
    }
  }
  final_x$days_guia <- vector_tocollapsedstring(days_guia)
  return(final_x)
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
    filter(tip=='2a') %>%
    group_by(id, month) %>% 
    group_modify(~get_adherence_guia(.x))
  
  df <- df %>% 
    mutate(perc_adh_guia=dplyr::if_else(tip=='2a', length(collapsedstring_tovector(days_guia))/30 * 100 , 0))
  
  df$start <- NULL
  df$end <- NULL
  df$days <- NULL
  df$group_id <- NULL
  df$familia <- NULL
  df$dura <- NULL
  df$duration <- NULL
  df$tip <- NULL
  df$estado_obje <- NULL # is added with adherencia_farmacos
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

