print ('montly_arrangement OK')



#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
rearranged_in_months <- function(df){
  baseJoinModel1 <- scale_startend(baseJoinModel_2)
  baseJoinModel2 <- divide_monthly_periods(baseJoinModel1)
  baseJoinModel2 <- build_monthly_data(baseJoinModel1)
  return (df)
}


#' Add time var
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
scale_startend <- function(df){
  df <- df %>%
    mutate(start_time = (start - falta_ing1 + 1)/30) %>%
    mutate(end_time = (end - falta_ing1 + 1)/30)
  df$start_time <- as.numeric(df$start_time, units="days")
  df$end_time <- as.numeric(df$end_time, units="days")
  return (df)
}


#' divide records in monthly periods
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
divide_monthly_periods <- function(df){
  return (df)
}


#' build monthly distributed data
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
build_monthly_data <- function(df){
  return(df)
}
