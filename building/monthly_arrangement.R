print ('montly_arrangement OK')



#' Rearrange prescription data in months
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
rearranged_in_months <- function(df){
  baseJoinModel1 <- scale_startend(df)
  baseJoinModel2 <- divide_monthly_periods(baseJoinModel1)
  return (baseJoinModel2)
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


split_inside_months <- function(x){
  
  start_month <- floor(x$start_time)
  final_month <- floor(x$end_time)
  count = 0
  for (m in c(start_month:final_month)) {

    new_row <- x
    if (m < x$start_time) {
      low_limit <- x$start_time
    } else {
      low_limit <- m
    }

    if (m + 1 > x$end_time) {
      high_limit <- x$end_time
    } else {
      high_limit <- m + 1
    }
    
    new_row$month <- m + 1
    new_row$start_time <- low_limit
    new_row$end_time <- high_limit

    if (count >= 1) {
      x_final <- rbind(x_final, new_row)
    } else {
      x_final <- new_row
    }
    count = count + 1
    
  }
  return (x_final)
}


transform_to_days <- function(x){
  days <- c()
  for (row in 1:nrow(x)) {
    month_floor <- floor(x[row, "start_time"])
    first_day <- ceiling((x[row, "start_time"] - month_floor)*30)
    last_day <- ceiling((x[row, "end_time"] - month_floor)*30)
    appending_days <- c(as.numeric(first_day):as.numeric(last_day))
    days <- c(days, appending_days)
    }
  x$start_time <- NULL
  x$end_time <- NULL
  final_x <- x[1, ]
  days <- unique(days[!(days %in% 0)])
  final_x$days <- paste(days, sep="", collapse=",")
  return(final_x)
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
  df <- df %>% group_by(id) %>% mutate(group_id = row_number())
  df <- df %>% group_by(id, group_id) %>% group_modify(~split_inside_months(.x))
  saveRDS(df, paste0(DATA_OUT_PATH, 'baseJoinModel_after_splitted_in_months.rds'))
  df <- df %>% group_by(id, familia, month) %>% group_modify(~transform_to_days(.x))
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
fill_monthly_data <- function(df){
  return(df)
}
