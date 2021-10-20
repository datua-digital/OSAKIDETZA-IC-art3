print("montly_arrangement OK")

# load sources ------------------------------------------------------------
source("utils/df_utils.R")

#' Rearrange prescription data in months
#'
#' @param df (data.frame)
#'
#' @return (data.frame)
rearranged_in_months <- function(df) {
  base_join_model1 <- scale_startend(df)
  base_join_model2 <- divide_monthly_periods(base_join_model1)
  return(base_join_model2)
}
#' scale_startend: add start_time and end_time in a numeric and relative scale
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) start time and end time columns added
scale_startend <- function(df) {
  df <- df %>%
    dplyr::mutate(start_time = (start - falta_ing1) / 30) %>%
    dplyr::mutate(end_time = (end - falta_ing1) / 30) %>%
    dplyr::mutate(dura_in_months = if_else((MortOingIcc - falta_ing1) / 30 < 12,
                                    (MortOingIcc - falta_ing1) / 30, 12)) %>%
    dplyr::mutate(last_month = if_else((MortOingIcc - falta_ing1) / 30 < 12,
                                ceiling((MortOingIcc - falta_ing1) / 30),
                                12))
  df$start_time <- as.numeric(df$start_time, units = "days") # real scale is month, which is not covered by the library
  df$end_time <- as.numeric(df$end_time, units = "days")
  return(df)
}


#' split_inside_months: split numeric start_time and end_time in ranges between integers (integers are months!)
#'
#' @param x (data.frame) Part of df
#'
#' @return x_final (data.frame) month column added, start time and end time modified
split_inside_months <- function(x) {
  start_month <- floor(x$start_time)
  final_month <- min(floor(x$end_time), 11) # month 13 not allowed
  if (final_month >= start_month) {
    count <- 0
    for (m in c(start_month:final_month)) {
      # lower bound
      new_row <- x
      if (m < x$start_time) {
        low_limit <- x$start_time
      } else {
        low_limit <- m
      }
      # upper bound
      if (m + 1 > x$end_time) {
        high_limit <- x$end_time
      } else {
        high_limit <- m + 1
      }
      # define new row columns
      new_row$month <- m + 1
      new_row$start_time <- low_limit
      new_row$end_time <- high_limit
      # bind row
      if (count >= 1) {
        x_final <- rbind(x_final, new_row)
      } else {
        x_final <- new_row
      }
      count <- count + 1
    }
  } else{
    new_row <- x
    new_row$month <- NA
    new_row$start_time <- NA
    new_row$end_time <- NA
    x_final <- new_row
  }
  return(x_final)
}


#' transform_to_days: transform periods between months in days of months
#'
#' @param x (data.frame) Part of df
#'
#' @return final_x (data.frame) days column added
transform_to_days <- function(x) {
  days <- c()
  for (row in seq_len(nrow(x))) {
    month_floor <- floor(x[row, "start_time"])
    first_day <- ceiling((x[row, "start_time"] - month_floor) * 30)
    last_day <- ceiling((x[row, "end_time"] - month_floor) * 30)
    appending_days <- c(as.numeric(first_day):as.numeric(last_day))
    days <- c(days, appending_days)
    }
  x$start_time <- NULL
  x$end_time <- NULL
  final_x <- x[1, ]
  days <- unique(days[!(days %in% c(0))]) # day 0 not allowed
  final_x$days <- vector_tocollapsedstring(days)
  return(final_x)
}


#' divide records in monthly periods
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) arranged in monthly chunks
divide_monthly_periods <- function(df) {
  df <- df %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(group_id = row_number())
  df <- df %>%
    group_by(id, group_id, .drop = FALSE) %>%
    group_modify(~split_inside_months(.x))
  # calcular last_day
  df <- df %>%
    dplyr::mutate(last_day = if_else(last_month == month,
                              ceiling((dura_in_months - (last_month - 1)) * 30),
                              30))
  df$last_day <- as.numeric(df$last_day, units = "days")
  df <- df[!is.na(df[c("month")]), ]
  saveRDS(df, paste0(DATAOUTPATH, "baseJoinModel_after_splitted_in_months.rds"))
  df <- df %>%
    group_by(id, familia, month, tip) %>%
    group_modify(~transform_to_days(.x))
  return(df)
}
