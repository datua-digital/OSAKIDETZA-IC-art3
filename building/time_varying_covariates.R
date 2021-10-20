# load sources ------------------------------------------------------------
source("utils/df_utils.R")

#' merge_timevarying_vars
#'
#' @param df0 (data.frame) df with adherence of each drug
#' @param df1 (data.frame) df with adherence of guide
#' @param df2 (data.frame) df with adherence of doctor
#'
#' @return df (data.frame) data merged
merge_timevarying_vars <- function(df0, df1, df2) {
  df_ <- df0 %>%
    dplyr::left_join(df1[c("id", "month", "perc_adh_guia")], by = c("id", "month"))
  df <- df_ %>%
    dplyr::left_join(df2[c("id", "month", "perc_adh_doctor")], by = c("id", "month"))
  return(df)
}

# adherence of each drug --------------------------------------------------

#' adherencia_farmacos
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) data with adherence of each drug, all in wide format
adherencia_farmacos <- function(df) {
  df <- df %>%
    dplyr::mutate(perc_adh = dplyr::if_else(tip == "2a",
                                     (length(collapsedstring_tovector(days)) / last_day) * 100,
                                     0))
  df <- deletemultiplecolumns(df,
                              c("start", "end", "days", "group_id", "dura",
                                "duration", "tip", "estado_obje"))
  df <- longtowide(as.data.frame(df),
                   idvar_ = c("id", "month"),
                   timevar_ = c("familia"),
                   v.names_ = c("perc_adh"),
                   direction_ = "wide"
                   )
  return(df)
}


# adherence of the guide --------------------------------------------------

#' adherencia_farmacos_guia
#'
#' @param df (data.frame)
#'
#' @return df (data.frame ) data with adherence respect to guide
adherencia_farmacos_guia <- function(df) {
  # filter adherent patients: tip 2a
  df <- df %>%
    dplyr::group_by(id, month) %>%
    dplyr::group_modify(~get_days_adhguia(.x))
  df <- df %>%
    dplyr::mutate(perc_adh_guia = (length(collapsedstring_tovector(days_adhguia)) / last_day) * 100)

  df <- deletemultiplecolumns(df,
                              c("start", "end", "days", "days_adhguia", "group_id",
                                "familia", "dura", "duration", "tip", "estado_obje"))
  return(df)
}


#' get_days_adhguia
#'
#' @param x (data.frame) Part of df
#'
#' @return final_x x with a new column days_adhguia
get_days_adhguia <- function(x) {
  final_x <- x[1, ]
  days_adhguia <- c()
  if (any(x$tip %in% c("2a"))) {
    x <- x[x$tip %in% c("2a"), ]
    if ((c("bbloq") %in% x$familia) & any(c("ieca", "ara2") %in% x$familia)) {
      if (all(c("ieca", "ara2") %in% x$familia)) {
        ieca_bbloq_adherent <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c("ieca")), "days"])),
                                         collapsedstring_tovector(as.character(x[which(x$familia %in% c("bbloq")), "days"])))
        ara2_bbloq_adherent <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c("ara2")), "days"])),
                                         collapsedstring_tovector(as.character(x[which(x$familia %in% c("bbloq")), "days"])))
        days_adhguia <- union(ieca_bbloq_adherent, ara2_bbloq_adherent)
      } else if (c("ieca") %in% x$familia) {
        days_adhguia <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c("ieca")), "days"])),
                                  collapsedstring_tovector(as.character(x[which(x$familia %in% c("bbloq")), "days"])))
      } else{ # ara2 in x$familia
        days_adhguia <- intersect(collapsedstring_tovector(as.character(x[which(x$familia %in% c("ara2")), "days"])),
                                  collapsedstring_tovector(as.character(x[which(x$familia %in% c("bbloq")), "days"])))
      }
    }
  }
  final_x$days_adhguia <- vector_tocollapsedstring(days_adhguia)
  return(final_x)
}

# adherence of the doctor's prescription --------------------------------------------------

#' adherencia_farmacos_medico
#'
#' @param df (data.frame)
#'
#' @return df (data.frame) data with adherence respect to doctor's prescriptions
adherencia_farmacos_medico <- function(df) {
  df <- df %>%
    dplyr::group_by(id, month) %>%
    dplyr::group_modify(~get_days_adhdoctor(.x))
  df <- df %>%
    dplyr::mutate(perc_adh_doctor = (length(collapsedstring_tovector(days_adhdoctor)) / last_day) * 100)
  df <- deletemultiplecolumns(df,
                              c("start", "end", "days", "days_adhdoctor", "group_id",
                                "familia", "dura", "duration", "tip", "estado_obje"))
  return(df)
}


#' get_days_adhdoctor
#'
#' @param x (data.frame) Part of df
#'
#' @return final_x (data.frame) x with a new column days_adhdoctor
get_days_adhdoctor <- function(x) {
  final_x <- x[1, ]
  days_adhdoctor <- c()
  if (any(x$tip %in% c("1c", "2a"))) {
    # days not beeing adherent in some drugs
    days_not_completely_adherent <- c()
    b_days_not_completely_adherent <- x[!x$tip %in% c("1c", "2a"), "days"]
    for (days in b_days_not_completely_adherent) {
      days_not_completely_adherent <- c(days_not_completely_adherent,
                                               collapsedstring_tovector(days))
    }
    days_not_completely_adherent <- unique(days_not_completely_adherent)
    # days beeing adherent in some drugs:
    days_beeing_partially_adherent <- c()
    g_days_beeing_partially_adherent <- x[x$tip %in% c("1c", "2a"), "days"]
    for (days in g_days_beeing_partially_adherent) {
      days_beeing_partially_adherent <- c(days_beeing_partially_adherent,
                                          collapsedstring_tovector(days))
    }
    days_beeing_partially_adherent <- unique(days_beeing_partially_adherent)
    days_adhdoctor <- setdiff(unique(days_beeing_partially_adherent), unique(days_not_completely_adherent))
  }
  final_x$days_adhdoctor <- vector_tocollapsedstring(days_adhdoctor)
  return(final_x)
}


acum_month <- function(df) {
  df <- df %>%
    dplyr::mutate(cum_perc_adh_ara2 = cumsum(perc_adh_ara2) / 100,
                  cum_perc_adh_bbloq = cumsum(perc_adh_bbloq) / 100,
                  cum_perc_adh_ieca = cumsum(perc_adh_ieca) / 100,
                  cum_perc_adh_doctor = cumsum(perc_adh_doctor) / 100,
                  cum_perc_adh_guia = cumsum(perc_adh_guia) / 100)
  return(df)
}

print("time_varying_covariates.R OK")
