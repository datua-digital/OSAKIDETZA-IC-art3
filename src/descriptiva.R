# load libraries and sources----------------------------------------------------------
library(survival)
library(survminer)
library(ggplot2)
library(Hmisc)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))

# script variables----------------------------------------------------------
df_jm <- readRDS(paste0(DATAOUTPATH, "df_JM_MortOingIcc.rds"))


# plot functions ------------------------------------------------------------------

plot_KM <- function(event_var, strat_var) {
  data <- readRDS(paste0(DATAOUTPATH, data_for_event(event_var), ".rds"))
  strat_var_ <<- strat_var
  if (is.numeric(data[[strat_var_]])) {
    data <- categorize_var(strat_var_, data)
    strat_var_ <<- paste0(strat_var_, '_2')
  }
  # fit and plot KM
  surv_object <- Surv(time = data$time_to_event, event = as.numeric(data$event))
  
  fit_km <- survival::survfit(surv_object ~ data[[strat_var_]])
  # fit_km <- survival::survfit(surv_object ~ data$fe.reducida.severa)
  print(
    ggsurvplot(
      fit_km, 
      data = data, 
      pval = TRUE,
      conf.int = TRUE,
      ggtheme = theme_bw()
    )
  )
}

categorize_var <- function(strat_var, data) {
  data[[paste0(strat_var, '_2')]] <- as.numeric(Hmisc::cut2(data[[strat_var]], g = 4))
  return (data)
}


plot_KM(event_var = "MortOingIcc", strat_var = "sexo")
plot_KM(event_var = "MortOingIcc", strat_var = "fe.reducida.severa")
plot_KM(event_var = "MortOingIcc", strat_var = "arm_prescribed_fechaalta")
plot_KM(event_var = "MortOingIcc", strat_var = "prescribediecaara2_fechaalta")
plot_KM(event_var = "MortOingIcc", strat_var = "bbloq_prescribed_fechaalta")
plot_KM(event_var = "MortOingIcc", strat_var = "denovo_ic_paciente")
plot_KM(event_var = "MortOingIcc", strat_var = "edad_ing1")
plot_KM(event_var = "MortOingIcc", strat_var = "charlson")