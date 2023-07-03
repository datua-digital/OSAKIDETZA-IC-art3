# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

# load libraries and sources----------------------------------------------------------
library(survival)
library(survminer)
library(ggplot2)
library(Hmisc)
library(tidyverse)
library(rlang)

source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(UTILSSCRIPTSPATH, "jm_utils.R"))
source(paste0(UTILSSCRIPTSPATH, "df_utils.R"))

# plot functions ------------------------------------------------------------------

# Kaplan Meier plots
plot_kaplanmeier <- function(event_var, strata_var) {
  data <- readRDS(paste0(DATAOUTPATH, data_for_event(event_var), ".rds"))
  strata_var_ <<- strata_var
  if (is.numeric(data[[strata_var_]])) {
    data <- categorize_numeric_var(strata_var_, data)
    strata_var_ <<- paste0(strata_var_, '_2')
  }
  # fit and plot KM
  surv_object <<- Surv(time = data$time_to_event, event = as.numeric(data$event))
  
  fit_km <- survival::survfit(surv_object ~ data[[strata_var_]])
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

categorize_numeric_var <- function(strata_var, data) {
  if (strata_var == 'edad_ing1') {
    data[[paste0(strata_var, '_2')]] <- anyadir_edad_categorizada(data)
  } else if (strata_var == 'charlson') {
    data[[paste0(strata_var, '_2')]] <- anyadir_charlson_categorizado(data)
  } else{
    data[[paste0(strata_var, '_2')]] <- as.numeric(Hmisc::cut2(data[[strata_var]], g = 4))
  }
  return(data)
}

anyadir_edad_categorizada <- function(df) {
  df$edadcat <- NA
  df$edadcat[df$edad_ing1 < 55] <- "40-54"
  df$edadcat[df$edad_ing1 >= 55 & df$edad_ing1 < 65] <- "55-64"
  df$edadcat[df$edad_ing1 >= 65 & df$edad_ing1 < 75] <- "65-74"
  df$edadcat[df$edad_ing1 >= 75 & df$edad_ing1 < 85] <- "75-84"
  df$edadcat[df$edad_ing1 >= 85] <- "85+"
  return(df$edadcat)
}

anyadir_charlson_categorizado <- function(df) {
  df$charlsoncat <- NA
  df$charlsoncat[df$charlson == 1] <- "1"
  df$charlsoncat[df$charlson == 2] <- "2"
  df$charlsoncat[df$charlson == 3] <- "3"
  df$charlsoncat[df$charlson == 4] <- "4"
  df$charlsoncat[df$charlson >= 5] <- "5+"
  return(df$charlsoncat)
}

# comportamiento de la variable tiempodependiente

plot_td_behaviour <- function(
    td_var, 
    strata_var=NULL, 
    filtering=list(event = c(0, 1), strata_var = NULL), 
    plot_type='line_mean',
    adjust_ylim=NULL,
    title=NULL
) {
  
  data <- readRDS(paste0(DATAOUTPATH, data_for_event("MortOingIcc"), ".rds"))
  data <- discard_float_values_of_months(data)
  data <- apply_filter(filtering, data, strata_var)
  if (!is.null(strata_var)) {
    if (is.numeric(data[[strata_var]])) {
      data <- preprocess_strata_var(strata_var, data)
      strata_var <- paste0(strata_var, '_2')
    }
  }
  if (is.null(title)) {
    title <- "Comportamiento de la variable tiempo dependiente"
  }
  
  
  if (plot_type %in% c('line_mean', 'line_median')) {
    data <- f_group_by(data, c('month', strata_var)) %>% 
      summarise(
        mean = mean(.data[[td_var]]),
        median = median(.data[[td_var]])
      )
    if (plot_type == 'line_mean') {
      g <- ggplot(
          data = data, 
          mapping = aes_string(x = "month", y = "mean", color = strata_var)
        ) + 
        geom_line() + 
        theme_bw() +
        ylab(td_var) +
        scale_x_continuous("Mes", breaks = c(1:12)) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (plot_type == 'line_median') {
      g <- ggplot(
          data = data, 
          mapping = aes_string(x = "month", y = "median", color = strata_var)
        ) + 
        geom_line() + 
        theme_bw() +
        ylab(td_var) +
        scale_x_continuous("Mes", breaks = c(1:12)) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    if (!is.null(adjust_ylim)) {
      g <- g + ylim(adjust_ylim[1], adjust_ylim[2])
    }
    g
  } else if (plot_type == 'boxplot') {
    data$month <- factor(data$month)
    ggplot(
      data = data, 
      mapping = aes_string(x = "month", y = td_var, fill = strata_var)
    ) + 
      geom_boxplot() + 
      theme_bw() +
      ylab(td_var) +
      scale_x_discrete("Mes", breaks = c(1:12))
  }
  
}

discard_float_values_of_months <- function(data) {
  return(data[data$month %in% c(1:12), ])
}

apply_filter <- function(filtering, data, strata_var) {
  if (!is.null(strata_var)) {
    data <- apply_filter_stratavar(filtering, data, strata_var)
  }
  data <- apply_filter_event(filtering, data)
  return(data)
}

apply_filter_stratavar <- function(filtering, data, strata_var) {
  data <- renamecolumn_base(strata_var, 'strata_var', data)
  strata_var_accepted_vals <- get_strata_var_accepted_vals(filtering, data)
  data <- subset(
    data, strata_var %in% strata_var_accepted_vals
  )
  data <- renamecolumn_base('strata_var', strata_var, data)
  return(data)
}

apply_filter_event <- function(filtering, data) {
  data <- subset(
    data, event %in% filtering$event
  )
  return(data)
}

get_strata_var_accepted_vals <- function(filtering, data) {
  if (is.null(filtering$strata_var)) {
    return(unique(data$strata_var))
  } else{
    return(filtering$strata_var)
  }
}

preprocess_strata_var <- function(strata_var, data) {
  data <- categorize_numeric_var(strata_var, data)
  strata_var <- paste0(strata_var, '_2')
  data[[strata_var]] <- factor(data[[strata_var]])
  return(data)
}

# Distribución del evento
event_distribution <- function(event_var, plot_type="histograma") {
  data <- readRDS(paste0(DATAOUTPATH, data_for_event(event_var), ".rds"))
  data <- preprocess(data)
  
  if (plot_type == 'histograma') {
    ggplot(data, aes(x = time_to_event)) + 
      geom_histogram(colour = "black", fill = "white") +
      theme_bw() +
      scale_x_continuous("Mes", breaks = c(1:12)) +
      ylab('conteo') +
      ggtitle(paste("Distribución del evento", event_var)) +
      theme(plot.title = element_text(hjust = 0.5))
  } else if (plot_type == 'densidad') {
    ggplot(data, aes(x = time_to_event)) + 
      geom_density(colour = "black", fill = "#FF6666", alpha = .2) +
      theme_bw() +
      scale_x_continuous("Mes", breaks = c(1:12)) +
      ylab('conteo') +
      ggtitle(paste("Distribución del evento", event_var)) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
}

preprocess <- function(data) {
  data <- data[c('id', 'event', 'time_to_event')]
  data <- data[!rev(duplicated(rev(data$id))),]
  data <- subset(data, event == 1)
  print(dim(data))
  return(data)
}


# Get results -------------------------------------------------------------
filtering_ <- list(event = c(0), strata_var = NULL)

# Kaplan Meier plots
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "sexo")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "fe.reducida.severa")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "arm_prescribed_fechaalta")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "prescribediecaara2_fechaalta")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "bbloq_prescribed_fechaalta")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "denovo_ic_paciente")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "edad_ing1")
plot_kaplanmeier(event_var = "MortOingIcc", strata_var = "charlson")

# behaviour of dependent variable

## comportamiento de los covariables:
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = NULL, plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "sexo", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "fe.reducida.severa", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "arm_prescribed_fechaalta", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "prescribediecaara2_fechaalta", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "bbloq_prescribed_fechaalta", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "denovo_ic_paciente", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "edad_ing1", plot_type = 'line_mean')
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "charlson", plot_type = 'line_mean')

## comportamiento de los covariables entre los que sobreviven:
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = NULL, filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "sexo", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "fe.reducida.severa", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "arm_prescribed_fechaalta", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "prescribediecaara2_fechaalta", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "bbloq_prescribed_fechaalta", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "denovo_ic_paciente", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "edad_ing1", filtering = filtering_)
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "charlson", filtering = filtering_)


## Plots del acta 01042022
plot_td_behaviour(td_var = "perc_adh_bbloq", strata_var = "bbloq_prescribed_fechaalta")
plot_td_behaviour(td_var = "perc_adh_ieca", strata_var = "prescribediecaara2_fechaalta")
plot_td_behaviour(td_var = "perc_adh_arm", strata_var = "arm_prescribed_fechaalta")
plot_td_behaviour(td_var = "perc_adh_guia_arm", strata_var = "prescribedtoguia_fechaalta")

## Plots del paper (Sacados por Eduardo):
# parece ser que se aplicó un filter (¿queriendo? preguntar a Eduardo!)
plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = NULL,
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)
plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = NULL,
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)
plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = NULL,
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)

plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = "sexo",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)
plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = "sexo",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)

plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = "sexo",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)

plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = "fe.reducida.severa",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)

plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = "fe.reducida.severa",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)

plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = "fe.reducida.severa",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)

plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = "denovo_ic_paciente",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)

plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = "denovo_ic_paciente",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)

plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = "denovo_ic_paciente",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)

plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = "edad_ing1",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)

plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = "edad_ing1",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)

plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = "edad_ing1",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)

plot_td_behaviour(
  td_var = "perc_adh_bbloq",
  strata_var = "charlson",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Beta blockers"
)

plot_td_behaviour(
  td_var = "perc_adh_ara2oieca",
  strata_var = "charlson",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "ACEi/ARB"
)

plot_td_behaviour(
  td_var = "perc_adh_arm",
  strata_var = "charlson",
  plot_type = 'line_mean',
  adjust_ylim = c(0, 100),
  title = "Mineralocorticoid receptor antagonist (MRA)"
)


### si se quisieran hacer por separado:
plot_td_behaviour(
  td_var = "perc_adh_bbloq", 
  strata_var = "bbloq_prescribed_fechaalta",
  filtering = list(event = c(0, 1), strata_var = c(FALSE)),
  plot_type = 'line_mean'
)

# Distribución del evento
event_distribution(event_var = "MortOingIcc", plot_type = "histograma")
event_distribution(event_var = "MortOingIcc", plot_type = "densidad")
event_distribution(event_var = "fmort2", plot_type = "histograma")
event_distribution(event_var = "fmort2", plot_type = "densidad")


# Latent classes ----------------------------------------------------------
binarizar_variables <- function(variables_abinarizar, threshold) {
  for (variable_abinarizar in variables_abinarizar) {
    data[[paste0(variable_abinarizar, '_binarized')]] <- as.numeric(data[[variable_abinarizar]] >= threshold)
  }
  return(data)
}

event_var <- "MortOingIcc"
threshold <- 80
data <- readRDS(paste0(DATAOUTPATH, data_for_event(event_var), ".rds"))
data <- binarizar_variables(
  variables_abinarizar = c("perc_adh_ara2oieca", "perc_adh_arm", "perc_adh_bbloq", "perc_adh_guia_arm"),
  threshold
)


# Correlación -------------------------------------------------------------

library(corrplot)

data <- readRDS(paste0(DATAOUTPATH, data_for_event("MortOingIcc"), ".rds"))

# A través de la media de la adherencia: Las adherencias se agrupan con medias

data_covariables <- data %>% 
  select("id", "sexo", "fe.reducida.severa", 
         "arm_prescribed_fechaalta",
         "bbloq_prescribed_fechaalta", 
         "prescribediecaara2_fechaalta",
         "denovo_ic_paciente", 
         "edad_ing1", 
         "charlson") %>%
  group_by(id) %>% 
  filter(row_number() == 1)

data_adherences_summarised <- f_group_by(data, c('id')) %>% 
  summarise(
    mean_perc_adh_ara2oieca = mean(.data[['perc_adh_ara2oieca']]),
    mean_perc_adh_arm = mean(.data[['perc_adh_arm']]),
    mean_perc_adh_bbloq = mean(.data[['perc_adh_bbloq']])
  )

data_joined <- dplyr::inner_join(data_covariables, data_adherences_summarised, by = "id")

# para prescripciones y adherencias

data_joined_prescripionandadherence_vars <- data_joined[
  c(
    "mean_perc_adh_ara2oieca", "mean_perc_adh_arm", "mean_perc_adh_bbloq",
    "bbloq_prescribed_fechaalta", "arm_prescribed_fechaalta", "prescribediecaara2_fechaalta"
  )]

cor_M <- cor(data_joined_prescripionandadherence_vars, method = "spearman")

corrplot(cor_M, method = "pie")

# para resto de variables y adherencias

cols <- sapply(data_joined, is.logical)
data_joined[,cols] <- lapply(data_joined[,cols], as.numeric)

data_joined$sexo <- as.integer(data_joined$sexo)
data_joined$fe.reducida.severa <- as.integer(data_joined$fe.reducida.severa)

data_joined_restcovariablesandadherence_vars <- data_joined[
  c(
    "mean_perc_adh_ara2oieca", "mean_perc_adh_arm", "mean_perc_adh_bbloq",
    "sexo", "fe.reducida.severa", "denovo_ic_paciente", "edad_ing1", 
    "charlson"
  )]

cor_M <- cor(data_joined_restcovariablesandadherence_vars, method = "spearman")
corrplot(cor_M, method = "pie")


# A través del mes 3, 6 y 9 de la adherencia.

data_covariables <- data %>% 
  select("id", "sexo", "fe.reducida.severa", 
         "arm_prescribed_fechaalta",
         "bbloq_prescribed_fechaalta", 
         "prescribediecaara2_fechaalta",
         "denovo_ic_paciente", 
         "edad_ing1", 
         "charlson") %>%
  group_by(id) %>% 
  filter(row_number() == 1)

data_adherences_filtered <- 
  data %>% 
  filter(month %in% c(3, 6, 9)) %>% 
  select(id, month, perc_adh_ara2oieca, perc_adh_arm, perc_adh_bbloq) %>%
  group_by(id, month) %>%
  filter(row_number() == 1)

data_adherences_filtered <- data_adherences_filtered %>% pivot_wider(names_from = month, values_from = c(perc_adh_ara2oieca, perc_adh_bbloq, perc_adh_arm))

data_joined <- dplyr::inner_join(data_covariables, data_adherences_filtered, by = "id")

colnames(data_joined)

# para prescripciones y adherencias

data_joined_prescripionandadherence_vars <- data_joined[
  c(
    "perc_adh_ara2oieca_3", "perc_adh_ara2oieca_6", "perc_adh_ara2oieca_9",
    "perc_adh_bbloq_3", "perc_adh_bbloq_6", "perc_adh_bbloq_9",
    "perc_adh_arm_3", "perc_adh_arm_6", "perc_adh_arm_9",
    "bbloq_prescribed_fechaalta", "arm_prescribed_fechaalta", "prescribediecaara2_fechaalta"
  )]

cor_M <- cor(data_joined_prescripionandadherence_vars, method = "spearman", use = "pairwise.complete.obs")
# View(cor_M)
corrplot(cor_M, method = "pie")

# para resto de variables y adherencias

cols <- sapply(data_joined, is.logical)
data_joined[,cols] <- lapply(data_joined[,cols], as.numeric)

data_joined$sexo <- as.integer(data_joined$sexo)
data_joined$fe.reducida.severa <- as.integer(data_joined$fe.reducida.severa)

data_joined_restcovariablesandadherence_vars <- data_joined[
  c(
    "perc_adh_ara2oieca_3", "perc_adh_ara2oieca_6", "perc_adh_ara2oieca_9",
    "perc_adh_bbloq_3", "perc_adh_bbloq_6", "perc_adh_bbloq_9",
    "perc_adh_arm_3", "perc_adh_arm_6", "perc_adh_arm_9",
    "sexo", "fe.reducida.severa", "denovo_ic_paciente", "edad_ing1", 
    "charlson"
  )]

cor_M <- cor(data_joined_restcovariablesandadherence_vars, method = "spearman")
corrplot(cor_M, method = "pie")
