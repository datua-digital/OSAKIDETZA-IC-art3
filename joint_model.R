# load libraries ----------------------------------------------------------
# 
library(JM)
library(JMbayes)
library(readr)
library(nlme)
library(tidyverse)
# global environment variables --------------------------------------------
OUTPATH = 'out/'



# load sources ------------------------------------------------------------
source('utils/jm_utils.R')
source('utils/table_utils.R')


# Selección de variables ---------------------------------------------------------------
VARIABLESCOX_IND <- c('sexo', 'edad_ing1')
VARIABLESCOX <- c('sexo', 'edad_ing1', 'cluster(id)')
VARIABLESTOTAL <- c('id', VARIABLESCOX_IND, 'event', 'time_to_event', 'month', 'cum_perc_adh_ara2', 'last_month')
DATAOUTPATH <- 'data/out/'

# load data ---------------------------------------------------------------
# 

df_jm <- readr::read_csv('data/out/df_JM.csv')
df_jm$id <- as.numeric(df_jm$id)
# df_jm <- df_jm %>% mutate(time_to_event2 = if_else(event == 1, time_to_event, 12.01))

cox_df <- df_jm[!duplicated(df_jm$id), ]
cox_df$id <- as.numeric(cox_df$id)
df_jm <- df_jm[VARIABLESTOTAL]
cox_df <- cox_df[VARIABLESTOTAL]
df_jm <- df_jm %>% dplyr::arrange(id, month)
cox_df <- cox_df %>% dplyr::arrange(id, month)

# Modelización de la variable longitudinal y el evento---------------------------------------------------------------
# 



# long_proc_cum_perc_adh_ara2 <- longitudinal_process(variable = 'cum_perc_adh_ara2',
#                                                     data = df_jm,
#                                                     tipo = 'splines_cubicas')

long_proc_cum_perc_adh_ara2 <- nlme::lme(as.formula(paste('cum_perc_adh_ara2', paste('ns(month, 4)', collapse = '+'), sep = '~')),
                          random = ~ ns(month, 4) | id,
                          data = df_jm,
                          control = lmeControl(opt = 'optim'))
surv_object <- Surv(time = cox_df$time_to_event,
                    event = as.numeric(cox_df$event))

coxFit.df_jm <- coxph(as.formula(paste('surv_object', paste(VARIABLESCOX, collapse = '+'), sep = '~')), 
                      data = cox_df,
                      x = TRUE,
                      model = TRUE)

# Modelización del proceso del evento ---------------------------------------------------------------
# M1: Fit JM with longitudinal process (4) and event process (6)
M1 <- JMbayes::jointModelBayes(long_proc_cum_perc_adh_ara2, 
                               coxFit.df_jm, 
                               timeVar = "month",
                               n.iter = 30000, 
                               n.burnin = 3000)
saveRDS(M1, paste0(DATAOUTPATH, 'JM_1_ara2.rds'))


# M2: Fit JM with longitudinal process (4) y componentes de tendencia y valor actuales
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4), 
              indFixed = 2:5, indRandom = 2:5)
M2 <- update(M1, param = 'td-both', extraForm = dForm)
saveRDS(M2, paste0(DATAOUTPATH, 'JM_2_ara2.rds'))
summary(M2)

# M3: Fit JM with longitudinal process (4) y componente de tendencia
dForm <- list(fixed = ~ 0 + dns(month, 4), random = ~ 0 + dns(month, 4), 
              indFixed = 2:5, indRandom = 2:5)
M3 <- update(M1, param = 'td-extra', extraForm = dForm)
saveRDS(M3, paste0(DATAOUTPATH, 'JM_3_ara2.rds'))




# Guardar resultados ---------------------------------------------------------------

JM_table <- summary_table(M1, M2, M3)

