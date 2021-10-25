library(data.table)
library(ggplot2)
library(JMbayes)
library(splines)
library(tidyverse)


OUTPATH <- "out/"
DATAINPATH <- "data/in/"
LONGVAR <- "cum_perc_adh_guia"
VARIABLESCOX_IND <- c("sexo", "edad_ing1")
VARIABLESCOX <- c("sexo", "edad_ing1", "cluster(id)")
VARIABLESTODOS <- c("id", VARIABLESCOX_IND, "event", "time_to_event", "month", "cum_perc_adh_ara2", "last_month")

M1$Data$data$event
ndata <- read.csv(paste(DATAINPATH, 'mockdata.csv', sep = '/'))
ndata$event <- as.numeric(ndata$event)


output <- 'JM_reducido3'

sfit.1 <- survfitJM(M1, newdata = subset(ndata, id == 'A'))
sfit.2 <- survfitJM(M3, newdata = subset(ndata, id == '2'), idVar = 'id')

myJMsurvfit(sfit.A, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Cumulative months at time t', main = "Patient A - Model 2",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)
myJMsurvfit(sfit.B, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Cumulative months at time t', main = "Patient B - Model 2",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)

pbc2
## Not run: 
# we construct the composite event indicator (transplantation or death)
pbc2$status2 <- as.numeric(pbc2$status != "alive")
pbc2.id$status2 <- as.numeric(pbc2.id$status != "alive")

# we fit the joint model using splines for the subject-specific 
# longitudinal trajectories and a spline-approximated baseline
# risk function
lmeFit <- lme(log(serBilir) ~ ns(year, 2), data = pbc2,
              random = ~ ns(year, 2) | id)
survFit <- coxph(Surv(years, status2) ~ drug, data = pbc2.id, x = TRUE)
jointFit <- jointModelBayes(lmeFit, survFit, timeVar = "year")

# we will compute survival probabilities for Subject 2 in a dynamic manner, 
# i.e., after each longitudinal measurement is recorded
ND2 <- pbc2[pbc2$id == 2, ] # the data of Subject 2

survPreds <- vector("list", nrow(ND))
for (i in 1:nrow(ND)) {
  survPreds[[i]] <- survfitJM(jointFit, newdata = ND)
}
survPreds




# mmy example
VARIABLESCOX <- c("sexo", "edad_ing1")
VARIABLESLONGS <- c("cum_perc_adh_ara2", "cum_perc_adh_bbloq", "cum_perc_adh_ieca", "cum_perc_adh_doctor", "cum_perc_adh_guia")
VARIABLESTODOS <- c("id", VARIABLESCOX, "status", "years", "year")
source("utils/jm_utils.R")
source("utils/table_utils.R")
df_jm <- readr::read_csv("data/out/df_JM2.csv")
patients_conditions <- list(
  denovo_ic_paciente = TRUE,
  denovo_tt_paciente_fing = TRUE,
  denovo_tt_paciente_falta = NULL,
  early_death_patient_30 = FALSE,
  patient_with_prescription = TRUE
)
OUTPATH <- "out/"
LONGVAR <- "cum_perc_adh_guia"
output <- 'test'
# choose patients
df_jm$event <- as.numeric(df_jm$event)
df_jm$id <- factor(df_jm$id)
df_jm$year <- df_jm$month
df_jm$years <- df_jm$time_to_event
df_jm$status <- df_jm$event
df_jm <- as.data.frame(df_jm)
df_jm <- filter_patients(df_jm, patients_conditions)
# build data ---------------------------------------------------------------

cox_df <- generate_coxdf(df_jm)
df_jm <- preprocess_dfjm(df_jm)

# Modelización de la variable longitudinal y el evento---------------------------------------------------------------
## long_proc <- longitudinal_process(LONGVAR = LONGVAR, data_ = df_jm, tipo = "splines_cubicas")

long_proc <- nlme::lme(
  cum_perc_adh_guia~ns(year, 4),
  random = ~ ns(year, 4) | id,
  data = df_jm,
  control = lmeControl(opt = 'optim')
)

survFit <- coxph(Surv(years, status) ~ sexo + edad_ing1,  data = cox_df, x = TRUE, model = TRUE)

# Modelización del proceso del evento ---------------------------------------------------------------
# M1: Fit JM with longitudinal process (4) and event process (6)
summary(M1)
M1 <- JMbayes::jointModelBayes(
  long_proc,
  survFit,
  timeVar = "year",
  n.iter = 30000,
  n.burnin = 3000)

ND<- df_jm[c(1:7, 13:19),]
ND$cum_perc_adh_guia <- c(1:7, rep(0,3), rep(1, 4))


sfit.A <- survfitJM(M1, newdata = ND)
sfit.B <- survfitJM(jointFit, newdata = ND2[c('serBilir', 'id', 'year', 'status', 'drug')])






myJMsurvfit(sfit.A, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 12),
            xlab = 'Time [months]', ylab = '',
            ylab2 = 'meses acumulados siendo adherente', main = "",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)
myJMsurvfit(sfit.B, estimator = "mean", include.y = TRUE, conf.int = TRUE,
            fill.area = TRUE, col.area = "lightgrey", xlim = c(0, 60),
            xlab = 'Time [months]', ylab = 'Survival Probability',
            ylab2 = 'Cumulative months at time t', main = "Patient B - Model 2",
            ymin = 0, ylim.y = c(0, 12), col = 'black',
            cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.05,
            cex.lab.z = 1.2, cex.axis.z = 1.05)

