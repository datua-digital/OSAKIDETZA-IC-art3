###################
# Data frame building for applying time-dependent survival models
# Initial data is in PROJECTRDATA: baseJoinModel
# farmacos_traye data is merged to PROJECTRDATA
# Data organized in monthly chunks
# Time varying variables are added


# load libraries ----------------------------------------------------------
rm(list = ls())
library(constructedBases) # it needs to be installed constructedBases_0.1.1.tar.gz
library(lintr)

# builder variables --------------------------------------------

# path and files parameters
PROJECTRDATA <- "base_joinModel_artic3.rda.RData"
CHARLSONDATA <- "IndiceCharlson.rda"

# case identification parameters
PROJECTDRUGS <- c("ara2", "ieca", "bbloq", "arm")# , "ado")
FOLLOWUP <- 365
EARLYDEATHPATIENTDAYS <- 30
EVENT <- "MortOingIcc"

# load sources ------------------------------------------------------------
source(paste("src", "configuration.R", sep = "/"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "preprocessing.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "postprocessing.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "case_identification.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "prescribed_adherence_drugs_in_fechaalta.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "baseJoinModel_farmaco_fusion.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "monthly_arrangement.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "time_varying_covariates.R"), encoding = "UTF-8")
source(paste0(BUILDINGSCRIPTSPATH, "input_months.R"), encoding = "UTF-8")

# load data ---------------------------------------------------------------
load(paste0(DATAINPATH, PROJECTRDATA))
load(paste0(DATAINPATH, CHARLSONDATA))
df_farmacos <- constructedBases::farmacos_traye


# preprocessing -----------------------------------------------------------

# preprocess baseJoinModel
base_join_model_0 <- preprocess_base_join_model(baseJoinModel)
base_join_model_01 <- get_guia_prescribed_infechaalta(df = base_join_model_0, drugs = PROJECTDRUGS)
base_join_model_02 <- get_guia_adherenced_infechaalta(df = base_join_model_01, drugs = PROJECTDRUGS)

# preprocess farmacos
df_farmacos <- preprocess_farmacos(df_farmacos, PROJECTDRUGS)

# preprocess basal_ch
basal_ch <- preprocess_basal_ch(basal_ch)

# Case identification -----------------------------------------------------
base_join_model_1 <- case_identification(base_join_model_02, EARLYDEATHPATIENTDAYS, PROJECTDRUGS)
saveRDS(base_join_model_1, paste0(DATAOUTPATH, "data_after_case_identification.rds"))

# df_farmacos, base_join_model and Charlson index fusion ------------------------------------
base_join_model_merged_0 <- merge_byid(base_join_model_1, df_farmacos)
base_join_model_merged_1 <- merge_byid(base_join_model_merged_0, basal_ch)
base_join_model_2 <- process_base_join_model(base_join_model_merged_1, duration = FOLLOWUP, event = EVENT)
saveRDS(base_join_model_2,  paste0(DATAOUTPATH, "baseJoinModel_and_farmacos.rds"))


# Data organized in monthly chunks ----------------------------------------
base_join_model1_0 <- rearranged_in_months(base_join_model_2)
saveRDS(base_join_model1_0, paste0(DATAOUTPATH, "baseJoinModel_afterMonthlyRearrangement.rds"))

# df with time varying variables added ------------------------------------

base_join_model2_0 <- adherencia_farmacos(base_join_model1_0)
base_join_model2_1 <- adherencia_farmacos_guia(base_join_model1_0)
base_join_model2_2 <- adherencia_farmacos_medico(base_join_model1_0)
base_join_model2_3 <- adherencia_farmacos_ara2ieca(base_join_model1_0)
base_join_model2_4 <- adherencia_farmacos_guia_arm(base_join_model1_0)
base_join_model2_5 <- merge_timevarying_vars(
  base_join_model2_0,
  base_join_model2_1,
  base_join_model2_2,
  base_join_model2_3,
  base_join_model2_4
)
# saveRDS(base_join_model2_5, paste0(DATAOUTPATH, "baseJoinModel_afterTimevarying_vars.rds"))

# Input final df ------------------------------------
base_join_model2_6 <- input_patients_noprescriptions(base_join_model2_5, base_join_model_1)
base_join_model2_7 <- input_patients_noiniprescriptions(base_join_model2_6)
base_join_model2_8 <- input_patients_nofinprescriptions(base_join_model2_7, FOLLOWUP)
base_join_model2_9 <- input_patients_nointerprescriptions(base_join_model2_8)
base_join_model2_10 <- input_adhvars(base_join_model2_9)
base_join_model2_11 <- reset_timeevent_vars(base_join_model2_10, EVENT)

# calcular adherencias acumuladas en meses ------------------------------------
base_join_model3 <- acum_month(base_join_model2_11)

# postprocessing:
base_join_model3 <- postprocessing(base_join_model3)

# TODO: Mejor que sea un .rds, seguramente evitará problemas y guardará mejor el tipo de datos
saveRDS(base_join_model3, paste0(DATAOUTPATH, "df_JM", "_", EVENT, ".rds"))
