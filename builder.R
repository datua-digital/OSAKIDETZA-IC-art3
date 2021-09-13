###################
# Data frame building for applying time-dependent survival models
# Initial data is in PROJECT_RDATA: baseJoinModel
# farmacos_traye data is merged to PROJECT_RDATA
# Data organized in monthly chunks
# Time varying variables are added



# load libraries ----------------------------------------------------------

rm( list = ls() )
library(constructedBases) # it needs to be installed constructedBases_0.1.1.tar.gz


# global environment variables --------------------------------------------

# path and files parameters
DATA_IN_PATH = 'data/in/'
DATA_OUT_PATH = 'data/out/'
BUILDING_SCRIPTS_PATH = 'building/'
PROJECT_RDATA = 'base_joinModel_artic3.rda.RData'

# case identification parameters
PROJECT_DRUGS = c('ara2', 'ieca', 'bbloq')# , 'arm', 'ado')
FOLLOW_UP = 365
EARLY_DEATH_PATIENT_DAYS = 30
FILTER_DENOVO_IC_PATIENTS = TRUE
FILTER_DENOVO_TT_PATIENTS = TRUE


# load sources ------------------------------------------------------------

source(paste0(BUILDING_SCRIPTS_PATH, 'preprocessing.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'case_identification.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'baseJoinModel_farmaco_fusion.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'monthly_arrangement.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'time_varying_covariates.R'), encoding = 'UTF-8')


# load data ---------------------------------------------------------------
load(paste0(DATA_IN_PATH, PROJECT_RDATA))
df_farmacos <- constructedBases::farmacos_traye


# preprocessing -----------------------------------------------------------

# preprocess baseJoinModel
baseJoinModel_0 <- preprocess_baseJoinModel(baseJoinModel)

# preprocess farmacos
df_farmacos <- preprocess_farmacos(df_farmacos, PROJECT_DRUGS)


# Case identification -----------------------------------------------------
baseJoinModel_1 <- case_identification(baseJoinModel_0, EARLY_DEATH_PATIENT_DAYS, PROJECT_DRUGS)
saveRDS(baseJoinModel_1, paste0(DATA_OUT_PATH, 'data_after_case_identification.rds'))

# df_farmacos and baseJoinModel fusion ------------------------------------
baseJoinModel_2 <- merge_farmacos(baseJoinModel_1, df_farmacos)
baseJoinModel_3 <- process_baseJoinModel1(baseJoinModel_2, duration=FOLLOW_UP)
saveRDS(baseJoinModel_3,  paste0(DATA_OUT_PATH, 'baseJoinModel_and_famrmacos.rds'))


# Data organized in monthly chunks ----------------------------------------
baseJoinModel1_0 <- rearranged_in_months(baseJoinModel_3)
saveRDS(baseJoinModel1_0, paste0(DATA_OUT_PATH, 'baseJoinModel_afterMonthlyRearrangement.rds'))


# df with time varying variables added ------------------------------------
baseJoinModel2_0 <- adherencia_farmacos(baseJoinModel1_0)
baseJoinModel2_1 <- adherencia_farmacos_guia(baseJoinModel1_0)
baseJoinModel2_2 <- adherencia_farmacos_medico(baseJoinModel1_0)
baseJoinModel2_3 <- merge_timevarying_vars(baseJoinModel2_0, baseJoinModel2_1, baseJoinModel2_2)
saveRDS(baseJoinModel2_3, paste0(DATA_OUT_PATH, 'baseJoinModel_afterTimevarying_vars.rds'))

write.csv(baseJoinModel2_3, paste0(DATA_OUT_PATH, 'df_JM.csv'))
