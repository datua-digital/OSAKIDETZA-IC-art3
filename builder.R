###################
# Data frame building for applying time-dependent survival models
# Initial data is in PROJECT_RDATA: baseJoinModel
# farmacos_traye data is merged to PROJECT_RDATA
# Data organized in monthly chunks
# Time varying variables are added


# load libraries
rm( list = ls() )
library(constructedBases) # it needs to be installed constructedBases_0.1.1.tar.gz

# global environment variables
DATA_IN_PATH = 'data/in/'
BUILDING_SCRIPTS_PATH = 'building/'
PROJECT_RDATA = 'base_joinModel_artic3.rda.RData'
PROJECT_DRUGS = c('ara2', 'ieca', 'bbloq')
FOLLOW_UP = 365

# load sources
source(paste0(BUILDING_SCRIPTS_PATH, 'baseJoinModel_farmaco_fusion.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'montly_arrangement.R'), encoding = 'UTF-8')
source(paste0(BUILDING_SCRIPTS_PATH, 'time_varying_covariates.R'), encoding = 'UTF-8')


# load data
load(paste0(DATA_IN_PATH, PROJECT_RDATA))
df_farmacos <- constructedBases::farmacos_traye


# df_farmacos and baseJoinModel fusion
baseJoinModel_0 <- preprocess_baseJoinModel(baseJoinModel)
df_farmacos <- preprocess_farmacos(df_farmacos, PROJECT_DRUGS)
baseJoinModel_1 <- merge_farmacos(baseJoinModel_0, df_farmacos)
baseJoinModel_2 <- process_baseJoinModel1(baseJoinModel_1, duration=FOLLOW_UP)


# Data organized in monthly chunks
baseJoinModel1_0 <- rearranged_in_months(baseJoinModel_2)


# df with time varying variables added
baseJoinModel3_0 <- timevarying1(baseJoinModel2)
baseJoinModel3_1 <- timevarying2(baseJoinModel2_0)
baseJoinModel3_2 <- timevarying2(baseJoinModel2_0)
