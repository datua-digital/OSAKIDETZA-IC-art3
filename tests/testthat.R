library(testthat)
# source("tests/test_jmdf.R")
# source("tests/test_basejoinmodel1.R")
source("utils/df_utils.R")

DATAOUTPATH <- "data/out"
df_jm <- readr::read_csv(paste(DATAOUTPATH, 'df_JM.csv', sep = '/'))
base_join_model1_0 <- readRDS(paste(DATAOUTPATH, 'baseJoinModel_afterMonthlyRearrangement.rds', sep = '/'))

# ***************************************************************************

test_file("tests/test_jmdf.R")
test_file("tests/test_basejoinmodel1.R")

# ***************************************************************************
