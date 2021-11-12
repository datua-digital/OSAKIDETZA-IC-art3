library(testthat)
source("tests/test_jmdf.R")
source("tests/test_basejoinmodel1.R")

df_jm <- readr::read_csv("data/out/df_JM.csv")
base_join_model1_0 <- readRDS(paste0(DATAOUTPATH, "baseJoinModel_afterMonthlyRearrangement.rds"))

# ***************************************************************************

show_failure(expect_output(comprobar_valores(), NA))

test_that("test de prueba",{
  #expect_success(expect_output(hola(), "Hola"))
  expect_success(expect_output(comprobar_length_duration(), NA))
  expect_success(expect_output(comprobar_valores_rango(), NA))
  expect_success(expect_output(comprobar_valores_rango(), NA))
  expect_success(expect_output(comprobar_valores_perc(), NA))
})

# ***************************************************************************