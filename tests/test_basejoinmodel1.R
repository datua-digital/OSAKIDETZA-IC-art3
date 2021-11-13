comprobar_coherencia_prescripciones_cortas <- function(base_join_model1_0){
  for (i in c(1:nrow(base_join_model1_0) )) {
    v <- collapsedstring_tovector(as.character(base_join_model1_0[i, c('days')]))
    if ((length(v) < 30) & (length(v) > as.numeric(base_join_model1_0[i, c('duration')]))) {
      print(i)
    }
  }
}

test_that("testear coherencia en prescripciones cortas", {
  expect_success(expect_output(comprobar_coherencia_prescripciones_cortas(base_join_model1_0), NA))
}
)
