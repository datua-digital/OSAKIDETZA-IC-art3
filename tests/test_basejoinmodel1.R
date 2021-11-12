source("utils/df_utils.R")
DATAOUTPATH <- "data/out/"
base_join_model1_0 <- readRDS(paste0(DATAOUTPATH, "baseJoinModel_afterMonthlyRearrangement.rds"))


for (i in c(1:nrow(base_join_model1_0) )) {
  v <- collapsedstring_tovector(as.character(base_join_model1_0[i, c('days')]))
  if ((length(v) < 30) & (length(v) > as.numeric(base_join_model1_0[i, c('duration')]))) {
    print(i)
  }
}
