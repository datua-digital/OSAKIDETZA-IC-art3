library(nlme)

longitudinal_process <- function(variable, data_, tipo = 'splines_cubicas') {
  if (tipo == 'splines_cubicas') {
    long_process <- nlme::lme(as.formula(paste(variable, paste('ns(month, 4)', collapse = '+'), sep = '~')),
                              random = ~ ns(month, 4) | id,
                              data = data_,
                              control = lmeControl(opt = 'optim'))
  }
}

# plot_longitudinal_process <- function(data_, id, variable )
# long_ids <- names(which(table(df_td$id) > 6))
# ids <- sample(long_ids, 16)
# xyplot(as.formula(paste(variable_longitudinal, paste('dura_acumulada | id', collapse = '+'), sep='~')),
#        data = df_td, 
#        subset = id %in% ids, type = c("p", "smooth"), 
#        lwd = 2, layout = c(4, 4))