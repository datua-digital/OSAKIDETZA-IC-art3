# plot to compare AUC JM y Cox
auc_jm <- c(0.7033, 0.6999, 0.6943, 0.6929, 0.6836, 0.6917, 0.6899, 0.6758, 0.6579, 0.6784, 0.6349)
auc_casero_data <- data.frame(
  auc_value = c(round(auc_cox, 3), round(auc_jm, 3)), 
  auc_type = factor(c(rep('auc_cox', 11), rep('auc_jm', 11))), 
  auc_range = c('1-12', '2-12', '3-12', '4-12', '5-12', '6-12', '7-12', '8-12', '9-12', '10-12', '11-12'),
  auc_range2 = c(c(1:11), c(1:11))
)

ggplotly(
  ggplot(auc_casero_data, mapping = aes(x = auc_range2, y = auc_value, color = auc_type)) + 
    geom_line() + 
    geom_point() + scale_x_continuous(limits = c(1, 12)) +
    theme_bw() +
    ylim(0.5, 1) + 
    xlab('Comparación enre AUCs comparativos')
)


auc_casero_data_ <- data.frame(
  auc_value = c(round(auc_cox_, 3), round(auc_jm2, 3)), 
  auc_type = factor(c(rep('auc_cox', 11), rep('auc_jm', 11))), 
  auc_range = c('1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', '9-10', '10-11', '11-12'),
  auc_range2 = c(c(1:11), c(1:11))
)

ggplotly(
  ggplot(auc_casero_data_, mapping = aes(x = auc_range2, y = auc_value, color = auc_type)) + 
    geom_line() + 
    geom_point() + scale_x_continuous(limits = c(1, 12)) +
    theme_bw() +
    ylim(0.5, 1) + 
    xlab('Comparación enre AUCs comparativos')
)

mean(auc_cox_)
mean(auc_jm2)