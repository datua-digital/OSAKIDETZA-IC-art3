calcular_auc_ <- function(scores, actuals, plot_roc = TRUE) {
  actuals <- actuals[order(scores)]
  sens <- (sum(actuals) - cumsum(actuals))/sum(actuals)
  spec <- cumsum(!actuals) / sum(!actuals)
  auc <- sum(spec * diff(c(0, 1 - sens)))
  
  if (plot_roc) {
    plot(1 - spec, sens, type = "l", col = "red", 
         ylab = "Sensitivity", xlab = "1 - Specificity")
    abline(c(0,0),c(1,1))
  }
  
  return(auc)
}