


summary_table <- function(m1, m2, m3) {
  # Function for the generation of Table 3 of the manuscript.
  #-------------#
  # Left column #
  #-------------#
  value <- rbind(t(t(m1$postMeans$betas)),
                 t(t(m1$postMeans$sigma)),
                 t(t(m1$postMeans$gammas)),
                 t(t(m1$postMeans$alphas)),
                 NA)
  lower <- rbind(t(t(m1$CIs$betas[1, ])),
                 t(t(m1$CIs$sigma[1, ])),
                 t(t(m1$CIs$gammas[1, ])),
                 t(t(m1$CIs$alphas[1, ])),
                 NA)
  upper <- rbind(t(t(m1$CIs$betas[2, ])),
                 t(t(m1$CIs$sigma[2, ])),
                 t(t(m1$CIs$gammas[2, ])),
                 t(t(m1$CIs$alphas[2, ])),
                 NA)
  intervals <- cbind(value, lower, upper)
  colnames(intervals) <- c("value", "lower", "upper")
  CI <- NULL
  for (i in 1:10) {
    CI <- c(CI, paste0("(", round(lower[i], 4), ";", round(upper[i], 4), ")"))
  }
  Tm1 <- data.frame("Mean" = round(value, 4), CI)
  colnames(Tm1) <- c("Mean", "95% CI")
  rownames(Tm1) <- c("Intercept", "$B_n(t,lambda_1)$", "$B_n(t,lambda_2)$",
                     "$B_n(t,lambda_3)$", "$B_n(t,lambda_4)$", "sigma_eps",
                     "Edad", "Sexo", "Current Value", "Slope")
  #----------------#
  # Central column #
  #----------------#
  value <- rbind(t(t(m2$postMeans$betas)),
                 t(t(m2$postMeans$sigma)),
                 t(t(m2$postMeans$gammas)),
                 t(t(m2$postMeans$alphas)),
                 t(t(m2$postMeans$Dalphas)))
  lower <- rbind(t(t(m2$CIs$betas[1, ])),
                 t(t(m2$CIs$sigma[1, ])),
                 t(t(m2$CIs$gammas[1, ])),
                 t(t(m2$CIs$alphas[1, ])),
                 t(t(m2$CIs$Dalphas[1, ])))
  upper <- rbind(t(t(m2$CIs$betas[2, ])),
                 t(t(m2$CIs$sigma[2, ])),
                 t(t(m2$CIs$gammas[2, ])),
                 t(t(m2$CIs$alphas[2, ])),
                 t(t(m2$CIs$Dalphas[2, ])))
  intervals <- cbind(value, lower, upper)
  colnames(intervals) <- c("value", "lower", "upper")
  CI <- NULL
  for (i in 1:10) {
    CI <- c(CI, paste0("(", round(lower[i], 4), ";", round(upper[i], 4), ")"))
  }
  Tm2 <- data.frame("Mean" = round(value, 4), CI)
  colnames(Tm2) <- c("Mean", "95% CI")
  rownames(Tm2) <- c("Intercept", "$B_n(t,lambda_1)$", "$B_n(t,lambda_2)$",
                     "$B_n(t,lambda_3)$", "$B_n(t,lambda_4)$", "sigma_eps",
                     "Edad", "Sexo", "Current Value", "Slope")
  #--------------#
  # Right column #
  #--------------#
  value <- rbind(t(t(m3$postMeans$betas)),
                 t(t(m3$postMeans$sigma)),
                 t(t(m3$postMeans$gammas)),
                 NA,
                 t(t(m3$postMeans$Dalphas)))
  lower <- rbind(t(t(m3$CIs$betas[1, ])),
                 t(t(m3$CIs$sigma[1, ])),
                 t(t(m3$CIs$gammas[1, ])),
                 NA,
                 t(t(m3$CIs$Dalphas[1, ])))
  upper <- rbind(t(t(m3$CIs$betas[2, ])),
                 t(t(m3$CIs$sigma[2, ])),
                 t(t(m3$CIs$gammas[2, ])),
                 NA,
                 t(t(m3$CIs$Dalphas[2, ])))
  intervals <- cbind(value, lower, upper)
  colnames(intervals) <- c("value", "lower", "upper")
  CI <- NULL
  for (i in 1:10) {
    CI <- c(CI, paste0("(", round(lower[i], 4), ";", round(upper[i], 4), ")"))
  }
  Tm3 <- data.frame("Mean" = round(value, 4), CI)
  colnames(Tm3) <- c("Mean", "95% CI")
  rownames(Tm3) <- c("Intercept", "$B_n(t,lambda_1)$", "$B_n(t,lambda_2)$",
                     "$B_n(t,lambda_3)$", "$B_n(t,lambda_4)$", "sigma_eps",
                     "Edad", "Sexo", "Current Value", "Slope")
  #---------------#
  # summary table #
  #---------------#
  summaryJM <- cbind(Tm1, Tm2, Tm3)
  DIC <- c(m1$DIC, m2$DIC, m3$DIC)
  names(DIC) <- c("m1", "m2", "m3")
  return(list("summaryJM" = summaryJM, "DIC" = DIC))
}