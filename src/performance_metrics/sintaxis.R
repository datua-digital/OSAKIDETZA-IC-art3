library(readxl)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
art3 <-
  read_excel(path = "./resultados_auc_predictionerror.xlsx", sheet = 1)
getwd()
art3 <- read.csv("./src/resultados_auc_predictionerror.csv")
colnames(art3) <- c("month", "Cox", "M1", "M2", "measure", "tramo")
summary(art3)


art3$Cox <- as.numeric(sub(".", ".", art3$Cox, fixed = TRUE))
art3$M1 <- as.numeric(sub(".", ".", art3$M1, fixed = TRUE))
art3$M2 <- as.numeric(sub(".", ".", art3$M2, fixed = TRUE))
art3$measure <- as.factor(art3$measure)
art3$tramo <- as.factor(art3$tramo)

# vemos tendencia de AUC

# modelo que compara auc en lo que queda de tiempo de seguimiento
class(art3)
art3 <- as.data.frame(art3)

art3 %>%  filter(measure == "auc")

##hago regresion y tambi?n har? gr?fica

# wide to long
library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
auc1 <-
  art3 %>% filter(measure == "auc" &
                    tramo == "todo") %>% pivot_longer(
                      cols = c('Cox', 'M1', 'M2'),
                      names_to = 'model',
                      values_to = 'auc'
                    )
auc1.r <- lm(auc ~ model + month, data = auc1)
summary(auc1.r)

auc1.rx <- lm(auc ~ model + month + model:month, data = auc1)
summary(auc1.rx)
confint(auc1.rx)



plot_auc1 <- ggplot(auc1, aes(x = month, y = auc, group = model)) +
  geom_point(aes(color = model), size = 3) +
  scale_x_continuous("Months from hospital discharge",
                     breaks = auc1$month,
                     limits = c(1, 11)) +
  scale_y_continuous("AUC", limits = c(0.5, 0.75))


auc1$pred_auc1.rx = predict(auc1.rx, auc1)
plot <-
  plot_auc1 + geom_line(data = auc1,
                        aes(y = pred_auc1.rx, color = model),
                        size = 3) +
  theme(axis.text.x = element_text(size = 25)) +
  theme(axis.text.y = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(legend.title = element_text(size = 25)) +
  theme(legend.text = element_text(size = 25)) +
  theme_bw()


# tabla con coeficientes
tabla.auc1 <- as.data.frame(cbind(coef(auc1.rx), confint(auc1.rx)))
tabla.auc1$variables <- row.names(tabla.auc1)
colnames(tabla.auc1) <-
  c("coefficient", "2.5 %"  ,   "97.5 %"   , "variables")
rownames(tabla.auc1) <- NULL
tabla.auc1 <- (tabla.auc1[, c(4, 1:3)])
summary(tabla.auc1)
# redondeo
tabla.auc1$coefficient <- round(tabla.auc1$coefficient, 3)
tabla.auc1$"2.5 %" <- round(tabla.auc1$"2.5 %", 3)
tabla.auc1$"97.5 %" <- round(tabla.auc1$"97.5 %", 3)

tabla.auc1a <-
  tableGrob(tabla.auc1, theme = ttheme_minimal(base_size = 15))
# grid.draw(tabla.auc1a)
##png grafica y modelo
library(grid)
library(gridExtra)
grid.arrange(plot,
             tabla.auc1a,
             # heights = c(0.8, 0.2),
             # widths = c(0.5,0.5),
             ncol = 2)

##################################
##################################

### 1 month prediction
auc2 <-
  art3 %>% filter(measure == "auc" &
                    tramo == "mes") %>% pivot_longer(
                      cols = c('Cox', 'M1', 'M2'),
                      names_to =
                        'model',
                      values_to =
                        'auc'
                    )
auc2.r <- lm(auc ~ model + month, data = auc2)
summary(auc2.r)

auc2.rx <- lm(auc ~ model + month + model:month, data = auc2)
summary(auc2.rx)
confint(auc2.rx)


plot_auc2 <- ggplot(auc2, aes(x = month, y = auc, group = model)) +
  geom_point(aes(color = model), size = 3) +
  scale_x_continuous("Months from hospital discharge",
                     breaks = auc2$month,
                     limits = c(1, 11)) +
  scale_y_continuous("AUC", limits = c(0.5, 0.75))


auc2$pred_auc2.rx = predict(auc2.rx, auc2)
plot <-
  plot_auc2 + geom_line(data = auc2,
                        aes(y = pred_auc2.rx, color = model),
                        size = 3) +
  theme(axis.text.x = element_text(size = 25)) +
  theme(axis.text.y = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(legend.title = element_text(size = 25)) +
  theme(legend.text = element_text(size = 25)) + 
  theme_bw()


#tabla con coeficientes
tabla.auc2 <- as.data.frame(cbind(coef(auc2.rx), confint(auc2.rx)))
tabla.auc2$variables <- row.names(tabla.auc2)
colnames(tabla.auc2) <-
  c("coefficient", "2.5 %"  ,   "97.5 %"   , "variables")
rownames(tabla.auc2) <- NULL
tabla.auc2 <- (tabla.auc2[, c(4, 1:3)])
summary(tabla.auc2)
#redondeo
tabla.auc2$coefficient <- round(tabla.auc2$coefficient, 3)
tabla.auc2$"2.5 %" <- round(tabla.auc2$"2.5 %", 3)
tabla.auc2$"97.5 %" <- round(tabla.auc2$"97.5 %", 3)

tabla.auc2a <-
  tableGrob(tabla.auc2, theme = ttheme_minimal(base_size = 15))
#grid.draw(tabla.auc2a)
##png grafica y modelo
library(grid)
library(gridExtra)
grid.arrange(plot,
             tabla.auc2a,
             # heights = c(0.8, 0.2),
             ncol = 2)


###prediction error remaining time
perror1 <-
  art3 %>% filter(measure == "perror" &
                    tramo == "todo") %>% pivot_longer(
                      cols = c('Cox', 'M1', 'M2'),
                      names_to =
                        'model',
                      values_to =
                        'perror'
                    )
perror1.r <- lm(perror ~ model + month, data = perror1)
summary(perror1.r)

perror1.rx <- lm(perror ~ model + month + model:month, data = perror1)
summary(perror1.rx)
confint(perror1.rx)


plot_perror1 <- ggplot(perror1, aes(x = month, y = perror, group = model)) +
  geom_point(aes(color = model), size = 3) + ylim(0.5, 0.75) + xlim(0, 11) +
  scale_x_continuous("Months from hospital discharge", breaks = auc1$month) +
  scale_y_continuous("Prediction error")


perror1$pred_error1.rx = predict(perror1.rx, perror1)
plot <-
  plot_perror1 + geom_line(data = perror1,
                           aes(y = pred_error1.rx, color = model),
                           size = 3) +
  theme(axis.text.x = element_text(size = 25)) +
  theme(axis.text.y = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(legend.title = element_text(size = 25)) +
  theme(legend.text = element_text(size = 25)) +
  theme_bw()

#tabla con coeficientes
tabla.perror1 <-
  as.data.frame(cbind(coef(perror1.rx), confint(perror1.rx)))
tabla.perror1$variables <- row.names(tabla.perror1)
colnames(tabla.perror1) <-
  c("coefficient", "2.5 %"  ,   "97.5 %"   , "variables")
rownames(tabla.perror1) <- NULL
tabla.perror1 <- (tabla.perror1[, c(4, 1:3)])
summary(tabla.perror1)
#redondeo
tabla.perror1$coefficient <- round(tabla.perror1$coefficient, 3)
tabla.perror1$"2.5 %" <- round(tabla.perror1$"2.5 %", 3)
tabla.perror1$"97.5 %" <- round(tabla.perror1$"97.5 %", 3)

tabla.perror1a <-
  tableGrob(tabla.perror1, theme = ttheme_minimal(base_size = 15))
#grid.draw(tabla.perror1a)
##png grafica y modelo
library(grid)
library(gridExtra)
grid.arrange(plot,
             tabla.perror1a,
             # heights = c(0.8, 0.2),
             ncol = 2)



###prediction error MONTH
perror2 <-
  art3 %>% filter(measure == "perror" &
                    tramo == "mes") %>% pivot_longer(
                      cols = c('Cox', 'M1', 'M2'),
                      names_to =
                        'model',
                      values_to =
                        'perror'
                    )
perror2.r <- lm(perror ~ model + month, data = perror2)
summary(perror2.r)

perror2.rx <- lm(perror ~ model + month + model:month, data = perror2)
summary(perror2.rx)
confint(perror2.rx)


plot_perror2 <- ggplot(perror2, aes(x = month, y = perror, group = model)) +
  geom_point(aes(color = model), size = 3) + ylim(0.5, 0.75) + xlim(0, 11) +
  scale_x_continuous("Months from hospital discharge", breaks = auc1$month) +
  scale_y_continuous("Prediction error")


perror2$pred_error2.rx = predict(perror2.rx, perror2)
plot <-
  plot_perror2 + geom_line(data = perror2,
                           aes(y = pred_error2.rx, color = model),
                           size = 3) +
  theme(axis.text.x = element_text(size = 25)) +
  theme(axis.text.y = element_text(size = 25)) +
  theme(axis.title = element_text(size = 25)) +
  theme(legend.title = element_text(size = 25)) +
  theme(legend.text = element_text(size = 25)) +
  theme_bw()

#tabla con coeficientes
tabla.perror2 <-
  as.data.frame(cbind(coef(perror2.rx), confint(perror2.rx)))
tabla.perror2$variables <- row.names(tabla.perror2)
colnames(tabla.perror2) <-
  c("coefficient", "2.5 %"  ,   "97.5 %"   , "variables")
rownames(tabla.perror2) <- NULL
tabla.perror2 <- (tabla.perror2[, c(4, 1:3)])
summary(tabla.perror2)
#redondeo
tabla.perror2$coefficient <- round(tabla.perror2$coefficient, 3)
tabla.perror2$"2.5 %" <- round(tabla.perror2$"2.5 %", 3)
tabla.perror2$"97.5 %" <- round(tabla.perror2$"97.5 %", 3)

tabla.perror2a <-
  tableGrob(tabla.perror2, theme = ttheme_minimal(base_size = 15))
#grid.draw(tabla.perror2a)
##png grafica y modelo
library(grid)
library(gridExtra)
grid.arrange(plot,
             tabla.perror2a,
             # heights = c(0.8, 0.2),
             ncol = 2)
