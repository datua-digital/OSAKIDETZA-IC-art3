comprobar_valores <- function (){
  
  variables <- list(
    "month"= c(12, 0),
    "edad_ing1" = c(1000, 0),
    "sexo" = c("Hombre", "Mujer"),
    "time_to_event" = c(12.001, 0.001),
    "dura_in_months" = c(12, 0),
    "last_month" = c(12.001, 0),
    "perc_adh_ara2" = c(100, 0),
    "perc_adh_bbloq" = c(100, 0),
    "perc_adh_arm" = c(100, 0),
    "perc_adh_ieca" = c(100, 0),
    "perc_adh_guia" = c(100, 0),
    "perc_adh_doctor" = c(100, 0),
    "perc_adh_ara2ioeca" = c(100, 0),
    "perc_adh_guia_arm" = c(100, 0),
    "cum_perc_adh_ara2" = c(12, 0),
    "cum_perc_adh_bbloq" = c(12, 0),
    "cum_perc_adh_ieca" = c(12, 0),
    "cum_perc_adh_doctor" = c(12, 0),
    "cum_perc_adh_guia" = c(12, 0),
    "cum_perc_adh_ara2ioeca" = c(12, 0),
    "cum_perc_adh_guia_arm" = c(12, 0)
  )
  
  v_tiempo <- 
    c("month",
      "cum_perc_adh_ara2",
      "cum_perc_adh_bbloq",
      "cum_perc_adh_ieca",
      "cum_perc_adh_doctor",
      "cum_perc_adh_guia",
      "cum_perc_adh_ara2oieca",
      "cum_perc_adh_guia_arm")
  v_rango <- 
    c("edad_ing1",
      "time_to_event",
      # "dura_in_months",
      #"last_month",
      "perc_adh_ara2",
      "perc_adh_bbloq",
      "perc_adh_arm",
      "perc_adh_ieca",
      "perc_adh_guia",
      "perc_adh_doctor",
      "perc_adh_ara2oieca",
      "perc_adh_guia_arm")
  v_categorico <-
    c("sexo")
  
  
  
  for( j in unique(df_jm$id)){
    for (i in names(variables)){
      df <- subset(df_jm, id == j)
      # Comprobar valores con variables que tienen tiempo como meses
      # se comprueba el rango y el 
      if (is.element(i, v_tiempo)){
        if (!all(df[i] <= variables[i][[1]][1] & df[i] >= variables[i][[1]][2])){
        	print(paste(i, j))
        }
        if (!all(df[[i]] == sort(df[[i]]))){
          print(paste(i, j))
        }
      # Comprobar los rangos de los valores
      }else if (is.element(i, v_rango)){
        if (!(i == "perc_adh_arm" & any(is.na(df[[i]])))){
          if (!all(df[i] <= variables[i][[1]][1] & df[i] >= variables[i][[1]][2])){
            print(paste(i, j))
          }
        }
      # Comprobar los valores de variables categóricas
      }else if (is.element(i, v_categorico)){
        if (!all(is.element(df[[i]], variables[i][[1]]))){
          print(paste(i, j))
        }
      }
    }
  }
  
  
  # comprobar que el valor mes de falta_ing1 es menor a MortOingl*****************
  for (i in unique(df_jm$id)) {
    df <- subset(df_jm, id == i)
    if(!all(is.na(df$MortOingIcc))){
      if (!all(df$falta_ing1 <= df$MortOingIcc)) {
        print(i)
      }
    }
  }
}


# 
# # comprobar que los meses están ordenados:
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$month == sort(df$month))) {
#     print(i)
#   }
# }
# 
# # comprobar que cum_perc_adh_guia_arm está ordenado:
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_guia_arm == sort(df$cum_perc_adh_guia_arm))) {
#     print(i)
#     browser()
#   }
# }
# 
# # comprobar que el rango de años está bien
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$edad_ing1 >= 0)) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor sexo tiene los valores correspondientes
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$sexo == "Hombre" | df$sexo == "Mujer")) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor mes tiene los valores correspondientes
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$month <= 12 & df$month >= 0 )) {
#     print(i)
#   }
# }
# 

# 
# # comprobar que el valor time_to_event tiene los valores correspondientes
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$time_to_event <= 12.001 & df$time_to_event >= 0.001 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor dura_in_months tiene los valores correspondientes
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if(!all(is.na(df$dura_in_months))){
#     if (!all(df$dura_in_months <= 12 & df$dura_in_months >= 0 )) {
#       print(i)
#     }
#   }
# }
# 
# # comprobar que el valor last_month tiene los valores correspondientes
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if(!all(is.na(df$last_month))){
#     if (!all(df$last_month <= 12.001 & df$last_month >= 0 )) {
#       print(i)
#     }
#   }
# }
# 
# # comprobar que el valor perc_adh_ara2 tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_ara2 <= 100 & df$perc_adh_ara2 >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_bloq tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_bbloq <= 100 & df$perc_adh_bbloq >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_arm tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if(!any(is.na(df$perc_adh_arm))){
#     if (!all(df$perc_adh_arm <= 100 & df$perc_adh_arm >= 0 )) {
#       print(i)
#     }
#   }
# }
# 
# # comprobar que el valor perc_adh_ieca tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_ieca <= 100 & df$perc_adh_ieca >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_guia tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_guia <= 100 & df$perc_adh_guia >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_doctor tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_doctor <= 100 & df$perc_adh_doctor >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_ara2oieca tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_ara2oieca <= 100 & df$perc_adh_ara2oieca >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor perc_adh_guia_arm tiene el rango correspondiente
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$perc_adh_guia_arm <= 100 & df$perc_adh_guia_arm >= 0 )) {
#     print(i)
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_ara2 tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_ara2 <= 12 & df$cum_perc_adh_ara2 >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_ara2)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_ara2[a] > df$cum_perc_adh_ara2[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_bbloq tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_bbloq <= 12 & df$cum_perc_adh_bbloq >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_bbloq)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_bbloq[a] > df$cum_perc_adh_bbloq[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_ieca tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_ieca <= 12 & df$cum_perc_adh_ieca >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_ieca)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_ieca[a] > df$cum_perc_adh_ieca[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_doctor tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_doctor <= 12 & df$cum_perc_adh_doctor >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_doctor)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_doctor[a] > df$cum_perc_adh_doctor[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# 
# # comprobar que el valor cum_perc_adh_guia tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_guia <= 12 & df$cum_perc_adh_guia >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_guia)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_guia[a] > df$cum_perc_adh_guia[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_ara2oieca tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_ara2oieca <= 12 & df$cum_perc_adh_ara2oieca >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_ara2oieca)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_ara2oieca[a] > df$cum_perc_adh_ara2oieca[a+1]){
#         print(i)
#       }
#     }
#   }
# }
# 
# # comprobar que el valor cum_perc_adh_guia_arm tiene el rango correspondiente y que va aumentandose
# for (i in unique(df_jm$id)) {
#   df <- subset(df_jm, id == i)
#   if (!all(df$cum_perc_adh_guia_arm <= 12 & df$cum_perc_adh_guia_arm >= 0 )) {
#     print(i)
#   }
#   meses <- length(df$cum_perc_adh_guia_arm)
#   if(meses != 1){
#     for(a in 1:(meses - 1)){
#       if(df$cum_perc_adh_guia_arm[a] > df$cum_perc_adh_guia_arm[a+1]){
#         print(i)
#       }
#     }
#   }
# }
