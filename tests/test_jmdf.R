
# Test comprobar valores de rangos ***************************************************************************
comprobar_valores_rango <- function (){
  
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
      # Comprobar los rangos de los valores de variables de tipo rango y tiempo
      if (is.element(i, append(v_rango , v_tiempo))){
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
}


# Comprobar que los valores relacionados con el tiempo tienen los valores correspondientes********************
comprobar_valores_tiempo <- function (){
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
  
  factor_ajuste <- 0.001
  
  #Comprobar tiempo
  for( j in unique(df_jm$id)){
    for (i in v_tiempo){
      df <- subset(df_jm, id == j)
      # Comprobar valores con variables que tienen tiempo como meses
      # se comprueba el rango y el valor
      if (!all(df[[i]] == sort(df[[i]]))){
        print(paste(i, j))
      }
    }
  }
  
  # Comprobar que el valor mes de falta_ing1 es menor a MortOingl
  # la mortalidad tiene que ser mas tarde que cuando entrase en el proceso
  for (i in unique(df_jm$id)) {
    df <- subset(df_jm, id == i)
    if(!all(is.na(df$MortOingIcc))){
      if (!all(df$falta_ing1 <= df$MortOingIcc)) {
        print(i)
      }
    }
  }
}


# Comprobar que los valores calculados están en los rangos correspondientes***************************************
comprobar_valores_perc <- function (){
  factor_ajuste <- 0.001
  # Comprobar perc_adh_guia está entre los umbrales establecidos upper and lower boundaries
  for (x in 1:nrow(df_jm)){
    if (!all(
          (df_jm[[x, "perc_adh_guia"]] >= (max(df_jm[[x, "perc_adh_ieca"]], df_jm[[x, "perc_adh_ara2"]]) + df_jm[[x, "perc_adh_bbloq"]] - 100 - factor_ajuste))
          & (df_jm[[x, "perc_adh_guia"]] <= min(df_jm[[x, "perc_adh_ieca"]] + df_jm[[x, "perc_adh_ara2"]], df_jm[[x, "perc_adh_bbloq"]]) + factor_ajuste))
        ) {
      print(paste(df_jm[x, "id"], df_jm[x, "month"], x))
    }
  }
  
  # # Comprobar perc_adh_doctor está entre el umbral upper boundary
  # for (x in 1:nrow(df_jm)){
  #   ieca <- df_jm[[x, "perc_adh_ieca"]]
  #   ara2 <- df_jm[[x, "perc_adh_ara2"]]
  #   bbloq <- df_jm[[x, "perc_adh_bbloq"]]
  #   arm <- df_jm[[x, "perc_adh_arm"]]
  #   
  #   maximo <- max(ieca, ara2, bbloq, arm)
  #   if (!all(df_jm[[x, "perc_adh_doctor"]] >= maximo)) {
  #     print(paste(df_jm[x, "id"], df_jm[x, "month"]))
  #   }
  # }
  
  
  # Comprobar perc_adh_guia_arm está entre los umbrales establecidos upper boundary
  for (x in 1:nrow(df_jm)){
    if (!all(df_jm[[x, "perc_adh_guia_arm"]] <= max(df_jm[[x, "perc_adh_ieca"]], df_jm[[x, "perc_adh_ara2"]], df_jm[[x, "perc_adh_bbloq"]], df_jm[[x, "perc_adh_arm"]]))
    ) {
      print(paste(df_jm[x, "id"], df_jm[x, "month"]))
    }
  }
  
  
  # Comprobar perc_adh_ara2oieca está entre los umbrales establecidos low boundary
  for (x in 1:nrow(df_jm)){
    if (!all(
      df_jm[[x, "perc_adh_ara2oieca"]] >= max(df_jm[[x, "perc_adh_ieca"]], df_jm[[x, "perc_adh_ara2"]])))
    {
      print(paste(df_jm[x, "id"], df_jm[x, "month"]))
    }
  }

}

