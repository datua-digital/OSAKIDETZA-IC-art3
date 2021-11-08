# comprobar que los meses están ordenados:
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  # print(all(df$month == sort(df$month)))
  if (!all(df$month == sort(df$month))) {
    print(i)
  }
  count <- count + 1
}

# comprobar que cum_perc_adh_guia_arm está ordenado:
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$cum_perc_adh_guia_arm == sort(df$cum_perc_adh_guia_arm))) {
    print(i)
    browser()
  }
  count <- count + 1
}

# comprobar que el rango de años está bien
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$edad_ing1 >= 0)) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor sexo tiene los valores correspondientes
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$sexo == "Hombre" | df$sexo == "Mujer")) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor mes tiene los valores correspondientes
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$month <= 12 & df$month >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor mes de falta_ing1 es menor a MortOingl
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if(!all(is.na(df$MortOingIcc))){
    if (!all(df$falta_ing1 <= df$MortOingIcc)) {
      print(i)
    }
  }
  count <- count + 1
}

# comprobar que el valor time_to_event tiene los valores correspondientes
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$time_to_event <= 12.001 & df$time_to_event >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor dura_in_months tiene los valores correspondientes
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if(!all(is.na(df$dura_in_months))){
    if (!all(df$dura_in_months <= 12 & df$dura_in_months >= 0 )) {
      print(i)
    }
  }
  count <- count + 1
}

# comprobar que el valor last_month tiene los valores correspondientes
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if(!all(is.na(df$last_month))){
    if (!all(df$last_month <= 12.001 & df$last_month >= 0 )) {
      print(i)
    }
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_ara2 tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_ara2 <= 100 & df$perc_adh_ara2 >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_bloq tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_bbloq <= 100 & df$perc_adh_bbloq >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_arm tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if(!all(is.na(df$perc_adh_arm))){
    if (!all(df$perc_adh_arm <= 100 & df$perc_adh_arm >= 0 )) {
      print(i)
    }
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_ieca tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_ieca <= 100 & df$perc_adh_ieca >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_guia tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_guia <= 100 & df$perc_adh_guia >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_doctor tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_doctor <= 100 & df$perc_adh_doctor >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_ara2oieca tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_ara2oieca <= 100 & df$perc_adh_ara2oieca >= 0 )) {
    print(i)
  }
  count <- count + 1
}

# comprobar que el valor perc_adh_guia_arm tiene el rango correspondiente
count <- 0
for (i in unique(df_jm$id)) {
  df <- subset(df_jm, id == i)
  if (!all(df$perc_adh_guia_arm <= 100 & df$perc_adh_guia_arm >= 0 )) {
    print(i)
  }
  count <- count + 1
}

