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
