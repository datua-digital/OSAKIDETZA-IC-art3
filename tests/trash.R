# browser() # analizando sin end < falta_ing1 + duration o end < MortOingIcc
# df_test <- df
# df_test <- df_test[!is.na(as.character(df_test$MortOingIcc)), ]
# df_test$cohorte1anyo <-  df_test$falta_ing1 +  duration
# df_test2 <- df_test %>% group_by(id) %>%
#   mutate(end_menor_max = dplyr::if_else( all(end < MortOingIcc) & all(end < cohorte1anyo), TRUE, FALSE) )
# 
# df_test2 <- df_test[(df_test$end < df_test$MortOingIcc) & (df_test$end < df_test$falta_ing1 + duration),]

# Casos a tener en cuenta
# first_month_bigger_one <- baseJoinModel2_3 %>% group_by(id) %>% filter(min(month)>1)




# baseJoinModel2_7_completo <- baseJoinModel2_7[is.na(as.character(baseJoinModel2_7$MortOingIcc)), ]
# nrow(baseJoinModel2_7_completo) / length(unique(baseJoinModel2_7_completo$id))
# 
# for (i in c(1:12)){
#   print(i)
#   ids <- baseJoinModel2_6_completo[baseJoinModel2_6_completo$month==i,'id']
#   print(base::setdiff(baseJoinModel2_6_completo$id, ids$id))
# }