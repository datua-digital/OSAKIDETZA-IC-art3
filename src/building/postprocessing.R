# Copyright 2022: Datua IA SL. All Rights Reserved
# Propietary and Confidential information of Datua IA
# Disclosure, Use or Reproduction without the written authorization of Datua IA is prohibited

postprocessing <- function(df) {
  df$id <- factor(df$id)
  return(df)
}