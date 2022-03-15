
postprocessing <- function(df) {
  df$id <- factor(df$id)
  return(df)
}