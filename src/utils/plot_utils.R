

obs_pred_plot <- function(df, id_, new_data = FALSE, longvar){
  df$pred <- predict(result)
  df$yaxis <- df[[longvar]]
  p <- ggplot(data = subset(df, id == id_) , aes(x = month, y = yaxis)) + 
    geom_point(size = 3) + 
    theme_bw() + 
    geom_line(aes(x = month, y = pred)) + 
    ylim(-0.1, 12.1) + 
    xlim(0.5, 12.5)
  return(p)
}