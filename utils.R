library(plotly)

points_color =c("chartreuse3","brown1")


plotscatter <- function(df) {
  df <- data.frame(df)
  df <- df %>%
    filter(!is.na(HT))
  if (length(unique(df$OUTLAYER)) == 1) {
    points_color_2 <- c("chartreuse3")
  } else {
    points_color_2 <- c("chartreuse3", "red")
  }
  
  df$DAP <- round(df$DAP/10, 2)
  df$HT <- round(df$HT/10, 2)
  df$HT_CURTIS <- round(df$HT_CURTIS/10, 2)
  max_DAP <- max(df$DAP, na.rm = TRUE)
  max_HT <- max(df$HT, na.rm = TRUE) 

  
  
  hover_text <- paste("<b>DAP:</b> %{x:.2f}<br>",
                      "<b>HT:</b> %{y:.2f}<br>",
                      "<b>Medição:</b> %{text}<br>",
                      "<b>ID Hub:</b> %{customdata}<br>",
                      "<b>CD Parcela:</b> %{meta}<br>")
  
  scatterPlot <- plot_ly(df, x = ~DAP, y = ~HT, color = ~OUTLAYER,
                         colors = points_color_2, type = "scatter", mode = "markers",
                         
                         customdata = ~CD_PROC_IMPORTA_HUB,
                         text = ~CD_MEDICAO,
                         meta = ~CD_MEDICAO_PARCELA,
                         marker = list(size = 10,
                                       line = list(color = 'rgba(7, 7, 7, 0.8)',
                                                   width = 2)),
                         hovertemplate = hover_text) 
  
  scatterPlot <- scatterPlot %>% layout(xaxis = list(title = "DAP (m)", range = c(0, max_DAP+5)),
                                        yaxis = list(title = "HT (m)", range = c(0, max_HT+5)),
                                        showlegend = TRUE,
                                        hoverlabel = list(bgcolor = "black", font = list(size = 12)))
  
  return(scatterPlot)
}





plotscatter_rna <- function(df) {
  df <- data.frame(df)
  df$DAP <- round(df$DAP/10,2)
  df$HT_FINAL <- round(df$HT_FINAL/10,2)

  max_DAP <- max(df$DAP, na.rm = TRUE)  
  max_HT_FINAL <- max(df$HT_FINAL, na.rm = TRUE) 
  
  hover_text <- paste("<b>DAP:</b> %{x:.2f}<br>",
                      "<b>HT Final:</b> %{y:.2f}<br>",
                      "<b>Medição:</b> %{text}<br>",
                      "<b>ID Hub:</b> %{customdata}<br>",
                      "<b>CD Parcela:</b> %{meta}<br>")
  
  scatterPlot <- plot_ly(df, x = ~DAP, y = ~HT_FINAL, color = ~ORIGEM,colors = points_color,
                         type = "scatter", mode = "markers",
                         text = ~CD_MEDICAO,
                         marker = list(size = 10,
                                       line = list(color = 'rgba(7, 7, 7, 0.8)',
                                                   width = 2)),
                         customdata = ~CD_PROC_IMPORTA_HUB,
                         meta = ~CD_MEDICAO_PARCELA,
                         hovertemplate = hover_text) %>%
    layout(xaxis = list(title = "DAP (m)", range = c(0, max_DAP+5)),
           yaxis = list(title = "HT (m)", range = c(0, max_HT_FINAL+5)),
           showlegend = TRUE,
           hoverlabel = list(bgcolor = "black", font = list(size = 12)))
  
  return(scatterPlot)
}




get_r2 = function(y_true,y_pred){
  sst = sum((y_true - mean(y_true))^2)
  sse = sum((y_pred - y_true)^2)
  r2 = 1 -(sse/sst)
  return(r2)
  
}

criar_zip_download <- function(data) {
  # Dividir o dataframe em pedaços baseados na coluna
  n_med = unique(data$CD_PROC_IMPORTA_HUB)
  
  # Criar um arquivo zip
  zip_file <- zip::zip(zipfile = data)
  
  return(zip_file)
}


curtis <- function(cap, b0, b1) {
  h <- exp(b0 + b1 * (1/cap))
  return(h)
}

linear_regression <- function(x, y) {
  N <- length(x)
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  B1_num <- sum((x - x_mean) * (y - y_mean))
  B1_den <- sum((x - x_mean)^2)
  
  B1 <- B1_num / B1_den
  B0 <- y_mean - (B1 * x_mean)
  
  return(list(B0 = B0, B1 = B1))
}
