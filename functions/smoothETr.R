smoothETr <- function(x) {
  
  LC = x[9:nrow(x),1]
  TS = colnames(x)[7:ncol(x)]
  
  
  
  weather_data = x[1:8,]
  meta_data = x[9:nrow(x),1:6]
  
  x = x[9:nrow(x),] # remove weather data
  rownames(x) = x$unit
  meta_data = x[,1:6]
  x = x[,7:ncol(x)] # remove meta data 
  
  etrOP <- etrIP <- x
  
  dates = as.Date(TS)
  
  df = as.data.frame(t(etrIP))
  df = cbind(dates= dates, df)
  
  for (i in 2:ncol(df)) { # boucle sur chaque LC / colonne
    subset_LC <- df[, c(1, i)]
    colnames(subset_LC) <- c("dates", "value")
    
    y_smoothed <- numeric(nrow(subset_LC))
    
    for (d in unique(subset_LC$dates)) {
      subset_day <- subset_LC %>% filter(dates == d)
      
      # si tout NA → skip
      if (all(is.na(subset_day$value))) next
      
      # Calculer Q1 et Q3
      value <- 
      Q <- quantile(subset_day$value, probs = c(0.25, 0.75), na.rm = TRUE)
      Q1 <- Q[1]
      Q3 <- Q[2]
      
      # interpolate NA
      subset_day$value <- na.spline(subset_day$value)
      
      # remplacer valeurs hors Q1-Q3 par NA
      subset_day$value[value < Q1 | value > Q3] <- NA
      
      # lissage LOESS si au moins 5 points non-NA
      if (sum(!is.na(subset_day$value)) >= 5) {
        x_day <- 1:length(subset_day$value)
        value <- predict(loess(subset_day$value ~ x_day, span = 0.25))
      }
      
      value[value < Q1 | value > Q3] <- NA
      
      # remettre dans y_smoothed
      idx <- which(subset_LC$dates == d)
      y_smoothed[idx] <- value
    }
    
    # remettre la colonne lissée dans etrOP
    etrOP[i-1, ] <- y_smoothed
  }
  
  
  etrOP_metadata= cbind(meta_data,etrOP)
  etrOP_metadata_wther = rbind(weather_data, etrOP_metadata)
  
  return(etrOP_metadata_wther)
}
