draw_weather_per_day <- function(data) {
  

  TS = rownames(data)
  data_num <- as.data.frame(apply(data, 2, as.numeric))
  data= cbind(TS, data_num)
  data$TS <- as.POSIXct(data$TS, tz = "UTC")  
  data$day <- as.Date(data$TS)
  
  full_path <- file.path(path, opPATH.profile, "/weather/")
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  
  days <- unique(data$day)
  
  plots <- lapply(days, function(d) {
    
    ddata <- subset(data, day == d)
    
    ggplot(ddata, aes(x = TS, y = Temp)) +
      geom_line(color =  "#264653", size = 1) +
      geom_point(color =  "#264653", size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "Temperature (°C)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/Temp_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  plots <- lapply(days, function(d) {
    
    ddata <- subset(data, day == d)
    
    ggplot(ddata, aes(x = TS, y = SR)) +
      geom_line(color =     "darkorange", size = 1) +
      geom_point(color =     "darkorange", size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "Solar radiation (W/m²)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/SR_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  
  plots <- lapply(days, function(d) {
    
    ddata <- subset(data, day == d)
    
    ggplot(ddata, aes(x = TS, y = RH)) +
      geom_line(color =   "#7570B3",  , size = 1) +
      geom_point(color =   "#7570B3",  , size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "Relative Humidity (%) "
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/RH_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  
  
  
  plots <- lapply(days, function(d) {
    
    ddata <- subset(data, day == d)
    
    ggplot(ddata, aes(x = TS, y = WS)) +
      geom_line(color =   "darkgray" , size = 1) +
      geom_point(color =   "darkgray", size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "Wind Speed (km/h)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/WS_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  
  
  
  
}