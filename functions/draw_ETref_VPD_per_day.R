draw_ETref_VPD_per_day <- function(data) {
  
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
    
    ggplot(ddata, aes(x = TS, y = ETref)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "ETref (mm· time interval min⁻¹)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/ETref_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  
  plots <- lapply(days, function(d) {
    
    ddata <- subset(data, day == d)
    
    ggplot(ddata, aes(x = TS, y = VPD)) +
      geom_line(color = "darkred", size = 1) +
      geom_point(color = "darkred", size = 1) +
      scale_x_datetime(  breaks = "1 hour", labels = scales::date_format("%H:%M")) +
      labs(
        title = paste("Day:", d),
        x = "Time of day",
        y = "VPD  (kPa)"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = paste0(full_path,"/VPD_day_", days[i], ".png"),
      plot = plots[[i]],
      width = 8,
      height = 4
    )
    
  }
  
  
  
}