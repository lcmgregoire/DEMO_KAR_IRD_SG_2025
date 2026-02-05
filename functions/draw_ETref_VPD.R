draw_ETref_VPD <- function(data) {
  
  TS = rownames(data)
  data_num <- as.data.frame(apply(data, 2, as.numeric))
  data= cbind(TS, data_num)
  data$TS <- as.POSIXct(data$TS, tz = "UTC")  
  data$day <- as.Date(data$TS)
  
  full_path <- file.path(path, opPATH.profile, "/Weather/")
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)

    png(file = paste(full_path,"/ETref.png", sep = "")) 
    
    plot<-
      ggplot(data =data, aes(x=TS, y=ETref))+
      ylab("ETref (mm. time interval min-1)")+
      xlab("Timestamp")+
      geom_point(color = "darkblue", size = 1) +
      ggplot2::scale_x_datetime(labels = scales::date_format(format = "%d-%m"), date_breaks = "1 days")+
      theme(axis.text.x=element_text(angle = -45, hjust = 0))
    print(plot)
    
    dev.off()
    
    png(file = paste(full_path,"/VPD.png", sep = "")) 
    
    plot<-
      ggplot(data =data, aes(x=TS, y=VPD))+
      ylab("VPD (kPa)")+
      xlab("Timestamp")+
      geom_point(color = "darkred", size = 1) +
      ggplot2::scale_x_datetime(labels = scales::date_format(format = "%d-%m"), date_breaks = "1 days")+
      theme(axis.text.x=element_text(angle = -45, hjust = 0))
    print(plot)
    
    dev.off()
    
    
 
    }
    
    
  



