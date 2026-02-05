draw_ETr_ETref_plot <- function(data) {
  data=as.data.frame(t(data)) 
  colnames(data)[9:ncol(data)] = data[1,9:ncol(data)]
  
  data = data[,-c(1:7)] 
  data = data[-c(1:6),] 
  
  
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  data <- cbind(
    TS = as.POSIXct(rownames(data), tz = "UTC"),
    data ## data and ETref
  )
  data[ , -1] <- lapply(data[ , -1], as.numeric) # all num except TS 
  
  full_path <- file.path(path, opPATH.profile)
  
  
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  output_dir <- file.path(full_path, "ETr_ETref")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)  # create folder if missing 
  
  for (i in 3:ncol(data)) {
    safe_name <- gsub("[:]", "_", as.character(colnames(data)[i]))
    file_name <- paste0("ETr_ETref_", safe_name, ".png")
    file_path <- file.path(output_dir, file_name)
    png(file = file_path)
    
    plot<-
      ggplot(data =data, aes(x=data$TS))+
      ylab("ETr and ETref (mm.15min-1) ")+
      xlab("Timestamp")+
      labs(title = paste("ETr and ETref",colnames(data)[i]))+
      geom_point(aes(y = as.numeric(data$ETref)), color = "darkolivegreen")+
      geom_line(aes(y = as.numeric(data$ETref)), color = "darkolivegreen")+ #ETref 
      
      geom_point(aes(y = as.numeric( data[,i])), color = "darkblue")+ # ETr
      
      ggplot2::scale_x_datetime(labels = scales::date_format(format = "%d-%m"), breaks  = "1 days")+
      theme(axis.text.x=element_text(angle = -45, hjust = 0))
    print(plot)
    dev.off()
    
  }
}

