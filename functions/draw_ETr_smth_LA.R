draw_ETr_smth_LA <- function(data) {
  
  data=as.data.frame(t(data)) 
  colnames(data) = data[1,]
  data = data[,-c(1:8)] 
  data = data[-c(1:6),] 
  
  
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  data <- cbind(
    TS = as.POSIXct(rownames(data), tz = "UTC"),
    data
  )
  data[ , -1] <- lapply(data[ , -1], as.numeric) # all num except TS 
  
  full_path <- file.path(path, opPATH.profile)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  
  output_dir <- file.path(full_path, "ETr_smth_LA/")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (i in 2:ncol(data)){
    
    safe_name <- gsub("[:]", "_", colnames(data)[i]) #by replacing ':' with '_'
    # Construct the full file name for the PNG output
    file_path <- paste0(output_dir,"/ETr_smth_LA", safe_name, ".png")
    
    png(file = file_path)
    
    plot<-
      ggplot(data =data, aes(x = TS, data[,i]))+
      ylab("ETr smoothed normalized by Leaf area  (mm.time interval-1.cm-2) ")+
      xlab("Timestamp")+
      labs(title = paste("ETr smoothed normalized by Leaf area ",colnames(data)[i]))+
      geom_point(aes(y = as.numeric(data[,i])), color = "chocolate")+
      ggplot2::scale_x_datetime(labels = scales::date_format(format = "%d-%m"), breaks  = "1 days")+
      theme(axis.text.x=element_text(angle = -45, hjust = 0))
    print(plot)
    dev.off()
  }} 

