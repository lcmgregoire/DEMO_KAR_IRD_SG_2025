draw_ETr_cleaned <- function(data) {
  
  data=t(data)
  data=data[,-c(1:8)]
  data=data[-c(1:6),]
  data_num <- as.data.frame(apply(data, 2, as.numeric))
  data=data_num
  data= cbind(TS, data)
  data$TS =ymd_hms(data$TS )
  colnames(data)[2:ncol(data)]=LC
  
  full_path <- file.path(path, opPATH.profile)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  output_dir <- file.path(full_path, "ETr_filtered")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)  # create folder if missing 
  
  
    for (i in 2:(ncol(data))){
      png(file = paste(opPATH.profile,"ETr_filtered/", gsub("[:]", "_", as.character(colnames(data)[i])), ".png", sep = "")) 
      
  plot<-
    ggplot(data =data, aes(TS, data[,i]))+
    ylab("ETr (mm or g. time interval min-1)")+
    xlab("Timestamp")+
    labs(title = paste("ETr profile filtered",colnames(data)[i]))+
    geom_point()+
    geom_line()+
    ggplot2::scale_x_datetime(labels = scales::date_format(format = "%d-%m"), date_breaks = "1 days")+
    theme(axis.text.x=element_text(angle = -45, hjust = 0))
  print(plot)
  dev.off()
}}