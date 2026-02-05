draw_ET_before_conversion <- function(data) {
  
  
  data=t(ETr_Meta)
  
  
  index_unit <- which(rownames(data) == "unit")
  LC <- as.character(data[index_unit, ]) # colnames based on unit
  
  
  data_num <- as.data.frame(apply(data, 2, as.numeric))
  data=as.data.frame(data_num)
  
  
  
  
  # Remove the first 6 rows  to leave only ET values
  rows_to_remove <- unique(c(1:6, index_unit))
  data <- data[-rows_to_remove, , drop = FALSE]
  data <- as.data.frame(data)
  
  
  saved_path = file.path(path, opPATH.obj)
  load(file = paste(saved_path, "/TS.RData", sep = ""))
  
  data$TS= TS
  data$TS =ymd_hms(data$TS )
  colnames(data)= LC
  
  
  
  opPATH.profile <- "./results/profile/"
  full_path <- file.path(path, opPATH.profile)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  output_dir <- file.path(full_path, "ETr_values_before_conversion_mm")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)  # create folder if missing 
  
  
  for (i in 1:((ncol(data)-1))){
    
    safe_name <- gsub("[:]", "_", colnames(data)[i]) #by replacing ':' with '_'
    
    # Construct the full file name for the PNG output
    file_name <- paste0("ETr_values_before_conversion_mm_", safe_name, ".png")
    file_path <- file.path(output_dir, file_name)
    
    # Open PNG device to write the plot to file
    png(file = file_path)
    
    plot<-
      ggplot(data =data, aes(TS, data[,i]))+
      ylab("ETr (g. time interval min-1)")+
      xlab("Timestamp")+
      labs(title = paste("ETr_values_before_conversion_mm",colnames(data)[i]))+
      geom_point()
    print(plot)
    dev.off()
  }} 

