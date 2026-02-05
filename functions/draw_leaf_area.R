draw_leaf_area<- function(data) {
  
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- cbind(
    DAS = rownames(data),
    data
  )
  
  data<- lapply(data, as.numeric) # all num except TS 
  
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  full_path <- file.path(path, opPATH.profile)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  
  output_dir <- file.path(full_path, "LEAF_AREA/")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (i in 2:ncol(data)){
    
    safe_name <- gsub("[:]", "_", colnames(data)[i]) #by replacing ':' with '_' if exists
    # Construct the full file name for the PNG output
    file_path <- paste0(output_dir,"/Daily_LA_", safe_name, ".png")
    
    png(file = file_path)
    
    plot<-
      ggplot(data =data, aes(x = DAS, data[,i]))+
      ylab("Leaf area cm² ")+
      xlab("DAS ")+
      labs(title = paste("Leaf area cm² ",colnames(data)[i]))+
      geom_point(aes(y = as.numeric(data[,i])), color = "darkred")+
      scale_x_continuous(breaks = seq(min(data$DAS), max(data$DAS), by = 2))
    print(plot)
    dev.off()
  }} 

