draw_weight_raw <- function(data) {
  
  # Extract timestamp
  TS <- data$TS
  # Assume first column is TS, the rest are numeric
  numeric_data <- data[, -1]
  rownames(numeric_data) = TS
  
  # all columns except TS
  numeric_data <- as.data.frame(lapply(numeric_data, as.numeric))  # convert to numeric
  colnames(numeric_data) <- colnames(data)[-1]
  
  # Combine with TS again
  data_clean <- cbind(TS = data$TS, numeric_data)
  
  
  
  full_path <- file.path(path, opPATH.profile)
  
  
if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
  output_dir <- file.path(full_path, "Weight_raw_values")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)  # create folder if missing 
  
  
  
  for (i in 2:ncol(data)) {
    safe_name <- gsub("[:]", "_", as.character(colnames(data)[i]))
    file_name <- paste0("Weight_raw_values_", safe_name, ".png")
    file_path <- file.path(output_dir, file_name)
    
    png(file = file_path)
    plot <- ggplot(data, aes(x = TS, y = data[, i])) +
      geom_point() +
      labs(title = paste("Weight_raw_values ", colnames(data)[i]),
           x = "Timestamp", y = "Weight (g)") +
      scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot)
    dev.off()
  }
}
