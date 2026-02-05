draw_weight_cleaned <- function(data, path) {
  
  # Transpose the data
  data <- t(data)
  
  # Find the row index named "unit" in the rownames
  index_unit <- which(rownames(data) == "unit")
  colnames(data) <- as.character(data[index_unit, ])
  
  # Remove the first 6 rows  to leave only weight
  rows_to_remove <- unique(c(1:6, index_unit))
  data <- data[-rows_to_remove, , drop = FALSE]
    data <- as.data.frame(data)
  
    TS = rownames(data)
    TS = as.POSIXct(TS)
  # Identify columns to convert to numeric (all except the timestamp column)
  # Here, assume there is a column named 'TS' for timestamps
  if ("TS" %in% colnames(data)) {
    # Convert the 'TS' column to proper datetime format (lubridate function)
    data$TS <- lubridate::ymd_hms(data$TS)
    
    # Select all columns except 'TS' for numeric conversion
    cols_num <- setdiff(colnames(data), "TS")
  } else {
    # If no 'TS' column, convert all columns (this might need adjustment)
    cols_num <- colnames(data)
  }
  
  # Convert selected columns to numeric safely (in case they are factors/characters)
  data[cols_num] <- lapply(data[cols_num], function(x) as.numeric(as.character(x)))
  
  # Define the base path to store plot
  full_path <- file.path(path, opPATH.profile)
  if (!dir.exists(full_path)) dir.create(full_path, recursive = TRUE)
    output_dir <- file.path(full_path, "Weight_filtered")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Loop through each numeric column to generate plots
  for (i in cols_num) {
    
    # Create a safe file name by replacing ':' with '_'
    safe_name <- gsub("[:]", "_", i)
    
    # Construct the full file name for the PNG output
    file_name <- paste0("Weight_filtered_", safe_name, ".png")
    file_path <- file.path(output_dir, file_name)
    
    # Open PNG device to write the plot to file
    png(file = file_path)
    
    # Build the ggplot object using timestamps (TS) on x-axis and column i on y-axis
    plot <- ggplot(data, aes(x = TS, y = .data[[i]])) +
      geom_point() +
      labs(title = paste("Weight_filtered", i),
           x = "Timestamp", y = "Weight (g)")+
      scale_x_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
      
    
    # Print the plot to the PNG device
    print(plot)
    
    # Close the PNG device to finalize the file
    dev.off()
  }
}
