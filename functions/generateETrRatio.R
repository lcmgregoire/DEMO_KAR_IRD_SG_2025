generateETrRatio <- function(x) {
  
wthr.ETref.ETobs.Ratio= as.data.frame(t(x))
wthr.ETref.ETobs.Ratio=wthr.ETref.ETobs.Ratio[-c(1:6),] # remobve climate data 
wthr.ETref.ETobs.Ratio[1,]=NA ## remove 0 at the first row  

wthr.ETref.ETobs.Ratio_num <- as.data.frame(apply(wthr.ETref.ETobs.Ratio, 2, as.numeric))
wthr.ETref.ETobs.Ratio=wthr.ETref.ETobs.Ratio_num
colnames(wthr.ETref.ETobs.Ratio)[9:ncol(wthr.ETref.ETobs.Ratio)] = x$unit[9:nrow(x)]
rownames(wthr.ETref.ETobs.Ratio)= colnames(x[7:ncol(x)])
limit_inf=-0.01
print("This may take a while, why don't you go for a coffee? ")

cat("\nCompare ETr to solar radiation and ETref values\n")

n_rows <- nrow(wthr.ETref.ETobs.Ratio)
n_cols <- ncol(wthr.ETref.ETobs.Ratio)

# Progress bar sur toutes les cellules à traiter (rows * cols)
total_steps <- n_rows * (n_cols - 8)
pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
step <- 0


  for (j in (9:ncol(wthr.ETref.ETobs.Ratio))){ ## j col LC

  for (i in 1:nrow(wthr.ETref.ETobs.Ratio)) { ## i row TS

    if (!is.na(wthr.ETref.ETobs.Ratio$SR[i]) == 0) { ### during the night period, SR 0, ETr= 0
      wthr.ETref.ETobs.Ratio[i,j] = 0
    }
    if (!is.na(wthr.ETref.ETobs.Ratio$SR[i]) > 0) { ### during the day period, SR +,  ETr value should be between 0 and ETref. Otherwise, outlier
      limit_sup = wthr.ETref.ETobs.Ratio$ETref[i]
      wthr.ETref.ETobs.Ratio[i,j][wthr.ETref.ETobs.Ratio[i,j]> limit_sup]<- NA ## if the ETr is above the limit, replace by NA
      wthr.ETref.ETobs.Ratio[i,j][wthr.ETref.ETobs.Ratio[i,j]<limit_inf]<- NA ## if the ETr is negative
      
      
      
    }
    # Update progress bar
    step <- step + 1
    setTxtProgressBar(pb, step)
  }
    
    }
wthr.ETref.ETobs.Ratio[1,]=NA ## remove 0 at the first row  

cat("\nInterpolate NA values\n")

total_steps <- n_rows * (n_cols - 8)
pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
step <- 0



  
  for (j in (9:ncol(wthr.ETref.ETobs.Ratio))){ ## j col LC
    
    for (i in 1:nrow(wthr.ETref.ETobs.Ratio)) { ## i row TS
      if (!is.na(sum(wthr.ETref.ETobs.Ratio[i,9:ncol(wthr.ETref.ETobs.Ratio)]))){
        wthr.ETref.ETobs.Ratio[i, j] <- na.approx(wthr.ETref.ETobs.Ratio[i, j]) }
      step <- step + 1
      setTxtProgressBar(pb, step)
      
}
}

return(wthr.ETref.ETobs.Ratio)
  }

