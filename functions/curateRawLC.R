curateRawLC <- function(x, y, z) {
  
  base.d <- x[,-1] ## remove TS
  
  data.RmOut <- base.d 
  
  for ( i in 1:(ncol(base.d)) ) {
    data <- base.d[, i]
    data[data < 0] <- NA # if less than 0, it is wrong.
    data[data < (z-(z*0.3))]<- NA
    data[data > (z+(z*0.3))]<- NA
    data <- as.numeric(unlist(data))
  
    ol <- boxplot(data, plot = FALSE)$out
    tf <- which(data %in% ol)
    
    data.RmOut[, i] <- data
    data.RmOut[tf, i] <- NA
    
  }
  
  rmOut <- as.data.frame(t(data.RmOut))
  
  rmOut.DF <- as.data.frame(cbind(y, rmOut))
  colnames(rmOut.DF) <- colnames(LC.MAT.raw) ##rmOut.DF DF with metadata and weight,  outliers removed before imputation
 
    ####### Imputation #######
  data.Imp <- data.frame(apply(data.RmOut, 2, na.approx, na.rm = F))
  
  data.Imp.df <- as.data.frame(t(data.Imp))
  
  imputed.DF <- as.data.frame(cbind(y, data.Imp.df))
  colnames(imputed.DF) <- colnames(LC.MAT.raw)
   
  # There still could remain columns with maximum missing, hence run the below script
  na.list <- as.numeric(apply(imputed.DF[,-c(1:6)], 1, FUN = function(x) {sum(is.na(x))}))
  
  # keep sectors which have more than 30% of values
  na.list.G.Locs <- which(na.list > ceiling(0.3*dim(imputed.DF[,-c(1:5)])[2])) 
  
  # replace 'na.list.G.Locs' only with 0 if NA so that ETr can be extracted
  if(length(na.list.G.Locs) > 0)
  {
    imputed.DF.tmp <- imputed.DF
    imputed.DF.tmp <- imputed.DF.tmp[-(na.list.G.Locs+5), ]
  } else {imputed.DF.tmp <- imputed.DF}
  
  imputed.DF.final <- imputed.DF.tmp
  
  interp.ip <- imputed.DF.final[ ,7:ncol(imputed.DF.final)]
  
  interp.df <- as.data.frame(t(interp.ip))
  
  interp.df.op <- as.data.frame(apply(interp.df, 1, na.aggregate.default))
  
  imputed.DF.final[ ,7:ncol(imputed.DF.final)] <- interp.df.op
  
  return(imputed.DF.final)
}