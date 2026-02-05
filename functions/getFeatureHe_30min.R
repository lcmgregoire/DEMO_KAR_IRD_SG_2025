getFeatureHe_30min <- function(x, y, d, p) {
  
  allFeatures <- x
  raw.trans <- y
  dates <- d
  nFeatures <- 15  # features
  
 
  F.He <- matrix(NA, nrow = length(dates), ncol = nFeatures)
  colnames(F.He) <- c("maxET", "slope.maxET.6", "slope.07maxET", "slope.00.07", "slope.19.2330", "curvmaxET", 
                      "total.auc","auc.10.15", "sd.10.15", "auc.prop.10.15", "auc.07.19", "sd.07.19",  
                      "auc.prop.07.19", "auc.night", "cos.sim.index")
  
  for (i in 1:length(dates)) {
    
    # Création du tmp DF pour stocker les features de tous les genotypes à cette date
    nRowsTmp <- nrow(raw.trans) - 8
    tmp <- as.data.frame(matrix(NA, nrow = nRowsTmp, ncol = nFeatures))
    colnames(tmp) <- colnames(F.He)
    
    # Remplissage ligne par ligne
    for (j in 1:length(allFeatures)) {
      vals <- allFeatures[[j]][i, ]
      

      length(vals) <- nFeatures  # remplit avec NA si moins, need to be 15 ! 
      tmp[j, ] <- vals
    }
    

    
    # Construire le dataframe final
    features.DF <- cbind(raw.trans[9:nrow(raw.trans), 1:6], tmp)
    
    # Créer le dossier si nécessaire et écrire le CSV
    if (!dir.exists(p)) dir.create(p, recursive = TRUE)
    write.csv(features.DF, file = paste0(p, "D.", dates[i], ".csv"), row.names = FALSE)
    
    print(paste("Processed date", i, "->", dates[i]))
    
    # ============================
    # heritability (He)
    # ============================
    he.vec <- c()
    for (f in 1:nFeatures) {
      he.tmp <- features.DF[, c(4, f + 6)]  # 4 = colonne Genotype
      colnames(he.tmp)[2] <- "Feature"
      
      he.tmp$Genotype <- factor(he.tmp$Genotype)
      y <- he.tmp$Feature
      selector <- !is.na(y)
      y <- y[selector]
      line <- as.factor(as.character(he.tmp$Genotype[selector]))
      
      model <- tryCatch(gam(y ~ s(line, bs = "re"), method = "REML"),
                        error = function(e) NULL)
      if (!is.null(model)) {
        res <- gam.vcomp(model)
        vg <- res[[1]]^2
        ve <- res[[4]]^2
        he <- vg / (vg + ve)
        he.vec <- c(he.vec, round(he, 3))
      } else {
        he.vec <- c(he.vec, NA)
      }
    }
    
    # Stocker les He
    F.He[i, ] <- he.vec
  }
  
  return(F.He)
}
