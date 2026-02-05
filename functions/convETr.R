convETr <- function(x){
  
  choice <- readline(prompt = "Convert ETr in grams to mm? Required :YES or NO :")
  platform <- readline(prompt = "What platform are you using ? Required : IRD or ICRISAT  : ") 
  

  if(choice=="YES"){
    if ((platform ==  "ICRISAT") == TRUE) {
      x[, 7:ncol(x)] <-  x[, 7:ncol(x)] * 4 / 1000  # conversion en mm pour ICRISAT
      ETr_F <- x
  }}
    
  if(choice=="YES"){
    if ((platform ==  "IRD") == TRUE) {
      x[, 7:ncol(x)] <- x[, 7:ncol(x)] / 90  # conversion en mm pour IRD
      ETr_F <- x
      
    }}
  
  
  
    
    else{
    print("No conversion was done!")
    ETr_F <- x
  } 
  return(ETr_F)
}

# 1 mm = 1 L of water per m² ( 1 mm = 1 cm³/m²).