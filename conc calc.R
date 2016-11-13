# used DTT as a example here to get right amounts
chem <- "DTT"
conc.wantedmML <- 5
volml <- 24
formweightgM <- 154.25
gram.calc <- function(conc.wantedmML, volml, formweightgM){
  grams <- (conc.wantedmML/1000) * (volml/1000) * formweightgM
  return(paste(chem, "=", grams, "g"))
  }

gram.calc(conc.wantedmML, volml, formweightgM)

