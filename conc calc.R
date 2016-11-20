#' Function for calculating weights of chemicals in solutions
#' 
#' Calculates the weights required to make solutions based on concentration required, 
#' volume wanted and molecular weight of chemicals
#' 
#' @param conc_wantedmMl concentration required of the chemical used
#' @param volml volume of the solution wanted
#' @param formweightgM molecular weight of the chemical used
#' 
#' @return weight (in g) of the chemical specified required to make the solution to specified concentration
#' 
#' @examples 
#' gram_calc(5, 25, 154.25)
#' 
#' chem <- "DTT"
#' conc_wantedmML <- 5
#' volml <- 25
#' formweightgM <- 154.25
#' gram_calc(conc_wantedmML, volml, formweightgM)
#' 
#' @export
chem <- "DTT"
conc_wantedmML <- 5
volml <- 24
formweightgM <- 154.25
gram_calc <- function(conc_wantedmML, volml, formweightgM){
  grams <- (conc_wantedmML/1000) * (volml/1000) * formweightgM
  return(paste(chem, "=", grams, "g"))
  }

gram_calc(conc_wantedmML, volml, formweightgM)

