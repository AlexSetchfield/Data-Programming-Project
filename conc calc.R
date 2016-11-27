#' Function for calculating the mass of chemicals in solutions
#' 
#' Calculates the mass required to make solutions based on concentration required, 
#' volume wanted and molecular weight of chemicals
#' 
#' @param chem name of chemical to be calculated
#' @param conc.wantedmMl concentration required of the chemical used
#' @param volml volume of the solution wanted
#' @param formweightgM molecular weight of the chemical used
#' 
#' @return mass (in g) of the chemical specified required to make the solution to a specified concentration
#' 
#' @examples 
#' gram.calc("DTT", 5, 25, 154.25)
#' 
#' chem <- "DTT"
#' conc.wantedmML <- 5
#' volml <- 25
#' formweightgM <- 154.25
#' gram.calc(chem, conc.wantedmML, volml, formweightgM)
#' 
#' @export
gram.calc <- function(chem, conc.wantedmML, volml, formweightgM){
  grams <- (conc.wantedmML/1000) * (volml/1000) * formweightgM
  return(paste(chem, "=", grams, "g"))
}
