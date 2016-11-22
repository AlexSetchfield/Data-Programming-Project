#' Lysis Buffer Calculations (Mass)
#'
#' Calculates the mass (g) required for constituents of lysis buffer, based on concentrations (mM), molecular weights (Mr) and final volume (ml).
#'
#' @param TrisHCl.conc
#' Concentration of Tris-HCl (mM) required
#' @param NaCl.conc
#' Concentration of NaCl (mM) required
#' @param DTT.conc
#' Concentration of DTT (mM) required
#' @param PMSF.conc
#' Concentration of PMSF (mM) required
#' @param volume
#' Volume (ml) required
#'
#' @return
#' List of masses (g) required to make lysis buffer: Tris, NaCl, DTT, PMSF.
#'
#' @examples
#' TrisHCl.conc <- 50
#' NaCl.conc <- 200
#' DTT.conc <- 5
#' PMSF.conc <- 1
#' volume <- 1000
#'
#' masscalc(TrisHCl.conc, NaCl.conc, DTT.conc, PMSF.conc, volume)
#'
#' masscalc(TrisHCl.conc = 50, NaCl.con = 200, DTT.conc = 5, PMSF.conc = 1, volume = 1000)
#' 
#' @export
masscalc <- function(TrisHCl.conc, NaCl.conc, DTT.conc, PMSF.conc, volume) {
  TrisHClmol.L <- (TrisHCl.conc / 1000)
  NaClmol.L <- (NaCl.conc / 1000)
  DTTmol.L <- (DTT.conc / 1000)
  PMSFmol.L <- (PMSF.conc / 1000)
  volume.L <- (volume / 1000)
  moles.TrisHCl <- (TrisHClmol.L * volume.L)
  moles.NaCl <- (NaClmol.L * volume.L)
  moles.DTT <- (DTTmol.L * volume.L)
  moles.PMSF <- (PMSFmol.L * volume.L)
  TrisHCl.mass <- (moles.TrisHCl * 121.14)
  NaCl.mass <- (moles.NaCl * 58.44)
  DTT.mass <- (moles.DTT * 154.253)
  PMSF.mass <- (moles.PMSF * 174.19)
  mass.list <- list("Tris Mass (g)" = TrisHCl.mass, "NaCl Mass (g)" = NaCl.mass, "DTT Mass (g)" = DTT.mass, "PMSF Mass (g)" = PMSF.mass)
  return(mass.list)
}

# Lysis buffer calculations (mass) for resuspension (required for sonication)
TrisHCl.conc <- 50 # Enter concentration of Tris-HCl (mM) required
NaCl.conc <- 200 # Enter concentration of NaCl (mM) required
DTT.conc <- 5 # Enter concentration of DTT (mM) required
PMSF.conc <- 1 # Enter concentration of PMSF (mM) required
volume <- 1000 # Enter final volume (ml) required

masscalc <- function(TrisHCl.conc, NaCl.conc, DTT.conc, PMSF.conc, volume) {
  TrisHClmol.L <- (TrisHCl.conc / 1000)
  NaClmol.L <- (NaCl.conc / 1000)
  DTTmol.L <- (DTT.conc / 1000)
  PMSFmol.L <- (PMSF.conc / 1000)
  volume.L <- (volume / 1000)
  moles.TrisHCl <- (TrisHClmol.L * volume.L)
  moles.NaCl <- (NaClmol.L * volume.L)
  moles.DTT <- (DTTmol.L * volume.L)
  moles.PMSF <- (PMSFmol.L * volume.L)
  TrisHCl.mass <- (moles.TrisHCl * 121.14)
  NaCl.mass <- (moles.NaCl * 58.44)
  DTT.mass <- (moles.DTT * 154.253)
  PMSF.mass <- (moles.PMSF * 174.19)
  mass.list <- list("Tris Mass (g)" = TrisHCl.mass, "NaCl Mass (g)" = NaCl.mass, "DTT Mass (g)" = DTT.mass, "PMSF Mass (g)" = PMSF.mass)
  return(mass.list)
}
masscalc(TrisHCl.conc, NaCl.conc, DTT.conc, PMSF.conc, volume)