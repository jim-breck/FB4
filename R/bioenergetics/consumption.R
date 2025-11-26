########################################################################
### Consumption Models
########################################################################
#
# This module contains temperature-dependent consumption functions
# for fish bioenergetics models.
#
# Four temperature function equations are provided:
#   CEQ 1: Hanson et al. (1997); equation from Stewart et al. (1983)
#   CEQ 2: Hanson et al. (1997); equation from Kitchell et al. (1977)
#   CEQ 3: Hanson et al. (1997); equation from Thornton and Lessem (1978)
#   CEQ 4: Bevelhimer et al. (1985)
#
########################################################################

Cf1T <- function(Temperature) {
  ### Temperature function equation 1 (Hanson et al. 1997; equation from Stewart et al. 1983)
  ft <- exp(CQ*Temperature)
  return(ft)
}

Cf2T <- function(Temperature) {
  ### Temperature function equation 2 (Hanson et al. 1997; equation from Kitchell et al. 1977)
  if (Temperature < CTM) {
    V <- (CTM - Temperature) / (CTM - CTO)
    ft <- V^CX * exp(CX * (1 - V))
  } else if (Temperature >=CTM) {ft  <-  0}

  if(ft < 0) {ft  <-  0}  ## prevent negative values
  return(ft)
}

Cf3T <- function(Temperature) {
  ### Temperature function equation 3 (Hanson et al. 1997; equation from Thornton and Lessem 1978)
  L1 <- exp(CG1*(Temperature-CQ))
  KA <- (CK1*L1) / (1 + CK1*(L1-1))
  L2 <- exp(CG2*(CTL-Temperature))
  KB <- (CK4*L2) / (1 + CK4*(L2-1))
  ft <- KA * KB
  return(ft)
}

Cf4T <- function(Temperature) {
  ### Temperature function equation 4; equation from Bevelhimer et al. 1985 )
  ft <- exp(CQ*Temperature + CK1*Temperature^2 + CK4*Temperature^3)
  if(ft < 0) {ft  <-  0}  ## prevent negative values; added by JEB
  return(ft)
}

consumption <- function(Temperature, W, p, CEQ) {
  ### Consumption function
  ### Calculates consumption based on temperature, weight, p-value, and equation number
  ### Returns consumption in g prey / g fish
  Cmax <- CA * W ^ CB
  if(CEQ == 1) {ft = Cf1T(Temperature)   # reformatted to minimize if-tests; JEB
  } else if(CEQ == 2) {ft = Cf2T(Temperature)
  } else if(CEQ == 3) {ft = Cf3T(Temperature)
  } else if(CEQ == 4) {ft = Cf4T(Temperature)}

  return(Cmax * p * ft)
}
