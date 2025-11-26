########################################################################
### Respiration Models
########################################################################
#
# This module contains temperature-dependent respiration functions
# for fish bioenergetics models.
#
# Two respiration equation options are provided:
#   REQ 1: Hanson et al. (1997); Stewart et al. (1983)
#   REQ 2: Hanson et al. (1997); Kitchell et al. (1977)
#
########################################################################

Rf1T <- function(Temperature) {
  ### Temperature function equation 1 (Hanson et al. 1997; Stewart et al. 1983)
  ft <- exp(RQ*Temperature)
  return(ft)
}

RACTf1T <- function(W,Temperature) {
  ### Temperature function equation 1 with activity component (Hanson et al. 1997; Stewart et al. 1983)
  if(Temperature <= RTL) {VEL <- ACT * W ^ RK4 * exp(BACT * Temperature)
  } else if(Temperature >  RTL) {VEL <- RK1 * W ^ RK4 * exp(RK5 * Temperature)}
  ACTIVITY <- exp(RTO * VEL)
  return(ACTIVITY)
}

Rf2T <- function(Temperature) {
  ### Temperature function equation 2 (Hanson et al. 1997; Kitchell et al. 1977)
  if (Temperature< RTM) {
    V <- (RTM - Temperature) / (RTM - RTO)
    ft <- V^RX * exp(RX * (1 - V))
  } else if (Temperature>=RTM) {ft <- 0.000001}

  if(ft < 0) {ft <- 0.000001}
  return(ft)
}

respiration <- function(Temperature, W, REQ) {
  ### Respiration function
  ### Calculates respiration based on temperature, weight, and equation number
  ### Returns respiration in g O2 / g fish / day
  Rmax <- RA * W ^ RB
  if(REQ == 1) {
    ft <- Rf1T(Temperature)
    ACTIVITY <- RACTf1T(W,Temperature)
  } else if(REQ == 2) {
    ft <- Rf2T(Temperature)
    ACTIVITY <- ACT
  }
  R <- (Rmax * ft * ACTIVITY)
  return(R)
}
