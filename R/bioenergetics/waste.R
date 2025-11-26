########################################################################
### Egestion and Excretion Models
########################################################################
#
# This module contains waste loss functions for fish bioenergetics models.
#
# Egestion: Waste from undigested food (feces)
#   Four egestion equation options (EGEQ 1-4)
#
# Excretion: Nitrogenous waste from metabolism (urine)
#   Four excretion equation options (EXEQ 1-4)
#
########################################################################

### Egestion Models ###

egestion1 <- function(C) {
  # egestion equation 1: simple proportion
  Eg = FA * C
  return(Eg)
}

egestion2 <- function(C,Temperature,p) {
  ### Egestion model from Elliott (1976)
  Eg = FA*(Temperature^FB)*exp(FG*p)*C
  return(Eg)
}

egestion3 <- function(C,Temperature,p) {
  ### Egestion model from Stewart et al. (1983)
  ### Includes indigestible prey fraction
  PE = FA*(Temperature^FB)*exp(FG*p)
  PFF = sum(globalout_Ind_Prey[i,]*globalout_Prey[i,]) # allows specification of indigestible prey, as proportions
  PF = ((PE-0.1)/0.9)*(1-PFF)+PFF
  Eg = PF*C
  return(Eg)
}

egestion4 <- function(C,Temperature) {
  ### Egestion model from Elliott (1976) - no p-value dependence
  Eg = FA*(Temperature^FB)*C
  return(Eg)
}

egestion <- function(C, Temperature, p, EGEQ) {
  ### Main egestion dispatcher function
  ### Returns egestion in J/g
  if(EGEQ == 1) {Eg <- egestion1(C)
  } else if(EGEQ == 2) {Eg <- egestion2(C,Temperature,p)
  } else if(EGEQ == 3) {Eg <- egestion3(C,Temperature,p)
  } else if(EGEQ == 4) {Eg <- egestion4(C,Temperature)
  }  # reformatted to minimize if-tests; JEB
  return(Eg)
}

### Excretion Models ###

excretion1 <- function(C, Eg) {
  # excretion equation 1: simple proportion of assimilated food
  U = UA * (C - Eg)
  return(U)
}

excretion2 <- function(C,Temperature,p,Eg) {
  ### Excretion with temperature and p-value dependence
  U = UA*(Temperature^UB)*exp(UG*p)*(C-Eg)
  return(U)
}

excretion3 <- function(C,Temperature,p,Eg) {
  ### Excretion with temperature and p-value dependence (same as equation 2)
  U = UA*(Temperature^UB)*exp(UG*p)*(C-Eg)
  return(U)
}

excretion4 <- function(C,Temperature,Eg) {
  ### Excretion with temperature dependence only - no p-value
  U = UA*(Temperature^UB)*(C-Eg)
  return(U)
}

excretion <- function(C, Eg, Temperature, p, EXEQ) {
  ### Main excretion dispatcher function
  ### Returns excretion in J/g
  if(EXEQ == 1) {U <- excretion1(C,Eg)
  } else if(EXEQ == 2) {U <- excretion2(C,Temperature,p,Eg)
  } else if(EXEQ == 3) {U <- excretion3(C,Temperature,p,Eg)
  } else if(EXEQ == 4) {U <- excretion4(C,Temperature,Eg)
  }  # reformatted to minimize if-checks; JEB
  return(U)
}
