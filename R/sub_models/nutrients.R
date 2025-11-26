########################################################################
### Nutrient Regeneration Functions
########################################################################
#
# This module contains functions for modeling nutrient (phosphorus and nitrogen)
# allocation in fish for nutrient regeneration studies.
#
# The functions calculate nutrient consumption, growth, excretion, and egestion
# based on prey nutrient concentrations, predator nutrient concentrations,
# and assimilation efficiencies.
#
########################################################################

phosphorous_allocation <- function(C,p_conc_prey,AEp,weightgain,p_conc_pred) {
  ### Calculate phosphorus allocation in fish
  ###
  ### Parameters:
  ###   C: Consumption by prey type (g/d)
  ###   p_conc_prey: Phosphorus concentration in prey (g P/g), by prey type
  ###   AEp: Phosphorus assimilation efficiency, by prey type
  ###   weightgain: Weight gain (g/d)
  ###   p_conc_pred: Phosphorus concentration in predator (g P/g)
  ###
  ### Returns: c(Cpsum, Gp, Up, Fp)
  ###   Cpsum: Total P consumed (g P)
  ###   Gp: P allocated to growth (g P)
  ###   Up: P excreted (g P)
  ###   Fp: P egested (g P)

  Cp <- C*p_conc_prey
  Cpsum <- sum(Cp)
  Gp <- weightgain*p_conc_pred
  Up <- sum(AEp*Cp)-Gp
  Fp <- Cpsum-Gp-Up
  return(c(Cpsum,Gp,Up,Fp))
}

nitrogen_allocation <- function(C,n_conc_prey,AEn,weightgain,n_conc_pred) {
  ### Calculate nitrogen allocation in fish
  ###
  ### Parameters:
  ###   C: Consumption by prey type (g/d)
  ###   n_conc_prey: Nitrogen concentration in prey (g N/g), by prey type
  ###   AEn: Nitrogen assimilation efficiency, by prey type
  ###   weightgain: Weight gain (g/d)
  ###   n_conc_pred: Nitrogen concentration in predator (g N/g)
  ###
  ### Returns: c(Cnsum, Gn, Un, Fn)
  ###   Cnsum: Total N consumed (g N)
  ###   Gn: N allocated to growth (g N)
  ###   Un: N excreted (g N)
  ###   Fn: N egested (g N)

  Cn <- C*n_conc_prey
  Cnsum <- sum(Cn)
  Gn <- weightgain*n_conc_pred
  Un <- sum(AEn*Cn)-Gn
  Fn <- Cnsum-Gn-Un
  return(c(Cnsum,Gn,Un,Fn))
}
