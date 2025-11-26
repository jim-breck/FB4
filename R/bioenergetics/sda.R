########################################################################
### Specific Dynamic Action
########################################################################
#
# This module contains the specific dynamic action (SDA) function.
#
# SDA represents the metabolic cost of digesting and processing food.
# Also known as "heat increment of feeding" or "specific dynamic effect."
#
########################################################################

SpDynAct <- function(C,Eg) {
  ### Specific dynamic action function (Hanson et al. 1997)
  ### C = Consumption (J/g)
  ### Eg = Egestion (J/g)
  ### SDA = proportion of assimilated food lost to SDA
  ### Returns SDA in J/g
  S <- SDA *(C-Eg)
  return(S)
}
