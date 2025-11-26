########################################################################
### Predator Energy Density Function
########################################################################
#
# This module contains functions for calculating predator energy density.
#
# Three equation options are provided:
#   PREDEDEQ 1: Daily interpolated values from input file (ignores weight)
#   PREDEDEQ 2: Linear function of weight (two line segments)
#   PREDEDEQ 3: Power function of weight
#
########################################################################

pred_En_D <- function(W,day,PREDEDEQ) {
  ### Find Energy Density (ED, J/g) for this day and weight
  ### W = weight (g)
  ### day = day number
  ### PREDEDEQ = equation option (1, 2, or 3)
  ### Returns energy density in J/g

  if(PREDEDEQ == 1) {return(Pred_E[day])   # Use daily interpolated values from csv file; ignore weight

  } else if(PREDEDEQ == 3) {return(alpha1*W^beta1)  # ED is power function of weight; ignore day

  } else if(PREDEDEQ == 2) {  # Using two line segments, ED is linear function of weight; ignore day
    Wco = as.numeric(cutoff)  # Wco is weight at cutoff, where the line breaks
    if(W <Wco) {return((as.numeric(alpha1) + as.numeric(beta1)*W))}
    if(W>=Wco) {return((as.numeric(alpha2) + as.numeric(beta2)*W))}
    if(W <Wco && as.numeric(beta1) == 0) {return((as.numeric(alpha1)))}
    if(W>=Wco && as.numeric(beta2) == 0) {return((as.numeric(alpha2)))}
  }  # restructured using "else if" to reduce if-tests; JEB
}
