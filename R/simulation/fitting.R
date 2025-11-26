########################################################################
### Binary Search Algorithm for p-value Fitting
########################################################################
#
# This module contains the binary search algorithm for finding the p-value
# (proportion of maximum consumption) that achieves a target final weight
# or total consumption.
#
########################################################################

fit.p <- function(p, IW, FW, W.tol, max.iter) {
  ### Binary search algorithm to find p-value that achieves target weight or consumption
  ###
  ### Parameters:
  ###   p: Initial guess for p-value (proportion of Cmax)
  ###   IW: Initial weight (g)
  ###   FW: Target final weight (g) or total consumption (g)
  ###   W.tol: Tolerance - predicted value must be within W.tol of target
  ###   max.iter: Maximum number of search iterations
  ###
  ### Returns: c(p, fit_p_Flag)
  ###   p: The fitted p-value
  ###   fit_p_Flag: TRUE if adequate fit found, FALSE otherwise
  ###
  ### Note: This function calls grow() which must be available in the environment

  W      <- IW    # Initial weight
  n.iter <- 0     # Counter for number of iterations
  p.max  <- 5.00  # current max
  p.min  <- 0.00  # current min
  outpt <- "End"  # desire only ending weight or consumption value, not full vector; revised by JEB
  fit_p_Flag <- FALSE # at start, no fit has been found

  withProgress(message = 'Calculating ...', min=0, max=max.iter, value = 0, {  # revised by JEB
    # initialize W.p
    W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
    while((n.iter <= max.iter) & (abs(W.p-FW) > W.tol)) {
      n.iter <- n.iter + 1
      incProgress(1, detail = paste("Doing iteration", n.iter))  # added by JEB
      if(W.p > FW) {p.max <- p} else {p.min <- p}
      p <- (p.min + p.max)/2 #p.min + (p.max - p.min)/2
      W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
    }
  })  # end of "withProgress" function; added by JEB

  if(abs(W.p-FW) <= W.tol) {fit_p_Flag <- TRUE} # adequate fit has been reached
  return(c(p, fit_p_Flag))  ## fit_p_Flag added by JEB
}
