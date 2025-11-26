########################################################################
### Growth Calculation Helper Functions
########################################################################
#
# This module contains helper functions for the grow() function
# to break down complex calculations into manageable pieces.
#
########################################################################

calculate_final_weight <- function(W, egain, SpawnE, Pred_E_i, PREDEDEQ, alpha1, beta1, cutoff, alpha2, beta2, i, nR, Temperature_i, p, outpt) {
  ### Calculate final weight at end of day given energy gain
  ###
  ### This function handles the three different predator energy density equations (PREDEDEQ)
  ### with all their complexity, including quadratic formulas and cutoff transitions.
  ###
  ### Parameters:
  ###   W: Starting weight (g)
  ###   egain: Net energy gain (J)
  ###   SpawnE: Energy lost to spawning (J)
  ###   Pred_E_i: Predator energy density at start of day (J/g)
  ###   PREDEDEQ: Predator energy density equation (1, 2, or 3)
  ###   alpha1, beta1, cutoff, alpha2, beta2: Parameters for PREDEDEQ equations
  ###   i, nR, Temperature_i, p, outpt: For error reporting
  ###
  ### Returns: list(finalwt, Pred_E_iplusone)

  if(PREDEDEQ == 3) {
    # PREDEDEQ 3: ED as power function of weight
    finalwt <- ((egain - SpawnE + (Pred_E_i * W)) / alpha1) ^ (1 / (beta1 + 1))
    Pred_E_iplusone <- pred_En_D(W = finalwt, day = i, PREDEDEQ = PREDEDEQ)

  } else if(PREDEDEQ == 2) {
    # PREDEDEQ 2: ED from weight using one of two line segments
    Wco <- as.numeric(cutoff)  # weight at cutoff

    if(W < Wco) {
      # Weight at start of day is below cutoff
      if(beta1 != 0) {
        # Use quadratic formula to calc finalwt
        flagvalue1 <- (alpha1 * alpha1 + 4 * beta1 * (W * (alpha1 + beta1 * W) + egain - SpawnE))
        if(is.na(flagvalue1)) {
          prt.msg(nR, i, W, Temperature_i, p, outpt, 1, 1)
          warning("fv1: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
        } else if(flagvalue1 < 0) {
          prt.msg(nR, i, W, Temperature_i, p, outpt, 2, 2)
          warning("fv1: Number inside sqrt is negative. Fish lost too much weight.")
        }
        finalwt <- (-alpha1 + sqrt(alpha1 * alpha1 + 4 * beta1 * (W * (alpha1 + beta1 * W) + egain - SpawnE))) / (2 * beta1)
      } else if(beta1 == 0) {
        # Can't use quadratic formula; ED = alpha1
        finalwt <- (egain - SpawnE + W * alpha1) / alpha1
      }

      # Check if new weight crosses cutoff
      if(finalwt > Wco) {
        egainCo <- Wco * (alpha1 + beta1 * Wco) - W * (alpha1 + beta1 * W)
        if(beta2 != 0) {
          flagvalue2 <- (alpha2 * alpha2 + 4 * beta2 * (egain - SpawnE - egainCo + Wco * (alpha1 + beta1 * Wco)))
          if(is.na(flagvalue2)) {
            prt.msg(nR, i, W, Temperature_i, p, outpt, 1, 3)
            warning("fv2: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
          } else if(flagvalue2 < 0) {
            prt.msg(nR, i, W, Temperature_i, p, outpt, 2, 4)
            warning("fv2: Number inside sqrt is negative. Fish lost too much weight.")
          }
          finalwt <- (-alpha2 + sqrt(alpha2 * alpha2 + 4 * beta2 * (egain - SpawnE - egainCo + Wco * (alpha1 + beta1 * Wco)))) / (2 * beta2)
        } else if(beta2 == 0) {
          finalwt <- (egain - SpawnE - egainCo + Wco * (alpha1 + beta1 * Wco)) / alpha2
        }
      }

    } else if(W >= Wco) {
      # Weight at start of day is above cutoff
      if(beta2 != 0) {
        # Use quadratic formula to calc finalwt
        flagvalue3 <- (alpha2 * alpha2 + 4 * beta2 * (W * (alpha2 + beta2 * W) + egain - SpawnE))
        if(is.na(flagvalue3)) {
          prt.msg(nR, i, W, Temperature_i, p, outpt, 1, 5)
          warning("fv3: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
        } else if(flagvalue3 < 0) {
          prt.msg(nR, i, W, Temperature_i, p, outpt, 2, 6)
          warning("fv3: Number inside sqrt is negative. Fish lost too much weight.")
        }
        finalwt <- (-alpha2 + sqrt(alpha2 * alpha2 + 4 * beta2 * (W * (alpha2 + beta2 * W) + egain - SpawnE))) / (2 * beta2)
      } else if(beta2 == 0) {
        # Can't use quadratic formula; ED = alpha2
        finalwt <- (egain - SpawnE + W * alpha2) / alpha2
      }

      # Check if new weight decreases below cutoff
      if(finalwt < Wco) {
        elossCo <- W * (alpha2 + beta2 * W) - Wco * (alpha1 + beta1 * Wco)
        if(beta1 != 0) {
          flagvalue4 <- (alpha1 * alpha1 + 4 * beta1 * (egain - SpawnE + elossCo + Wco * (alpha1 + beta1 * Wco)))
          if(is.na(flagvalue4)) {
            prt.msg(nR, i, W, Temperature_i, p, outpt, 1, 7)
            warning("fv4: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
          } else if(flagvalue4 < 0) {
            prt.msg(nR, i, W, Temperature_i, p, outpt, 2, 8)
            warning("fv4: Number inside sqrt is negative. Fish lost too much weight.")
          }
          partwt1 <- sqrt(alpha1 * alpha1 + 4 * beta1 * (egain - SpawnE + elossCo + Wco * (alpha1 + beta1 * Wco)))
          finalwt <- (-alpha1 + partwt1) / (2 * beta1)
        } else if(beta1 == 0) {
          finalwt <- (egain - SpawnE + elossCo + Wco * alpha1) / alpha1
        }
      }
    }

    Pred_E_iplusone <- pred_En_D(W = finalwt, day = i, PREDEDEQ = PREDEDEQ)

  } else if(PREDEDEQ == 1) {
    # PREDEDEQ 1: Use daily interpolated values from input file
    Pred_E_iplusone <- pred_En_D(W = W, day = (i + 1), PREDEDEQ = PREDEDEQ)
    finalwt <- (egain - SpawnE + (Pred_E_i * W)) / Pred_E_iplusone
  }

  return(list(finalwt = finalwt, Pred_E_iplusone = Pred_E_iplusone))
}

initialize_output_dataframe <- function(Fin, globalout_Prey) {
  ### Create blank output dataframe with proper column names
  ###
  ### Parameters:
  ###   Fin: Number of days in simulation
  ###   globalout_Prey: Prey proportion data (for column naming)
  ###
  ### Returns: Empty dataframe with all column names set

  globalout <- data.frame(matrix(NA, nrow = Fin, ncol = (53 + ncol(globalout_Prey) * 4)))
  colnames(globalout) <- c(
    "Day",
    "Temperature.C",
    "Starting.Weight",
    "Weight.g",
    "Population.Number",
    "Population.Biomass.g",
    "Specific.Growth.Rate.J.g.d",
    "Specific.Consumption.Rate.J.g.d",
    "Specific.Egestion.Rate.J.g.d",
    "Specific.Excretion.Rate.J.g.d",
    "Specific.Respiration.Rate.J.g.d",
    "Specific.SDA.Rate.J.g.d",
    "Specific.Consumption.Rate.g.g.d",
    "Specific.Growth.Rate.g.g.d",
    "Initial.Predator.Energy.Density.J.g",
    "Final.Predator.Energy.Density.J.g",
    "Mean.Prey.Energy.Density.J.g",
    "Gross.Production.g",
    "Gross.Production.J",
    "Cum.Gross.Production.g",
    "Cum.Gross.Production.J",
    "Gametic.Production.g",
    "Cum.Gametic.Production.J",
    "Net.Production.g",
    "Net.Production.J",
    "Cum.Net.Production.g",
    "Cum.Net.Production.J",
    "Consumption.g",
    "Consumption.J",
    "Cum.Cons.g",
    "Cum.Cons.J",
    "Cons.Pop.g",
    "Cons.Pop.J",
    "Cum.Cons.Pop.g",
    "Cum.Cons.Pop.J",
    "Mortality.number",
    "Mortality.g",
    "Nitrogen.Egestion.g",
    "Phosphorous.Egestion.g",
    "N.to.P.Egestion",
    "Nitrogen.Excretion.g",
    "Phosphorous.Excretion.g",
    "N.to.P.Excretion",
    "Nitrogen.Consumption.g",
    "Phosphorous.Consumption.g",
    "N.to.P.Consumption",
    "Nitrogen.Growth.g",
    "Phosphorous.Growth.g",
    "N.to.P.Growth",
    "Contaminant.Clearance.Rate.ug.d",
    "Contaminant.Uptake.ug",
    "Contaminant.Burden.ug",
    "Contaminant.Predator.Concentration.ug.g",
    paste("Cons", colnames(globalout_Prey), "J", sep = " "),
    paste("Cons", colnames(globalout_Prey), "g", sep = " "),
    paste("Cons Pop", colnames(globalout_Prey), "J", sep = " "),
    paste("Cons Pop", colnames(globalout_Prey), "g", sep = " ")
  )

  return(globalout)
}
