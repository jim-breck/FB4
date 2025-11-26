########################################################################
### Contaminant Accumulation Functions
########################################################################
#
# This module contains functions for modeling contaminant accumulation in fish.
#
# Three contaminant equation options (CONTEQ):
#   CONTEQ 1: Food uptake only, no elimination, no water uptake
#   CONTEQ 2: Food uptake with elimination (MeHg model, Trudel & Rasmussen 1997)
#   CONTEQ 3: Food and water uptake with elimination (Arnot & Gobas 2004)
#
########################################################################

pred_cont_conc_old <- function(C,W,Temperature,X_Prey,X_Pred,TEx,X_ae,CONTEQ) {
  ### Legacy contaminant accumulation function
  ### Used in testing version of FB4; only two CONTEQ options
  ### Returns: c(Clearance, Uptake, Burden, X_Pred)

  Burden <- X_Pred*W
  Kx <- ifelse(CONTEQ==2, exp(0.066*Temperature-0.2*log(W)-6.56)/1.5, 0)
  Uptake <- ifelse(CONTEQ == 1,sum(C*X_Prey*TEx),sum(C*X_Prey*X_ae))
  Clearance <- ifelse(CONTEQ==2,Kx*Burden,0)
  Accumulation <- Uptake-Clearance
  Burden <- ifelse(CONTEQ == 1,Burden+Uptake,Burden+Accumulation)
  X_Pred <- Burden/W  # Caution! Should calculate new Conc using "finalwt", not W; JEB
  return(c(Clearance, Uptake, Burden, X_Pred))
}

pred_cont_conc <- function(R.O2,C,W,Temperature,X_Prey,X_Pred,TEx,X_ae,Ew,Kbw,CONTEQ) {
  ### Contaminant accumulation function with three equation options
  ### Includes CONTEQ 3 for Arnot & Gobas (2004) model
  ###
  ### Parameters:
  ###   R.O2: Respiration rate (g O2/g/d)
  ###   C: Consumption by prey type (g/d)
  ###   W: Fish weight (g) at start of day
  ###   Temperature: Water temperature (C)
  ###   X_Prey: Contaminant concentration in prey (ug/g)
  ###   X_Pred: Contaminant concentration in predator (ug/g) at start of day
  ###   TEx: Transfer efficiency
  ###   X_ae: Assimilation efficiency
  ###   Ew: Gill chemical uptake efficiency
  ###   Kbw: Fish:water partition coefficient
  ###   CONTEQ: Contaminant equation number (1, 2, or 3)
  ###
  ### Returns: c(Clearance, Uptake, Burden, NA)
  ###   NA is placeholder for X_Pred to be calculated later using final weight

  Burden <- X_Pred*W  # Burden in micrograms; This uses W and X_Pred at **start** of day

  if(CONTEQ==1) {
    Uptake <- sum(C*X_Prey*TEx)   # uptake from food only; no elimination
    Kx <- 0  # no elimination

  } else if(CONTEQ==2) {
    Uptake <- sum(C*X_Prey*X_ae)  # uptake from food only (no uptake from water)
    Kx <- exp(0.066*Temperature-0.2*log(W)-6.56)  # MeHg elimination rate coeff; Trudel & Rasmussen (1997)

  } else if(CONTEQ==3) {
    # Arnot & Gobas (2004) model with water and food uptake
    VOx = 1000*R.O2  # (mg O2/g/d) = (1000 mg/g)*(g O2/g/d), where R.O2 is (g O2/g/d)
    COx = (-0.24*Temperature +14.04)*DO_Sat.fr  # dissolved oxygen concentration (mg O2/L); (eq.9)
    K1 = Ew*VOx/COx  # water cleared of contaminant per g per day; (L/g/day), proportional to Resp
    Uptake.water = W*K1*phi_DT*Cw_tot*1000  # contam from water (ug/d); (L/day)*(mg/L)*(1000 ug/mg)
    Uptake.food  = sum(C*X_Prey*X_ae)  # contam in all food eaten (ug/d); (g/d)*(ug/g)
    Uptake = Uptake.water + Uptake.food  # (ug/d)
    Kx = K1/Kbw  # clearance rate is proportional to K1 and to 1/fish:water partition coefficient
  }

  Clearance <- Kx*Burden
  Accumulation <- Uptake - Clearance  # Accumulation = net change in Burden
  Burden = Burden + Accumulation  # update Burden
  #X_Pred <- Burden/W  # update predator contaminant concentration using new Burden & FINALWEIGHT!
  return(c(Clearance,Uptake,Burden,NA))  # Use NA to save a place for X_Pred
}
