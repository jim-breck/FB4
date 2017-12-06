require(shiny)

# Bioenergetics parameters, by species
parms <- read.csv("Parameters_official.csv",stringsAsFactors = FALSE) #  Read parameter values from .csv file

#    Main Input files
Temperature_File = "Main Inputs/Temperature.csv" # Temperature (deg C), over time
Diet_prop_File   = "Main Inputs/Diet_prop.csv"   # Diet proportions, by prey type, over time
Prey_E_File      = "Main Inputs/Prey_E.csv"      # Energy density, by prey type, over time
Indigestible_Prey_File = "Main Inputs/Indigestible_Prey.csv" # Fraction indigestible, by prey type, over time
Predator_E_File  = "Main Inputs/Pred_E.csv"      # Predator energy density, over time; only read if PREDEDEQ == 1
#   Mortality
Mortality_File   = "Sub-Models/Mortality/Mortality.csv" # Mortality during time intervals
#   Reproduction
Reproduction_File= "Sub-Models/Reproduction/Reproduction.csv" # Day(s) and fraction wt lost spawning
#   Contaminants    # Only read if calc.contaminant == TRUE
Prey_conc_File       = "Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv" # Contam conc in prey, by prey type, over time
Contam_assim_File    = "Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv" # Fraction of contaminant assimilated by predator, from each prey type, over time
Contam_trans_eff_File= "Sub-Models/Contaminant Accumulation/Transfer Efficiency.csv"  # Transfer efficiency of contam from prey to predator, by prey type, over time
#   Phosphorus      # Only read if calc.nut == TRUE
Phos_Ae_File        = "Sub-Models/Nutrient Regeneration/Phos_Ae.csv" # Predator's Phos assimilation efficiency, by prey type, over time
Phos_Conc_Pred_File = "Sub-Models/Nutrient Regeneration/Phos_Conc_Pred.csv" # Phos in predator (g Phos/g), over time
Phos_Conc_Prey_File = "Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv" # Phos in prey (g Phos/g), by prey type, over time
#   Nitrogen        # Only read if calc.nut == TRUE
Nit_Ae_File         = "Sub-Models/Nutrient Regeneration/Nit_Ae.csv" # Predator's N assimilation efficiency, by prey type, over time
Nit_Conc_Pred_File  = "Sub-Models/Nutrient Regeneration/Nit_Conc_Pred.csv" # N in predator (g N/g), over time
Nit_Conc_Prey_File  = "Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv" # N in prey (g N/g), by prey type, over time

shinyServer(function(input, output,session) {
  
  Model <- reactive({
  
  ########################################################################
  ### Initial conditions
  ########################################################################
  
  Sp <- input$spec  # Select the species of interest by changing the number to the corresponding row number
  
  fit.to  <- print(input$fitto)       ### Fit to either final weight, consumption, ration, or p-value
  
  Init_W  <- input$InW                ### Initial weight in g wet weight
  
  Final_W  <- input$FinW              ### Final weight in g wet weight
  
  eaten <- input$FinW                 ### Total food eaten in g wet weight
  
  Ration_prey <- input$FinW           ### Daily food eaten in g wet weight
  
  Ration <- input$FinW/100            ### % of body weight eaten
  
  p_value <- input$FinW               ### Proporation of Cmax eaten
  
  First_day   <- input$ID             ### First day of the simulation
  
  Last_day    <- input$FD             ### Last day of the simulation
  
  Fin         <- Last_day-First_day+1 ### Number of days in simulation
  
  Ind         <- input$ind            ### Number of individuals in the population
  
  Oxycal <- input$oxycal              ### Oxycalorific coefficient
  
  calc.nut <- input$nut               ### Do nutrient calcs? (TRUE/FALSE)
  # calc.nut is used to avoid reading the six nutrient files if not used

  calc.contaminant <- input$contaminant  ### Do contaminant calcs? (TRUE/FALSE)
  # calc.contaminant is used to avoid reading the three contaminant files if not used

  # Could add flag calc.mortality to use to avoid reading Mortality_File if not used
  # Could add flag calc.PRED_E to use to avoid reading Predator_E_File unless PREDEDEQ == 1
  # Could add flag calc.reproduction to use to avoid reading Reproduction_File if not used
    
  ########################################################################
  ### Consumption parameters 
  ########################################################################
  
  CEQ <- parms[Sp,"CEQ"] ### equation used 
  CA  <- parms[Sp,"CA"]  ### intercept for the allometric mass function
  CB  <- parms[Sp,"CB"]  ### slope for the allometric mass function
  CQ  <- parms[Sp,"CQ"]  ### water temperature dependent coefficient of consumption
  CTO <- parms[Sp,"CTO"] ### laboratory temperature preferendum
  CTM <- parms[Sp,"CTM"] ### maximum water temperature above which consumption ceases
  CTL <- parms[Sp,"CTL"] ### temperature at which dependence is some reduced fraction (CK4) of the max. rate
  CK1 <- parms[Sp,"CK1"] ### small fraction of the maximum rate
  CK4 <- parms[Sp,"CK4"] ### reduced fraction of the maximum rate
  
  if(CEQ == 2) {      ### Additional parameters neded if the consumption model 2 is used
    CY <- log(CQ) * (CTM - CTO + 2)
    CZ <- log(CQ) * (CTM - CTO)
    CX <- (CZ^2 * (1+(1+40/CY)^0.5)^2)/400
  }
  
  if(CEQ == 3) {      ### Additional parameters neded if the consumption model 3 is used
    CG1 = (1/(CTO-CQ))*log((0.98*(1-CK1))/(CK1*0.02))
    CG2 = (1/(CTL-CTM))*log((0.98*(1-CK4))/(CK4*0.02))
  }
  
  ########################################################################
  ### Respiration parameters 
  ########################################################################
  
  REQ  <-  parms[Sp,"REQ"] ### equation used    
  RA   <-  parms[Sp,"RA"]  ### specific weight of oxygen consumed by a 1 g fish at 0°C and zero swimming speed
  RB   <-  parms[Sp,"RB"]  ### slope of the allometric mass function for standard metabolism
  RQ   <-  parms[Sp,"RQ"]  ### Q10 rate at which the function increases over relatively low temperatures
  RTO  <-  parms[Sp,"RTO"] ### optimum temperature for respiration
  RTM  <-  parms[Sp,"RTM"] ### maximum (lethal) water temperature
  RTL  <-  parms[Sp,"RTL"] ### cutoff temperature at which the activity relationship changes
  RK1  <-  parms[Sp,"RK1"] ### intercept for the swimming speed above the cutoff temperature
  RK4  <-  parms[Sp,"RK4"] ### mass dependence coefficient for swimming speed at all water temperatures
  RK5  <-  parms[Sp,"RK5"] ### temperature-dependent coefficient for swimming speed 
  ACT  <-  parms[Sp,"ACT"] ### activity multiplier
  BACT <-  parms[Sp,"BACT"] ### water temperature dependence coefficient of swimming speed at water temperature below RTL
  SDA  <-  parms[Sp,"SDA"] ### specific dynamic action
  
  if(REQ == 2) {        ### Additional parameters neded if the respiration model 2 is used
    RY <- log(RQ) * (RTM - RTO + 2)
    RZ <- log(RQ) * (RTM - RTO)
    RX <- (RZ^2 * (1+(1+40/RY)^0.5)^2)/400
  }   
  
  ########################################################################
  ### Egestion and Excretion parameters 
  ########################################################################
  
  EGEQ   <-  parms[Sp,"EGEQ"] ### egestion equation used
  FA     <-  parms[Sp,"FA"] ### egestion
  FB     <-  parms[Sp,"FB"] ### coefficient of water temperature dependence of egestion
  FG     <-  parms[Sp,"FG"] ### coefficient for feeding level dependence (P-value) of egestion
  EXEQ   <-  parms[Sp,"EXEQ"] ### excretion equation used
  UA     <-  parms[Sp,"UA"] ### excretion
  UB     <-  parms[Sp,"UB"] ### coefficient of water temperature dependence of excretion
  UG     <-  parms[Sp,"UG"] ### coefficient for feeding level dependence (P-value) of excretion
  
  ########################################################################
  ### Predator Energy Density 
  ########################################################################
  
  PREDEDEQ <- parms[Sp,"PREDEDEQ"] ### equation used
  PREDED   <- parms[Sp,"ED"]     ### predator energy density
  alpha1   <- parms[Sp,"Alpha1"] ### intercept for the allometric mass function for first size range
  beta1    <- parms[Sp,"Beta1"]  ### slope of the allometric mass function for first size range
  cutoff   <- parms[Sp,"Cutoff"] ### end of first size range 
  alpha2   <- parms[Sp,"Alpha2"] ### intercept for the allometric mass function for second size range
  beta2    <- parms[Sp,"Beta2"]  ### slope of the allometric mass function for second size range
  
  ########################################################################
  ### Consumption models
  ########################################################################
  
  Cf1T <- function(Temperature) { ### Temperature function equation 1 (Hanson et al. 1997; equation from Stewart et al. 1983)
    ft <- exp(CQ*Temperature)
    return(ft)
  }
  
  Cf2T <- function(Temperature) { ### Temperature function equation 2 (Hanson et al. 1997; equation from Kitchell et al. 1977)
    if (Temperature < CTM) { 
      V <- (CTM - Temperature) / (CTM - CTO)
      ft <- V^CX * exp(CX * (1 - V))
    } else if (Temperature >=CTM) {ft  <-  0}  

    if(ft < 0) {ft  <-  0}  ## prevent negative values
    return(ft)
  }
  
  Cf3T <- function(Temperature) { ### Temperature function equation 3 (Hanson et al. 1997; equation from Thornton and Lessem 1978)
    L1 <- exp(CG1*(Temperature-CQ))
    KA <- (CK1*L1) / (1 + CK1*(L1-1))
    L2 <- exp(CG2*(CTL-Temperature))
    KB <- (CK4*L2) / (1 + CK4*(L2-1))
    ft <- KA * KB
    return(ft)
  }
  
  Cf4T <- function(Temperature) { ### Temperature function equation 4; equation from Bevelhimer et al. 1985 )
    ft <- exp(CQ*Temperature + CK1*Temperature^2 + CK4*Temperature^3)
    if(ft < 0) {ft  <-  0}  ## prevent negative values; added by JEB
    return(ft)
  }
  
  
  consumption <- function(Temperature, W, p, CEQ) { ### Consumption function
    Cmax <- CA * W ^ CB 
    if(CEQ == 1) {ft = Cf1T(Temperature)   # reformatted to minimize if-tests; JEB
    } else if(CEQ == 2) {ft = Cf2T(Temperature)
    } else if(CEQ == 3) {ft = Cf3T(Temperature)
    } else if(CEQ == 4) {ft = Cf4T(Temperature)}
    
    return(Cmax * p * ft)
  }
  
  ########################################################################
  ### Respiration models 
  ########################################################################
  
  Rf1T <- function(Temperature) { ### Temperature function equation 1 (Hanson et al. 1997; Stewart et al. 1983)
    ft <- exp(RQ*Temperature)
    return(ft)
  }
  
  RACTf1T <- function(W,Temperature) { ### Temperature function equation 1 with activity component (Hanson et al. 1997; Stewart et al. 1983)
    if(Temperature <= RTL) {VEL <- ACT * W ^ RK4 * exp(BACT * Temperature)
    } else if(Temperature >  RTL) {VEL <- RK1 * W ^ RK4 * exp(RK5 * Temperature)}  
    ACTIVITY <- exp(RTO * VEL)
    return(ACTIVITY)
  }
  
  Rf2T <- function(Temperature) { ### Temperature function equation 2 (Hanson et al. 1997; Kitchell et al. 1977)
    if (Temperature< RTM) {
      V <- (RTM - Temperature) / (RTM - RTO)
      ft <- V^RX * exp(RX * (1 - V))
    } else if (Temperature>=RTM) {ft <- 0.000001}

    if(ft < 0) {ft <- 0.000001}  
    return(ft)
  }
    
  respiration <- function(Temperature, W, REQ) { ### Respiration function
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
  
  ########################################################################
  ### Specific dynamic action 
  ########################################################################
  
  SpDynAct <- function(C,Eg) { ### Specific dynamic action function (Hanson et al. 1997)
    S <- SDA *(C-Eg)
    return(S)
  }
  
  ########################################################################
  ### Egestion and Excretion models 
  ########################################################################
  
  egestion1 <- function(C) {
    # egestion
    Eg = FA * C
    return(Eg)
  }
  
  egestion2 <- function(C,Temperature,p) { ### Egestion model from Elliott (1976)
    Eg = FA*(Temperature^FB)*exp(FG*p)*C
    return(Eg)
  }
  
  egestion3 <- function(C,Temperature,p) { ### Egestion model from Stewart et al. (1983)
    PE = FA*(Temperature^FB)*exp(FG*p)
    PFF = sum(globalout_Ind_Prey[i,]*globalout_Prey[i,]) # allows specification of indigestible prey, as proportions
    PF = ((PE-0.1)/0.9)*(1-PFF)+PFF  
    Eg = PF*C
    return(Eg)
  }
  
  egestion4 <- function(C,Temperature) { ### Egestion model from Elliott (1976)
    Eg = FA*(Temperature^FB)*C
    return(Eg)
  }
  
  egestion <- function(C, Temperature, p, EGEQ) {
    if(EGEQ == 1) {Eg <- egestion1(C)
    } else if(EGEQ == 2) {Eg <- egestion2(C,Temperature,p)
    } else if(EGEQ == 3) {Eg <- egestion3(C,Temperature,p)
    } else if(EGEQ == 4) {Eg <- egestion4(C,Temperature)
    }  # reformatted to minimize if-tests; JEB
    return(Eg)
  }
  
  excretion1 <- function(C, Eg) {
    U = UA * (C - Eg)
    return(U)
  }
  
  excretion2 <- function(C,Temperature,p,Eg) {
    U = UA*(Temperature^UB)*exp(UG*p)*(C-Eg)
    return(U)
  }
  
  excretion3 <- function(C,Temperature,p,Eg) {
    U = UA*(Temperature^UB)*exp(UG*p)*(C-Eg)
    return(U)
  }
  
  excretion4 <- function(C,Temperature,Eg) {
    U = UA*(Temperature^UB)*(C-Eg)
    return(U)
  }
  
  excretion <- function(C, Eg, Temperature, p, EXEQ) {
    if(EXEQ == 1) {U <- excretion1(C,Eg)
    } else if(EXEQ == 2) {U <- excretion2(C,Temperature,p,Eg)
    } else if(EXEQ == 3) {U <- excretion3(C,Temperature,p,Eg)
    } else if(EXEQ == 4) {U <- excretion4(C,Temperature,Eg)
    }  # reformatted to minimize if-checks; JEB
    return(U)
  }
  
  ########################################################################
  ### Temperature 
  ########################################################################

  Temperature <- read.csv(Temperature_File,stringsAsFactors = FALSE) #  Read daily Temp values from .csv file
  Day <- Temperature[,1] # Days
  Temperature <- Temperature[,2]  # Just use the Temp values, which are in column 2
  last_day <- tail(Day, n = 1)  # get the total number of days
  Day_Temp <- approx(Day,Temperature, n = last_day, method="linear")$x
  Day_Temp <- Day_Temp[First_day:Last_day]
  Temperature <- approx(Day,Temperature, n = last_day, method="linear")$y # interpolate temperature data
  Temperature <- Temperature[First_day:Last_day]
  
  ########################################################################
  ### Diet proportions and energetical contribution 
  ########################################################################
  
  Diet_prop <- read.csv(Diet_prop_File,head=TRUE,stringsAsFactors = FALSE)
  Day_prey <- Diet_prop[,1] # Days
  Prey_E <- read.csv(Prey_E_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Prey_E <- Prey_E[,1] # Days
  prey_items   <- (ncol(Diet_prop))-1  # number of prey items in Diet_prop file
  prey_items_E <- (ncol(Prey_E))-1     # number of prey items in Prey_E file
  if(prey_items != prey_items_E) stop("Must have same number of prey items in file Prey_E.csv as in file Diet_prop.csv")
  last_day_prey <- tail(Day_prey, n = 1)  # get the total number of days
  last_day_prey_E <- tail(Day_Prey_E, n = 1)  # get the total number of days
  
  globalout_Prey <- NULL
  globalout_Prey_E <- NULL
  
  for(i in 1:prey_items){
    Prey <- Diet_prop[,i+1]
    Prey <- approx(Day_prey,Prey, n = last_day_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Prey <- cbind(globalout_Prey,Prey)  # Proportion of each prey type in the diet
    Diet_prop <- read.csv(Diet_prop_File,head=TRUE,stringsAsFactors = FALSE)
    Prey_E <- Prey_E[,i+1]  
    Prey_E <- approx(Day_Prey_E,Prey_E, n = last_day_prey_E,method="linear")$y # interpolate prey 1 energy density
    Prey_E <- Prey_E[First_day:Last_day]
    globalout_Prey_E <- cbind(globalout_Prey_E,Prey_E)
    Prey_E <- read.csv(Prey_E_File,head=TRUE,stringsAsFactors = FALSE)
  }
  
  colnames(globalout_Prey) <- names(Diet_prop)[-1]
  colnames(globalout_Prey_E) <- names(Prey_E)[-1]
  # end of diet proportions and energetical contributions section
  
  ########################################################################  
  ### Indigestible Prey 
  ########################################################################
  
  globalout_Ind_Prey <- NULL
  if(EGEQ == 3) {  # Only read file if EGEQ == 3
    # Fraction of each prey type that are indigestible (See Stewart et al. 1983)
    Ind_prey <- read.csv(Indigestible_Prey_File,head=TRUE,stringsAsFactors = FALSE)
    Day_ind_prey <- Ind_prey[,1] # Days
    Ind_prey_items <- (ncol(Ind_prey))-1
    if(prey_items != Ind_prey_items) stop("Must have same number of prey items in file Indigestible_Prey.csv as in file Diet_prop.csv") # JEB
    last_day_ind_prey <- tail(Day_ind_prey, n = 1)  # get the total number of days
    
    for(i in 1:Ind_prey_items){
      Prey <- Ind_prey[,i+1]
      Prey <- approx(Day_ind_prey,Prey, n = last_day_ind_prey,method="linear")$y # interpolate prey 1 energy density
      Prey <- Prey[First_day:Last_day]
      globalout_Ind_Prey <- cbind(globalout_Ind_Prey,Prey)
      Ind_prey <- read.csv(Indigestible_Prey_File,head=TRUE,stringsAsFactors = FALSE)
    }
    colnames(globalout_Ind_Prey) <- names(Ind_prey)[-1]
  }  # end of indigestible prey section
  
  ########################################################################  
  ### Predator energy density 
  ########################################################################
  
  # We only need to read the Predator_E_File and set up daily vectors if PREDEDEQ == 1;
  if(PREDEDEQ == 1) {
    Predator_E <- read.csv(Predator_E_File,head=TRUE,stringsAsFactors = FALSE) 
    Day_pred <- Predator_E[,1] # Days
    Pred_E <- Predator_E[,2]  # Just use the predator energy values, which are in column 2
    last_day_pred <- tail(Day_pred, n = 1)  # get the total number of days + 1
    Dayz_pred <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$x
    Dayz_pred <- Dayz_pred[First_day:Last_day]
    Pred_E <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$y # interpolate temperature data
    Pred_model <- data.frame(Y_Pred=Pred_E[c(last_day_pred-1,last_day_pred)],
                             X_Pred=c(last_day_pred-1,last_day_pred))
    predict_Pred_Eplusone <- lm(Y_Pred ~ X_Pred,data=Pred_model)
    new <- data.frame(X_Pred=last_day_pred+1)
    last_Pred_E <- predict(predict_Pred_Eplusone,new)
    Pred_E <- c(Pred_E,last_Pred_E)
    Pred_E <- Pred_E[First_day:(Last_day+1)]  ## added by JEB
  }
  # Define function for obtaining predator energy density
  pred_En_D <- function(W,day,PREDEDEQ) {    # Find Energy Density (ED, J/g) for this day and W
    if(PREDEDEQ == 1) {return(Pred_E[day])   # Use daily interpolated values from csv file; ignore weight
    } else if(PREDEDEQ == 3) {return(alpha1*W^beta1)  # ED is power function of weight; ignore day
    } else if(PREDEDEQ == 2) {  # Using two line segments, ED is linear function of weight; ignore day
      Wco = as.numeric(cutoff)  # Wco is weight at cutoff, where the line breaks
      if(W <Wco) {return((as.numeric(alpha1) + as.numeric(beta1)*W))}
      if(W>=Wco) {return((as.numeric(alpha2) + as.numeric(beta2)*W))} 
      if(W <Wco && as.numeric(beta1) == 0) {return((as.numeric(alpha1)))}  
      if(W>=Wco && as.numeric(beta2) == 0) {return((as.numeric(alpha2)))}
    }  # restructured using "else if" to reduce if-tests; JEB
  }  # end of predator energy density section
   
  ########################################################################
  ### Mortality
  ########################################################################
  
  Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
  Day_mort <- Mortality[,1] # Days
  mort_types <- (ncol(Mortality))-1
  last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
  globalout_mort <- NULL
  
  for(j in 1:mort_types){
    Mort <- Mortality[,j+1]
    Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate mortality
    Mort <- Mort[First_day:Last_day]
    globalout_mort <- cbind(globalout_mort,Mort)
    Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
  }

colnames(globalout_mort) <- names(Mortality)[-1]
globalout_mort <- data.frame(globalout_mort)
globalout_mort$Nat_Mort_Int <- NA
globalout_mort$Fish_Mort_Int <- NA

days <- nrow(globalout_mort)
intervals <- 1
nat_int_start <- 1
nat_int_dur <- 0
fish_int_start <- 1
fish_int_dur <- 0

for(i in 1:(days-1)){
  nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],nat_int_dur+1,nat_int_dur+1)
  globalout_mort$Nat_Mort_Int[nat_int_start:i] <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],rep(nat_int_dur,nat_int_dur),NA)
  globalout_mort$Nat_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start,NA)
  globalout_mort$Nat_Mort_Int[nat_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start+1,globalout_mort$Nat_Mort_Int[nat_int_start:i])
  nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],0,nat_int_dur)
  nat_int_start <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],i+1,nat_int_start)
  
  fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],fish_int_dur+1,fish_int_dur+1)
  globalout_mort$Fish_Mort_Int[fish_int_start:i] <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],rep(fish_int_dur,fish_int_dur),NA)
  globalout_mort$Fish_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start,NA)
  globalout_mort$Fish_Mort_Int[fish_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start+1,globalout_mort$Fish_Mort_Int[fish_int_start:i])
  fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],0,fish_int_dur)
  fish_int_start <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],i+1,fish_int_start)

}

Individuals <- Ind
globalout_individuals <- NULL

for(i in 1:(Last_day-First_day+1)){  
Z <- exp(log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]+log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]-
  (log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]*log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]))
Individuals <- Individuals*Z
globalout_individuals <- rbind(globalout_individuals,data.frame(day=i,
                                                                individuals=Individuals))

}
globalout_individuals$day <- First_day:Last_day
# end of mortality section

########################################################################
### Reproduction
########################################################################

Reproduction <- read.csv(Reproduction_File,head=TRUE,stringsAsFactors = FALSE)
Day <- Reproduction[,1] # Days
Reproduction <- Reproduction[,2]  # Just use the Temp values, which are in column 2
last_day <- tail(Day, n = 1)  # get the total number of days
Dayz <- approx(Day,Reproduction, n = last_day, method="linear")$x
Dayz <- Dayz[First_day:Last_day]
Reproduction <- approx(Day,Reproduction, n = last_day, method="constant")$y # interpolate temperature data
Reproduction <- Reproduction[First_day:Last_day]
  
########################################################################
### Body Composition
###   as Protein, Lipid, Ash and Water
###   and energy density
########################################################################  
# Fish proximate body composition, based on Breck (2014)
# Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
#
W.0 = Init_W          # initial wet weight (g)
H2O.fr = 0.728        # assumed fraction water (= 0.72 in Stadnicka et al. Table S4)
H2O.g = H2O.fr * W.0  # assumed g of water, wet weight
#
# Estimate g Protein from g Water
Protein = function(H2O) {
  # Regression from Breck (2014), N = 101, r2 = 0.9917
  Pro = 10**(-0.8068 + 1.0750 * log10(H2O))
  return(Pro)
}
# Estimate g Ash from g Water
Ash = function(H2O){
  # Regression from Breck (2014), N = 101, r2 = 0.9932
  A.g = 10**(-1.6765 + 1.0384 * log10(H2O))
  return(A.g)
}
#
# Estimate g Fat from W and g water, g Protein, g Ash, by subtraction
Fat = function(W, H2O, Pro, Ash){
  F.g = W - H2O - Pro - Ash
  return(F.g)
}
Pro.g = Protein(H2O.g)  # estimate from g water
Ash.g = Ash(H2O.g)      # estimate from g water
#
# Estimate g Fat by subtraction
# Fat.g = W - H2O.g - Pro.g - Ash.g
Fat.g = Fat(W.0, H2O.g, Pro.g, Ash.g)

Fat.fr = Fat.g/W.0  # Fat fraction (g Fat/g wet weight)
Pro.fr = Pro.g/W.0  # Protein fraction (g Protein/g wet weight)
Ash.fr = Ash.g/W.0  # Ash fraction (g Ash/g wet weight)
# fraction Protein and Ash, used for contaminant model of Arnot & Gobas (2004): NLOM:non-lipid organic matter
ProAsh.fr = (Pro.g + Ash.g)/W.0  # Protein + Ash fraction (g Protein+Ash/g wet weight)
#
EnDen = function(Fat,Pro,W){
  ED = (Fat.g*36200 + Pro.g*23600)/W  # J/g wet weight
  return(ED)
}
EnDen.est = EnDen(Fat.g,Pro.g,W.0)  # J/g wet weight
# end of body composition

########################################################################
### Contaminant Accumulation 
########################################################################  
# CONTEQ == 1: only food uptake; No clearance, no water uptake; Same as contaminant EQ 1 in FB3.
#
# CONTEQ == 2; only food uptake; Yes to clearance, no water uptake; clearance = f(T,W); 
#    combines EQ 2 & 3 from FB3
#
# CONTEQ == 3; food uptake and water uptake; Yes to clearance; model of Arnot & Gobas (2004)
#    CONTEQ 3 requires several additional input values; these are indicated as input.nnnn below.
#    We use the Respiration model in FB4 to compute R.O2, oxygen consumption (g O2/g/d), in
#    estimating uptake of contaminant from water.
#
#
# default concentrations are (micrograms/g), parts per million
X_Pred <- input$init_pred_conc  # initial contaminant conc in fish (micrograms/g), parts per million
CONTEQ <- input$cont_acc
#
#For testing:
# CONTEQ = 3  # model of Arnot & Gobas (2004)
#
# Parameters and equations for the contaminant model of Arnot & Gobas (2004)
#
# Test model using values from Stadnicka et al. () Supporting Information, Table S2.
# "Pentachlorobenzene"; logKow:5.17; O2:8.87 mg/L; T:15C; W:250 g; time:105 d; Cw:0.0000093 mg/L; Cfinal:0.22 ug/g
# DO = 10.44 mg/L at T=15 C, so DO_Sat = 85.0 % = 100*8.87/10.44
input_cont.name = "Pentachlorobenzene"
cont.name = input_cont.name
#
input_logKow = 5.17  # log10(Octanol:water partition coeff) of the contaminant; test using log Kow = 5.0
logKow = input_logKow
Kow = 10**logKow
#
input_DO_Sat = 85.0   # 85.0 % = 100* 8.87/10.44; Dissolved oxygen saturation (%)
DO_Sat.fr = input_DO_Sat/100  # convert to fraction DO saturation
#
input_Cw_tot = 0.0000093 # total contaminant concentration in water (mg/L) = parts per million
Cw_tot = input_Cw_tot
#
input_phi_DT = 1.0  # fraction of total contam conc in water that is freely dissolved (Cw_dis/Cw_tot)
phi_DT = input_phi_DT
# Arnot & Gobas (2004), (Eq.4), give an alternate estimation for phi_DT:
# phi_DT = 1/(1 + XPOC*DPOC*aPOC*Kow + XDOC*DDOC*aDOC*Kow)  # (eq.4)
#    where XPOC and XDOC are concentrations of POC and DOC in the water (kg/L)
#          DPOC and DDOC are the disequilibrium factors for POC and DOC partitioning
#          aPOC and aDOC are proportionality constants describing phase partitioning relative to octanol
#          aPOC ~ 0.35; aDOC ~ 0.08  # error bars ~ factor of 2.5 for each.
Ew = 1/(1.85 + (155/Kow))  # gill chemical uptake efficiency (eq.6)
# Gv = (1400*Wkg**0.65)/Cox  # ventilation rate (L/d) from resp rate; (eq.8); Note: they don't include T!!
# R.O2 is Oxygen consumption as g O2/g/d, obtained from Respiration as f(T,W)

# Kbw is the fish:water partition coeff
Kbw = Fat.fr*Kow + ProAsh.fr*0.035*Kow + H2O.fr  # Arnot & Gobas (2004, eq. 3)

globalout_Prey_Conc <- NULL
globalout_Prey_ass <- NULL
globalout_Trans_eff <-NULL

if(calc.contaminant==TRUE){
  # only read these contaminant files if calc.contaminant==TRUE
  # Concentrations in fish are micrograms/g, or parts per million
  Prey_conc <- read.csv(Prey_conc_File,head=TRUE,stringsAsFactors = FALSE)
  Day_conc <- Prey_conc[,1] # Days
  Prey_ass <- read.csv(Contam_assim_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Prey_ass <- Prey_ass[,1] # Days
  Trans_eff <- read.csv(Contam_trans_eff_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Trans_eff <- Trans_eff[,1] # Days
  prey_items <- (ncol(Prey_conc))-1
  last_day_conc <- tail(Day_conc, n = 1)  # get the total number of days
  last_day_prey_ass <- tail(Day_Prey_ass, n = 1)  # get the total number of days
  last_day_trans_eff <- tail(Day_Trans_eff, n=1)
  
  for(i in 1:prey_items){
    Prey_Conc <- Prey_conc[,i+1]
    Prey_Conc <- approx(Day_conc,Prey_Conc, n = last_day_conc,method="linear")$y # interpolate prey 1 energy density
    Prey_Conc <- Prey_Conc[First_day:Last_day]
    globalout_Prey_Conc <- cbind(globalout_Prey_Conc,Prey_Conc)
    Prey_conc <- read.csv(Prey_conc_File,head=TRUE,stringsAsFactors = FALSE)
    Prey_ass <- Prey_ass[,i+1]  
    Prey_ass <- approx(Day_Prey_ass,Prey_ass, n = last_day_prey_ass,method="constant")$y # interpolate prey 1 energy density
    Prey_ass <- Prey_ass[First_day:Last_day]
    globalout_Prey_ass <- cbind(globalout_Prey_ass,Prey_ass)
    Prey_ass <- read.csv(Contam_assim_File,head=TRUE,stringsAsFactors = FALSE)
    Trans_eff <- Trans_eff[,i+1]  
    Trans_eff <- approx(Day_Trans_eff,Trans_eff, n = last_day_trans_eff,method="constant")$y # interpolate prey 1 energy density
    Trans_eff <- Trans_eff[First_day:Last_day]
    globalout_Trans_eff <- cbind(globalout_Trans_eff,Trans_eff)
    Trans_eff <- read.csv(Contam_trans_eff_File,head=TRUE,stringsAsFactors = FALSE)
  }

  colnames(globalout_Prey_Conc) <- names(Prey_conc)[-1]
  colnames(globalout_Prey_ass)  <- names(Prey_ass)[-1]
  colnames(globalout_Trans_eff) <- names(Trans_eff)[-1]
}  # end of if(calc.contaminant == TRUE)

pred_cont_conc_old <- function(C,W,Temperature,X_Prey,X_Pred,TEx,X_ae,CONTEQ) {
  # used in testing version of FB4; only two CONTEQ's.
  Burden <- X_Pred*W
  Kx <- ifelse(CONTEQ==2,exp(0.066*Temperature-0.2*log(W)-6.56)/1.5,0)
  Uptake <- ifelse(CONTEQ == 1,sum(C*X_Prey*TEx),sum(C*X_Prey*X_ae))
  Clearance <- ifelse(CONTEQ==2,Kx*Burden,0)
  Accumulation <- Uptake-Clearance
  Burden <- ifelse(CONTEQ == 1,Burden+Uptake,Burden+Accumulation)
  X_Pred <- Burden/W  # Caution! Should calculate new Conc using "finalwt", not W; JEB
  return(c(Clearance,Uptake,Burden,X_Pred))     
}

# Includes CONTEQ 3 for Arnot & Gobas (2004)
pred_cont_conc <- function(R.O2,C,W,Temperature,X_Prey,X_Pred,TEx,X_ae,Ew,Kbw,CONTEQ) {
  Burden <- X_Pred*W  # Burden in micrograms; This uses W and X_Pred at **start** of day
  if(CONTEQ==1) {
    Uptake <- sum(C*X_Prey*TEx)   # uptake from food only; no elimination
    Kx <- 0  # no elimination
  } else if(CONTEQ==2) {
    Uptake <- sum(C*X_Prey*X_ae)  # uptake from food only (no uptake from water)
    Kx <- exp(0.066*Temperature-0.2*log(W)-6.56)  # MeHg elimination rate coeff; Trudel & Rasmussen (1997)
  } else if(CONTEQ==3) {
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
# end of predator contaminant accumulation

########################################################################
### Nutrient Regeneration
########################################################################

if(calc.nut==TRUE){  # only need to read these files if "(calc.nut == TRUE)"
  Phos_Ae <- read.csv(Phos_Ae_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Phos_Ae <- Phos_Ae[,1] # Days
  Phos_Conc_Pred <- read.csv(Phos_Conc_Pred_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Phos_Conc_Pred <- Phos_Conc_Pred[,1] # Days
  Phos_Conc_Prey <- read.csv(Phos_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Phos_Conc_Prey <- Phos_Conc_Prey[,1] # Days
  
  prey_items_nut <- (ncol(Phos_Ae))-1
  
  last_day_Phos_Ae <- tail(Day_Phos_Ae, n = 1)  # get the total number of days
  last_day_Phos_Conc_Pred <- tail(Day_Phos_Conc_Pred, n = 1)  # get the total number of days
  last_day_Phos_Conc_Prey <- tail(Day_Phos_Conc_Prey, n = 1)  # get the total number of days
  
  globalout_Phos_Ae <- NULL
  globalout_Phos_Conc_Pred <- NULL
  globalout_Phos_Conc_Prey <- NULL
  
  Phos_Conc_Pred <- Phos_Conc_Pred[,2]  
  Phos_Conc_Pred <- approx(Day_Phos_Conc_Pred,Phos_Conc_Pred, n = last_day_Phos_Conc_Pred,method="constant")$y # interpolate prey 1 energy density
  Phos_Conc_Pred <- Phos_Conc_Pred[First_day:Last_day]
  globalout_Phos_Conc_Pred <- cbind(globalout_Phos_Conc_Pred,Phos_Conc_Pred)
  
  Nit_Ae <- read.csv(Nit_Ae_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Nit_Ae <- Nit_Ae[,1] # Days
  Nit_Conc_Pred <- read.csv(Nit_Conc_Pred_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Nit_Conc_Pred <- Nit_Conc_Pred[,1] # Days
  Nit_Conc_Prey <- read.csv(Nit_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Nit_Conc_Prey <- Nit_Conc_Prey[,1] # Days
  
  prey_items_nut <- (ncol(Nit_Ae))-1
  
  last_day_Nit_Ae <- tail(Day_Nit_Ae, n = 1)  # get the total number of days
  last_day_Nit_Conc_Pred <- tail(Day_Nit_Conc_Pred, n = 1)  # get the total number of days
  last_day_Nit_Conc_Prey <- tail(Day_Nit_Conc_Prey, n = 1)  # get the total number of days
  
  globalout_Nit_Ae <- NULL
  globalout_Nit_Conc_Pred <- NULL
  globalout_Nit_Conc_Prey <- NULL
  
  Nit_Conc_Pred <- Nit_Conc_Pred[,2]  
  Nit_Conc_Pred <- approx(Day_Nit_Conc_Pred,Nit_Conc_Pred, n = last_day_Nit_Conc_Pred,method="constant")$y # interpolate prey 1 energy density
  Nit_Conc_Pred <- Nit_Conc_Pred[First_day:Last_day]
  globalout_Nit_Conc_Pred <- cbind(globalout_Nit_Conc_Pred,Nit_Conc_Pred)
  
  for(i in 1:prey_items_nut){
    Phos_Ae <- Phos_Ae[,i+1]
    Phos_Ae <- approx(Day_Phos_Ae,Phos_Ae, n = last_day_Phos_Ae,method="linear")$y # interpolate prey 1 energy density
    Phos_Ae <- Phos_Ae[First_day:Last_day]
    globalout_Phos_Ae <- cbind(globalout_Phos_Ae,Phos_Ae)
    Phos_Ae <- read.csv(Phos_Ae_File,head=TRUE,stringsAsFactors = FALSE)
    
    Phos_Conc_Prey <- Phos_Conc_Prey[,i+1]  
    Phos_Conc_Prey <- approx(Day_Phos_Conc_Prey,Phos_Conc_Prey, n = last_day_Phos_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
    Phos_Conc_Prey <- Phos_Conc_Prey[First_day:Last_day]
    globalout_Phos_Conc_Prey <- cbind(globalout_Phos_Conc_Prey,Phos_Conc_Prey)
    Phos_Conc_Prey <- read.csv(Phos_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
    
    Nit_Ae <- Nit_Ae[,i+1]
    Nit_Ae <- approx(Day_Nit_Ae,Nit_Ae, n = last_day_Nit_Ae,method="linear")$y # interpolate prey 1 energy density
    Nit_Ae <- Nit_Ae[First_day:Last_day]
    globalout_Nit_Ae <- cbind(globalout_Nit_Ae,Nit_Ae)
    Nit_Ae <- read.csv(Nit_Ae_File,head=TRUE,stringsAsFactors = FALSE)
    
    Nit_Conc_Prey <- Nit_Conc_Prey[,i+1]  
    Nit_Conc_Prey <- approx(Day_Nit_Conc_Prey,Nit_Conc_Prey, n = last_day_Nit_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
    Nit_Conc_Prey <- Nit_Conc_Prey[First_day:Last_day]
    globalout_Nit_Conc_Prey <- cbind(globalout_Nit_Conc_Prey,Nit_Conc_Prey)
    Nit_Conc_Prey <- read.csv(Nit_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
  }
  
  colnames(globalout_Phos_Ae) <- names(Phos_Ae)[-1]
  colnames(globalout_Phos_Conc_Pred) <- names(Phos_Conc_Pred)[-1]
  colnames(globalout_Phos_Conc_Prey) <- names(Phos_Conc_Prey)[-1]
  colnames(globalout_Nit_Ae) <- names(Nit_Ae)[-1]
  colnames(globalout_Nit_Conc_Pred) <- names(Nit_Conc_Pred)[-1]
  colnames(globalout_Nit_Conc_Prey) <- names(Nit_Conc_Prey)[-1]
}  # end of if(calc.nut == TRUE)

phosphorous_allocation <- function(C,p_conc_prey,AEp,weightgain,p_conc_pred) {
  Cp <- C*p_conc_prey
  Cpsum <- sum(Cp)
  Gp <- weightgain*p_conc_pred
  Up <- sum(AEp*Cp)-Gp
  Fp <- Cpsum-Gp-Up
  return(c(Cpsum,Gp,Up,Fp))
}

nitrogen_allocation <- function(C,n_conc_prey,AEn,weightgain,n_conc_pred) {
  Cn <- C*n_conc_prey
  Cnsum <- sum(Cn)
  Gn <- weightgain*n_conc_pred
  Un <- sum(AEn*Cn)-Gn
  Fn <- Cnsum-Gn-Un
  return(c(Cnsum,Gn,Un,Fn))
}
# end of nutrient regeneration

########################################################################
### Growth Function using p-value
########################################################################

grow <- function(Temperature, W, p, outpt, globalout_Prey, globalout_Prey_E) { # Growth as a function of temperature, weight, p-value, prey proportion and energy density and predator energy density
  TotSpawnE <- 0  # Initialize sum of energy lost in spawning; JEB
  TotConsG  <- 0  # Initialize sum of grams of prey consumed; used for fitting total g consumed; JEB
  #TotEgain  <- 0  # Initialize sum of energy gained in consumption; JEB
  if(outpt != "End") {  # Daily values not needed if only fitting final weight or consumption
    globalout <- data.frame(matrix(NA, nrow = Fin, ncol= (53+ncol(globalout_Prey)*4))) # Create a blank dataframe to store outputs
    colnames(globalout) <- c("Day",
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
                             "Cum.Gametic.Production.J", # need total spawning E; JEB
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
                             paste("Cons",colnames(globalout_Prey),"J", sep = " "),
                             paste("Cons",colnames(globalout_Prey),"g", sep = " "),
                             paste("Cons Pop",colnames(globalout_Prey),"J", sep = " "),
                             paste("Cons Pop",colnames(globalout_Prey),"g", sep = " "))
  }  # end of if(outpt != "End"); JEB
  #
  for(i in 1:Fin) { # Create a loop that estimates growth for the duration of the simulation (Fin)      

    if(input$pop_mort==TRUE){ 
      Ind2 <- globalout_individuals[i,2] # Population mortality
    }else{
      Ind2 <- 1
      Ind <- 1
    }
    
    Pred_E_i <- pred_En_D(W=W,day=i,PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g); only correct for PREDEDEQ==1
    Prey_ED_i <- globalout_Prey[i,]*globalout_Prey_E[i,] # vector of Prey energy densities (J/g) on day i
    mean_prey_ED <- sum(Prey_ED_i) # weighted mean prey energy density (J/g) on day i; JEB
    # Calculate consumption differently if specifying "Ration" or "Ration_prey":
    if(fit.to == "Ration") {
      # Calculate Consumption based on Ration, then use that to find the corresponding p-value for this day
      Cons.gg <- Ration  # units (g prey/g fish) per day
      Cons <- Ration*mean_prey_ED  # (J/g); Ration has units of (g prey)/(g fish); mean_prey_ED has units (J/g prey)
      Cons_p1 <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      p <- Cons/Cons_p1  # Calculated daily p-value for this Ration
    } else if(fit.to == "Ration_prey") {
      # Calculate Consumption based on Ration_prey, then use that to find the corresponding p-value for this day
      Cons.gg <- Ration_prey/W  # units (g food/g fish), where Ration_prey is (g food) per day.
      Cons <- (Ration_prey/W)*mean_prey_ED  # (J/g) = ((g prey)/(g fish))*(J/g prey)
      Cons_p1 <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      p <- Cons/Cons_p1  # Calculated daily p-value for this Ration_prey
    } else { 
      # Now, with p-value determined or specified, calculate Cons for this day
      #Cons.gg is the consumption (g prey/g fish), using T on day i, p-value, Weight, and the specified Consumption Equation number.
      Cons.gg <- consumption(Temperature=Temperature[i], p=p, W=W, CEQ=CEQ)  # (g prey/g fish); only call once with these params; JEB
      # Cons <- consumption(Temperature=Temperature[i],p=p, W=W, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      Cons <- Cons.gg*mean_prey_ED # Cons = Consumption in (J/g) ; units: (J/g) = (g prey/g fish)*(J/g prey)  # JEB
    }
    # record daily values only if needed.
    if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
      # Cons_prey_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in J
      Cons_prey_J <- data.frame(t(Cons.gg*Prey_ED_i*W)) # Consumption by prey type in J  # added by JEB
      colnames(Cons_prey_J) <- paste(colnames(Cons_prey_J),"J", sep = " ")
      # Cons_prey_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in g
      Cons_prey_G <- data.frame(t(Cons.gg*(globalout_Prey[i,]*W))) # Consumption by prey type in g  # added by JEB
      colnames(Cons_prey_G) <- paste(colnames(Cons_prey_G),"g", sep = " ")
      # Cons_prey_pop_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in J
      Cons_prey_pop_J <- data.frame(t(Cons.gg*Prey_ED_i*W*Ind)) # Population consumption by prey in J  # added by JEB
      colnames(Cons_prey_pop_J) <- paste(colnames(Cons_prey_pop_J),"pop.J", sep = " ")
      # Cons_prey_pop_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in g
      Cons_prey_pop_G <- data.frame(t(Cons.gg*(globalout_Prey[i,]*W*Ind))) # Population consumption by prey in g  # added by JEB
      colnames(Cons_prey_pop_G) <- paste(colnames(Cons_prey_pop_G),"pop.g", sep = " ")
    }
    Eg  <- egestion(C=Cons,Temperature=Temperature[i],p=p, EGEQ=EGEQ) # Egestion in J/g
    Ex  <- excretion(C=Cons, Eg=Eg,Temperature=Temperature[i], p=p, EXEQ=EXEQ) # Excretion in J/g
    SpecDA  <- SpDynAct(C=Cons,Eg=Eg) # Specific dynamic action in J/g 
    R.O2  <- respiration(Temperature=Temperature[i], W=W, REQ) # respiration in (g O2/g); used in some contaminant models
    Res <- R.O2*Oxycal  # respiration in (J/g) = (g O2/g)*(j/g O2); Oxycal = 13560 J/g O2
    
    G <-  Cons - (Res + Eg + Ex + SpecDA) # Energy put towards growth in J/g
    
    egain  <-  (G * W)      # net energy gain in J
    #Now account for spawning loss of energy; JEB
    if(input$spawn==TRUE){ # Spawning function
      spawn <- Reproduction[i]
    }else{
      spawn <- 0
    }
    # I think spawning should appear as another daily loss, so use spawn*W*Pred_E_i; JEB
    #   Not spawn at end of day.
    #TotSpawnE <- TotSpawnE + spawn*W*Pred_E_i  # Better to spawn during day i; Total E lost in reproduction; JEB
    #TotSpawnE <- TotSpawnE + spawn*finalwt*Pred_E_i  # Caution! If using finalwt, then use Pred_E_iplusone to balance energy; Total E lost in reproduction; JEB
    SpawnE <- spawn*W*Pred_E_i  # use W at start of day and energy density at start of day; JEB
    TotSpawnE <- TotSpawnE + SpawnE  # Cumulative Total Energy lost in reproduction so far; JEB
    
    # Calculate finalwt = weight at end of day i, and corresponding Predator energy density at end of day i; JEB
    if(PREDEDEQ == 3) {  # calculating ED as power function of weight
      finalwt <- ((egain -SpawnE +(Pred_E_i*W))/alpha1)^(1/(beta1+1))
      Pred_E_iplusone <- pred_En_D(W=finalwt,day=(i),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    }else if(PREDEDEQ == 2){  # estimate ED from weight using one of two line segments
      Wco = as.numeric(cutoff)  # weight at cutoff
      if(W < Wco){    # weight (W) at start of day is below cutoff.
        if(beta1 != 0){  # use quadratic formula to calc finalwt
          finalwt <- (-alpha1 +sqrt(alpha1*alpha1 +4*beta1*(W*(alpha1+beta1*W) +egain -SpawnE)))/(2*beta1)
        }else if(beta1 == 0){  # can't use quadratic formula; ED = alpha1
          finalwt = (egain -SpawnE +W*alpha1)/alpha1
        }
        if(finalwt > Wco){  # if new wt crosses cutoff, recalculate finalwt to correctly account for this
          egainCo = Wco*(alpha1+beta1*Wco) - W*(alpha1+beta1*W)  # energy needed to reach cutoff from W < Wco
          if(beta2 != 0){  # accounting for energy needed to reach cutoff and beyond, calc new wt
            finalwt = (-alpha2 +sqrt(alpha2*alpha2 +4*beta2*(egain-SpawnE -egainCo +Wco*(alpha1+beta1*Wco))))/(2*beta2)
          }else if(beta2 == 0){  # then grow to cutoff, with ED = alpha2 beyond cutoff
            finalwt = (egain-SpawnE -egainCo +Wco*(alpha1+beta1*Wco))/alpha2
          }
        }
      }else if(W >= Wco){   # weight (W) at start of day is above cutoff.
        if(beta2 != 0){  # use quadratic formula to calc finalwt
          flagvalue <- ((alpha2*alpha2 +4*beta2*(W*(alpha2+beta2*W) +egain -SpawnE)))
          if(is.na(flagvalue)){warning("Number inside sqrt is NaN: non a number")
          }else if(flagvalue < 0){warning("Number inside sqrt is negative")}
          finalwt <- (-alpha2 +sqrt(alpha2*alpha2 +4*beta2*(W*(alpha2+beta2*W) +egain -SpawnE)))/(2*beta2)
        }else if(beta2 == 0){  # can't use quadratic formula; ED = alpha1
          finalwt = (egain -SpawnE +W*alpha2)/alpha2
        }
        if(finalwt < Wco){  # if new wt decreases below cutoff, recalculate finalwt to account for this
          elossCo = W*(alpha2+beta2*W) - Wco*(alpha1+beta1*Wco)  # energy loss needed to reach cutoff from W
          if(beta1 != 0){  # accounting for energy to reach cutoff and beyond, calc new weight
            partwt1 = sqrt(alpha1*alpha1 +4*beta1*(egain-SpawnE +elossCo +Wco*(alpha1+beta1*Wco))) # testing
            finalwt = (-alpha1 +sqrt(alpha1*alpha1 +4*beta1*(egain-SpawnE +elossCo +Wco*(alpha1+beta1*Wco))))/(2*beta1)
            testfinalwt = finalwt
          }else if(beta1 == 0){  # then decline to cutoff, with ED = alpha1 below cutoff
            finalwt = (egain-SpawnE +elossCo +Wco*alpha1)/alpha1
          }
        }
      }
      Pred_E_iplusone <- pred_En_D(W=finalwt,day=(i),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    }else if(PREDEDEQ == 1){   # if PREDEDEQ == 1: then use 'day=(i+1)' to interpolate from the input file
      Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g) is from csv file
      finalwt <- (egain -SpawnE +(Pred_E_i*W))/Pred_E_iplusone  # For PREDEDEQ ==1, Pred_E_iplusone
    }

    # finalwt is Predator weight (g) at end of current day
    
    weightgain  <-  finalwt-W  	#change in g/day
    

    #Cons_cont: vector of (g prey consumed), by prey type; 
    #  Note: Cons.gg is now computed (above) for all values of "fit.to"; JEB
    Cons_cont <- (Cons.gg*W)*globalout_Prey[i,]  # vector of (g prey), by prey type; use Cons.gg to avoid call to Consumption(); JEB

    if(calc.nut==TRUE){
      Phos <- phosphorous_allocation(C=Cons_cont,p_conc_prey=globalout_Phos_Conc_Prey[i,],AEp=globalout_Phos_Ae[i,],weightgain=weightgain,p_conc_pred=globalout_Phos_Conc_Pred[i,])
      Nit <- nitrogen_allocation(C=Cons_cont,n_conc_prey=globalout_Nit_Conc_Prey[i,],AEn=globalout_Nit_Ae[i,],weightgain=weightgain,n_conc_pred=globalout_Nit_Conc_Pred[i,])
    } else{
      Phos <- NA
      Nit <- NA
    }
    
    if(calc.contaminant==TRUE){
      #Cont <- pred_cont_conc_old(C=Cons_cont,W=finalwt,Temperature=Temperature,X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,TEx=globalout_Trans_eff[i,],X_ae=globalout_Prey_ass[i,],CONTEQ=CONTEQ)  
      Cont <- pred_cont_conc(R.O2=R.O2,C=Cons_cont,W=W,Temperature=Temperature[i],
                             X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,
                             TEx=globalout_Trans_eff[i,],X_ae=globalout_Prey_ass[i,],
                             Ew=Ew, Kbw=Kbw, CONTEQ=CONTEQ)  
      X_Pred <- Cont[3]/finalwt  # Calc new burden for this day, then calc conc at end of day
      Cont[4] = X_Pred
    } else {
      Cont <- NA
    }

    ConsW <- Cons*W  # units (J) = (J/g)*(g); save a repeated multiplication
    
    if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
      ConsWInd <- ConsW*Ind  # save a repeated multiplication
      globalout[i,"Day"] <- Day_Temp[i]
      globalout[i,"Temperature.C"]<-Temperature[i]
      globalout[i,"Starting.Weight"]<-W
      globalout[i,"Weight.g"]<-finalwt  # spawning loss already accounted for; JEB
      globalout[i,"Population.Number"]<-Ind2
      globalout[i,"Population.Biomass.g"]<-finalwt*Ind
      globalout[i,"Specific.Growth.Rate.J.g.d"]<-G
      globalout[i,"Specific.Consumption.Rate.J.g.d"]<-Cons  # (J/g)
      globalout[i,"Specific.Egestion.Rate.J.g.d"]<-Eg
      globalout[i,"Specific.Excretion.Rate.J.g.d"]<-Ex
      globalout[i,"Specific.Respiration.Rate.J.g.d"]<-Res
      globalout[i,"Specific.SDA.Rate.J.g.d"]<-SpecDA
      globalout[i,"Specific.Consumption.Rate.g.g.d"]<-Cons/mean_prey_ED # (g/g)
      globalout[i,"Specific.Growth.Rate.g.g.d"]<-G/mean_prey_ED
      globalout[i,"Initial.Predator.Energy.Density.J.g"]<-Pred_E_i
      globalout[i,"Final.Predator.Energy.Density.J.g"]<-Pred_E_iplusone
      globalout[i,"Mean.Prey.Energy.Density.J.g"]<-mean_prey_ED  # (J/g)
      globalout[i,"Gross.Production.g"]<-(Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i
      globalout[i,"Gross.Production.J"]<-(Cons +Res + Eg + Ex + SpecDA)*W
      globalout[i,"Cum.Gross.Production.g"]<-cumsum((Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i)
      globalout[i,"Cum.Gross.Production.J"]<-cumsum((Cons +Res + Eg + Ex + SpecDA)*W)
      globalout[i,"Gametic.Production.g"]<-spawn*W  # (g); JEB; was spawn*finalwt
      globalout[i,"Cum.Gametic.Production.J"]<-TotSpawnE # (J); Cumulative energy (J) for spawning; JEB
      globalout[i,"Net.Production.g"]<-weightgain  # includes spawning losses; JEB
      globalout[i,"Net.Production.J"]<-egain  # not including spawning; JEB
      globalout[i,"Cum.Net.Production.g"]<-cumsum(weightgain)
      globalout[i,"Cum.Net.Production.J"]<-cumsum(egain)
      globalout[i,"Consumption.g"]<-ConsW/mean_prey_ED
      globalout[i,"Consumption.J"]<-ConsW
      globalout[i,"Cum.Cons.g"]<-cumsum(ConsW/mean_prey_ED)
      globalout[i,"Cum.Cons.J"]<-cumsum(ConsW)
      globalout[i,"Cons.Pop.g"]<-ConsWInd/mean_prey_ED
      globalout[i,"Cons.Pop.J"]<-ConsWInd
      globalout[i,"Cum.Cons.Pop.g"]<-cumsum(ConsWInd/mean_prey_ED)
      globalout[i,"Cum.Cons.Pop.J"]<-cumsum(ConsWInd)
      globalout[i,"Mortality.number"]<-Ind-Ind2
      globalout[i,"Mortality.g"]<-(Ind-Ind2)*W
      globalout[i,"Nitrogen.Egestion.g"]<-Nit[4]
      globalout[i,"Phosphorous.Egestion.g"]<-Phos[4]
      globalout[i,"N.to.P.Egestion"]<-Nit[4]/Phos[4]
      globalout[i,"Nitrogen.Excretion.g"]<-Nit[3]
      globalout[i,"Phosphorous.Excretion.g"]<-Phos[3]
      globalout[i,"N.to.P.Excretion"]<-Nit[3]/Phos[3]
      globalout[i,"Nitrogen.Consumption.g"]<-Nit[1]
      globalout[i,"Phosphorous.Consumption.g"]<-Phos[1]
      globalout[i,"N.to.P.Consumption"]<-Nit[1]/Phos[1]
      globalout[i,"Nitrogen.Growth.g"]<-Nit[2]
      globalout[i,"Phosphorous.Growth.g"]<-Phos[2]
      globalout[i,"N.to.P.Growth"]<-Nit[2]/Phos[2]
      globalout[i,"Contaminant.Clearance.Rate.ug.d"]<-Cont[1]
      globalout[i,"Contaminant.Uptake.ug"]<-Cont[2]
      globalout[i,"Contaminant.Burden.ug"]<-Cont[3]
      globalout[i,"Contaminant.Predator.Concentration.ug.g"]<-Cont[4]
      globalout[i,paste("Cons",colnames(globalout_Prey),"J", sep = " ")]<-Cons_prey_J
      globalout[i,paste("Cons",colnames(globalout_Prey),"g", sep = " ")] <-Cons_prey_G
      globalout[i,paste("Cons Pop",colnames(globalout_Prey),"J", sep = " ")]<-Cons_prey_pop_J
      globalout[i,paste("Cons Pop",colnames(globalout_Prey),"g", sep = " ")] <-Cons_prey_pop_G
    }
    #globalout<-cbind(globalout,Cons_prey)
    TotConsG  <- TotConsG + ConsW/mean_prey_ED  # Tot g cons; JEB
    # TotSpawnE is total energy lost at spawning; value now calculated earlier and passed in globalout; JEB
    #W <- finalwt-(spawn*finalwt) # spawning loss is accounted for earlier
    W <- finalwt  # Weight at the end of the day serves as the starting weight for the next day
    Ind <- Ind2                
  }
  if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
    globalout[,c("Cum.Gross.Production.g","Cum.Gross.Production.J","Cum.Net.Production.g","Cum.Net.Production.J","Cum.Cons.g","Cum.Cons.J","Cum.Cons.Pop.g","Cum.Cons.Pop.J")] <- 
      cumsum( globalout[,c("Cum.Gross.Production.g","Cum.Gross.Production.J","Cum.Net.Production.g","Cum.Net.Production.J","Cum.Cons.g","Cum.Cons.J","Cum.Cons.Pop.g","Cum.Cons.Pop.J")])
  }
  if(outpt == "vector")                         {return(globalout)} 
  if(outpt == "End" && fit.to=="Weight")        {return(W)} # only ending W value is needed; JEB
  if(outpt == "End" && fit.to=="Consumption")   {return(TotConsG)} # only ending Total Consumption value is needed; JEB
  if(outpt == "final" && fit.to=="Weight")      {return(globalout[Fin,4])} 
  if(outpt == "final" && fit.to=="Consumption") {return(sum(globalout[,'Specific.Consumption.Rate.g.g.d']*globalout[,'Starting.Weight']))}
  if(outpt == "final" && fit.to=="p-value")     {return(sum(globalout[,'Specific.Consumption.Rate.g.g.d']*globalout[,'Starting.Weight']))}
  
}  # end of function grow()
   

########################################################################
### binary search algorithm for p-value 
########################################################################

  p <- 0.3 # Need some value to start the fitting process for p-value
  W.tol  <- 0.0001  # Predicted W must be within W.tol of the specified W for an adequate p-fit.
  max.iter <- 25 # Max number of iterations of binary search (increased from 20 to 25; JEB)
  
  fit.p <- function(p, IW, FW, W.tol, max.iter) {
    W      <- IW    # Initial weight
    n.iter <- 0     # Counter for number of iterations
    p.max  <- 5.00  # current max
    p.min  <- 0.00  # current min
    outpt <- "End"  # desire only ending weight or consumption value, not full vector; revised by JEB
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
    return(p) (g)
  }  # end of fit.p function
   
########################################################################
### Model run 
########################################################################

time.Start <- proc.time() # start clock to time model run # added by JEB

  
if(fit.to=="Ration"){ 
  #W1.p <- grow_ration(Temperature=Temperature,W=Init_W,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p)

}else if(fit.to=="Ration_prey"){ 
  #W1.p <- grow_ration_prey(Temperature=Temperature,W=Init_W,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p
}else if(fit.to=="p-value"){ 
  p <- p_value
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p)
}else{ 
  FW <-ifelse(fit.to=="Weight",Final_W, eaten) # if fit.to=="Consumption", then set FW to the value of eaten
  p <- 0.5  
  p <- fit.p(p,Init_W, FW, W.tol,max.iter) ###  fit.p(Init_W, FW, W.tol)
  outpt <- "vector"
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p) 
  rownames(W1.p) <- NULL
}

time.Elapsed <- proc.time() - time.Start # elapsed time # added by JEB

########################################################################
### Outputs
########################################################################

  output$summary <- renderTable({ 
    Model()
    parms <- read.csv("Parameters_official.csv",stringsAsFactors = FALSE) #  Read parameter values from .csv file
    if(p < 0.0000002){  # Because of binary search method, lowest p-value is not quite zero.
      Pmsg1 <- "Warning! p-value at lower limit. Model could not fit user-specified final weight."
      p.OK = FALSE
    }else if(p >= 4.9999998){  # Because of binary search method, highest p-value is not quite five.
      Pmsg1 <- "Warning! p-value at upper limit. Model could not fit user-specified final weight."
      p.OK = FALSE
    }else{
      Pmsg1 = NA
      p.OK = TRUE
    }
    p.t <- ifelse(input$fitto=="Ration" || input$fitto=="Ration_prey",NA,format(round(p,4), nsmall = 3))
    Growth <- format(round(W1.p[(input$FD-input$ID+1),4],3), nsmall = 3)
    Consom <- format(round(sum(W1.p[,"Specific.Consumption.Rate.g.g.d"]*W1.p[,"Starting.Weight"]),3), nsmall = 3)
    time.Run <- format(round(time.Elapsed[3],3), nsmall = 3) # 3rd value is total elapsed time JEB
    # Check for energy balance; JEB
    ED.start <- pred_En_D(W=Init_W,day=1,PREDEDEQ=PREDEDEQ) # initial predator energy density (J/g) on day 1
    BodyE.start <- Init_W*ED.start # initial total body energy (J); JEB
    W.final  <- W1.p[(input$FD-input$ID+1),4]   # final weight (g); JEB
    ED.final <- W1.p[(input$FD-input$ID+1),16]  # final energy density (J/g); JEB
    BodyE.final <- W.final*ED.final  # Final total body energy (J) = Final weight (g) * final ED (J/g)
    TotEgain <- W1.p[(input$FD-input$ID+1),27]  # total energy gain (J);  JEB
    TotSpawnE <- W1.p[(input$FD-input$ID+1),23]  # total energy lost in spawning (J);  JEB
    # TotSpawnE is total energy lost at spawning; value from globalout; JEB
    EStartGain <- BodyE.start + TotEgain - TotSpawnE  # Initial Body E + Egains - Elosses, all in Joules; JEB
    ErelDiff <- abs(EStartGain - BodyE.final)/BodyE.final  # Relative absolute difference in E budget; JEB
    Etol1 <- 0.000001  # Allowable relative error in energy budget; JEB
    Emsg1 <- ifelse(ErelDiff < Etol1, "OK. Energy budget balanced.", "Warning! Energy budget not balanced!") # JEB
    EB.OK <- ifelse(ErelDiff < Etol1, TRUE, FALSE) # JEB
    E.relDiff.f <- format(round(ErelDiff,10), nsmall = 3)  # relative difference from E balance; JEB
    # end of check for energy balance; JEB
    if(EB.OK) {   # Energy budget balanced within tolerance; JEB
      Emsg1 <- "OK. Energy budget balanced."  # JEB
      Parameter <- c("p-value","Total consumption (g)","Final weight (g)","Run time (s)",Emsg1) # JEB
      Value <- c(p.t, Consom, Growth, time.Run, E.relDiff.f) # added time.Run, E.relDiff, E.Diff.f;  JEB
    } else {      # Energy budget not balanced within tolerance; JEB
      Emsg1 <- "Warning! Energy budget not balanced!"  # JEB
      E.Diff <- BodyE.start + TotEgain - TotSpawnE - BodyE.final  # Difference in E budget; JEB
      E.Diff.f <- format(round(E.Diff,3), nsmall = 3)  # Difference from E balance (J); JEB
      Emsg2 <- ifelse(E.Diff < 0, "Warning! Missing joules in energy budget!", "Warning! Extra joules in energy budget!") # JEB
      Parameter <- c("p-value","Total consumption (g)","Final weight (g)","Run time (s)",Emsg1,Emsg2) # JEB
      Value <- c(p.t, Consom, Growth, time.Run, E.relDiff.f, E.Diff.f) # added time.Run, E.relDiff, E.Diff.f;  JEB
    }    
    if(p.OK == FALSE){
      Parameter = append(Parameter, Pmsg1)
      Value     = append(Value, p.t)
    }
    as.data.frame(cbind(Parameter,Value))
  
  },include.rownames=FALSE)
  
   W1.p 

})

  output$table <- renderTable({
    W1.p <- Model() 
    #cumsum(W1.p[,c("Cum.Gross.Production.g")])
    newdata = data.frame(W1.p[c(seq(1,(input$FD-input$ID+1),input$int)),c(input$var1,input$var2,input$var3,input$var4)]) 
    newdata
  },include.rownames=FALSE)

output$downloadData <- downloadHandler(
  filename = function() { paste("Output", '.csv', sep='') },
  content = function(file) {
    W1.p <- Model()
    write.csv(data.frame(W1.p[c(seq(1,(input$FD-input$ID+1),input$int)),c(input$var1,input$var2,input$var3,input$var4)]), file)
  }
)

output$plot <- renderPlot({
  
  W1.p <- Model()
  op <- par(mar = c(5,4,4,4) + 0.1)
  plot( W1.p[,input$xaxis],W1.p[,input$yaxis],xlim=c(min(W1.p[,input$xaxis]),max(W1.p[,input$xaxis])),
        ylim=c(min(W1.p[,input$yaxis]),max(W1.p[,input$yaxis])),xlab=input$xaxis,ylab=input$yaxis,
        cex.lab=1.5,type="l",col="red")  # added cex.lab=1.5 JEB
  par(new=TRUE)
  plot(W1.p[,input$xaxis], W1.p[,input$yaxis2],type="l",ylim=c(min(W1.p[,input$yaxis2]),max(W1.p[,input$yaxis2])),
       col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext(input$yaxis2,side=4,line=3,cex = par("cex.lab"))
  par(op)
  legend("topleft",col=c("red","blue"),lty=1,legend=c(input$yaxis,input$yaxis2), bty = "n")
  
})

output$parameters <- renderTable({ 
  Sp <- input$spec
    
  Parameter <- c("Species","Scientific name","Reference","Consumption equation","CA","CB","CQ","CTO","CTM","CTL","CK1","CK4",
                 "Respiration equation","RA","RB","RQ","RTO","RTM","RTL","RK1","RK4","RK5","ACT","BACT","SDA","Egestion equation","FA","FB",
                 "FG","Excretion equation","UA","UB","UG","Predator equation","Energy density","Alpha 1","Beta 1","Cutoff","Alpha 2","Beta 2","Notes")
  Value <- c(as.character(parms[Sp,"Species"]),as.character(parms[Sp,"Sci_Name"]),as.character(parms[Sp,"Source"]),parms[Sp,"CEQ"],parms[Sp,"CA"],parms[Sp,"CB"],parms[Sp,"CQ"],parms[Sp,"CTO"],
             parms[Sp,"CTM"],parms[Sp,"CTL"],parms[Sp,"CK1"],parms[Sp,"CK4"],parms[Sp,"REQ"],parms[Sp,"RA"],parms[Sp,"RB"],parms[Sp,"RQ"],parms[Sp,"RTO"],parms[Sp,"RTM"],parms[Sp,"RTL"],
             parms[Sp,"RK1"],parms[Sp,"RK4"],parms[Sp,"RK5"],parms[Sp,"ACT"],parms[Sp,"BACT"],parms[Sp,"SDA"],parms[Sp,"EGEQ"],parms[Sp,"FA"],parms[Sp,"FB"],parms[Sp,"FG"],parms[Sp,"EXEQ"],parms[Sp,"UA"],parms[Sp,"UB"],
             parms[Sp,"UG"],parms[Sp,"PREDEDEQ"],as.character(parms[Sp,"ED"]),parms[Sp,"Alpha1"],parms[Sp,"Beta1"],parms[Sp,"Cutoff"],parms[Sp,"Alpha2"],parms[Sp,"Beta2"],as.character(parms[Sp,"Notes"]))
  as.data.frame(cbind(Parameter,Value))  
},include.rownames=FALSE)

output$temp <- renderPlot({
  First_day   <- input$ID             ### First day of the simulation
  Last_day    <- input$FD             ### Last day of the simulation
  Temperature <- read.csv(Temperature_File,stringsAsFactors = FALSE) #  Read daily Temp values from .csv file
  Day <- Temperature[,1] # Days
  Temperature <- Temperature[,2]  # Just use the Temp values, which are in column 2
  last_day <- tail(Day, n = 1)  # get the total number of days
  Dayz <- approx(Day,Temperature, n = last_day, method="linear")$x
  Dayz <- Dayz[First_day:Last_day]
  Temperature <- approx(Day,Temperature, n = last_day, method="linear")$y # interpolate temperature data
  Temperature <- Temperature[First_day:Last_day]
  plot(Dayz,Temperature,type="l",xlab="Day",ylab="Temperature (°C)")
})

output$diet_prop <- renderPlot({
  First_day   <- input$ID             ### First day of the simulation
  Last_day    <- input$FD             ### Last day of the simulation
  Diet_prop <- read.csv(Diet_prop_File,head=TRUE,stringsAsFactors = FALSE)
  Day_prey <- Diet_prop[,1] # Days
  prey_items <- (ncol(Diet_prop))-1
  last_day_prey <- tail(Day_prey, n = 1)  # get the total number of days
  globalout_Prey <- NULL
  for(i in 1:prey_items){
    Prey <- Diet_prop[,i+1]
    Prey <- approx(Day_prey,Prey, n = last_day_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Prey <- cbind(globalout_Prey,Prey)
    Diet_prop <- read.csv(Diet_prop_File,head=TRUE,stringsAsFactors = FALSE)
  }
  colnames(globalout_Prey) <- names(Diet_prop)[-1]
  plot(First_day:Last_day,globalout_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(0),max(1)),xlab="Day",ylab="Diet Proportion")
  if(prey_items>=2){ 
    for(i in 2:prey_items){
      lines(First_day:Last_day,globalout_Prey[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items+1),lty=1,legend=colnames(globalout_Prey), bty = "n")
})

output$prey_ED <- renderPlot({
  First_day   <- input$ID             ### First day of the simulation
  Last_day    <- input$FD             ### Last day of the simulation
  Prey_E <- read.csv(Prey_E_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Prey_E <- Prey_E[,1] # Days
  prey_items <- (ncol(Prey_E))-1
  last_day_prey_E <- tail(Day_Prey_E, n = 1)  # get the total number of days
  globalout_Prey_E <- NULL
  for(i in 1:prey_items){
    Prey_E <- Prey_E[,i+1]  
    Prey_E <- approx(Day_Prey_E,Prey_E, n = last_day_prey_E,method="linear")$y # interpolate prey 1 energy density
    Prey_E <- Prey_E[First_day:Last_day]
    globalout_Prey_E <- cbind(globalout_Prey_E,Prey_E)
    Prey_E <- read.csv(Prey_E_File,head=TRUE,stringsAsFactors = FALSE)
  }
  colnames(globalout_Prey_E) <- names(Prey_E)[-1]
  plot(First_day:Last_day,globalout_Prey_E[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Prey_E[,2:(ncol(Prey_E))]),max(Prey_E[,2:(ncol(Prey_E))])),xlab="Day",ylab="Energy Density (J/g)")
  if(prey_items>=2){
    for(i in 2:prey_items){
      lines(First_day:Last_day,globalout_Prey_E[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items+1),lty=1,legend=colnames(globalout_Prey_E), bty = "n")
})

output$indigest_prey <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation
Sp <- input$spec  # Species chosen
EGEQ <- parms[Sp,"EGEQ"] ### equation used for egestion
if(EGEQ == 3) {
  Ind_prey <- read.csv(Indigestible_Prey_File,head=TRUE,stringsAsFactors = FALSE)
  Day_ind_prey <- Ind_prey[,1] # Days
  Ind_prey_items <- (ncol(Ind_prey))-1
  last_day_ind_prey <- tail(Day_ind_prey, n = 1)  # get the total number of days
  globalout_Ind_Prey <- NULL
  for(i in 1:Ind_prey_items){
    Prey <- Ind_prey[,i+1]
    Prey <- approx(Day_ind_prey,Prey, n = last_day_ind_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Ind_Prey <- cbind(globalout_Ind_Prey,Prey)
    Ind_prey <- read.csv(Indigestible_Prey_File,head=TRUE,stringsAsFactors = FALSE)
  }
  colnames(globalout_Ind_Prey) <- names(Ind_prey)[-1]
  plot(First_day:Last_day,globalout_Ind_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Ind_prey[,2:(ncol(Ind_prey))]),max(Ind_prey[,2:(ncol(Ind_prey))])),xlab="Day",ylab="Indigestible Prey (proportion)")
  if(Ind_prey_items>=2){
    for(i in 2:Ind_prey_items){
      lines(First_day:Last_day,globalout_Ind_Prey[,i],type="l",col=i+1)
    }
  }
  legend("topleft",col=2:(Ind_prey_items+1),lty=1,legend=colnames(globalout_Ind_Prey), bty = "n")
  # end of if EGEQ == 3
}else {  # if EGEQ not equal to 3, then
  plot(First_day:Last_day,rep(0,(Last_day-First_day+1)),type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(0,1),xlab="Day",ylab="Indigestible Prey (proportion)")
  text((Last_day+First_day)*0.5,0.8,"Proportion indigestible assumed to be 0.",col="red")
} 
})

output$pred_ED <- renderPlot({
  # First, need to define the Predator Energy Density parameters for the chosen species
  Sp <- input$spec  # Species chosen
  PREDEDEQ <- parms[Sp,"PREDEDEQ"] ### equation used
  PREDED   <- parms[Sp,"ED"]     ### predator energy density
  alpha1   <- parms[Sp,"Alpha1"] ### intercept for the allometric mass function for first size range
  beta1    <- parms[Sp,"Beta1"]  ### slope of the allometric mass function for first size range
  cutoff   <- parms[Sp,"Cutoff"] ### end of first size range 
  alpha2   <- parms[Sp,"Alpha2"] ### intercept for the allometric mass function for second size range
  beta2    <- parms[Sp,"Beta2"]  ### slope of the allometric mass function for second size range
  # Define the function used to calculate Predator Energy Density, 
  #   because it is not available to be used for plotting, otherwise; JEB
  # Alternatively, could this function (and others) be defined for use in the entire session?? JEB
  pred_En_D <- function(W,day,PREDEDEQ) {  # function copied directly from earlier in the code; JEB
    if(PREDEDEQ == 1) {return(Pred_E[day])   
    } else if(PREDEDEQ == 3) {return(alpha1*W^beta1)  # Note that ED is nonlinear in W for PREDEQ 3
    } else if(PREDEDEQ == 2) {   # Note that for PREDEQ 2, ED is linear in W, changing slope at cutoff 
      if(W <as.numeric(cutoff)) {return((as.numeric(alpha1) + as.numeric(beta1)*W))}
      if(W>=as.numeric(cutoff)) {return((as.numeric(alpha2) + as.numeric(beta2)*W))} 
      if(W <as.numeric(cutoff) && as.numeric(beta1) == 0) {return((as.numeric(alpha1)))}  
      if(W>=as.numeric(cutoff) && as.numeric(beta2) == 0) {return((as.numeric(alpha2)))}
    }  # restructured to minimize if-tests; JEB
  }
  
  if(PREDEDEQ == 1) {  # Use Predator Energy Density values specified in the csv table; JEB
    First_day   <- input$ID             ### First day of the simulation
    Last_day    <- input$FD             ### Last day of the simulation  
    Predator_E <- read.csv(Predator_E_File,head=TRUE,stringsAsFactors = FALSE) 
    Day_pred <- Predator_E[,1] # Days
    Pred_E <- Predator_E[,2]  # Just use the predator energy values, which are in column 2
    last_day_pred <- tail(Day_pred, n = 1)  # get the total number of days + 1
    Dayz_pred <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$x
    Dayz_pred <- Dayz_pred[First_day:Last_day]
    Pred_E <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$y # interpolate temperature data
    plot(Dayz_pred,Pred_E[First_day:Last_day],type="l",xlab="Day",ylab="Energy Density (J/g)")
  }else if(PREDEDEQ == 2) {
    npt = 3  # number of points. PREDEDEQ 2 is linear in W, so go from 1 to cutoff to Wmax.
    Wco = as.numeric(cutoff)  # Weight at the change in slope for ED
    Wmin = ifelse(Wco > 1, 1, 0.1) # use 1 as the minimum for the plot if cutoff > 1, else 0.1
    Wmax = 1.25*(as.numeric(cutoff))  # Set upper limit of W axis to 1.5*cutoff, to show change in slope
    X.W <- c(Wmin, Wco, Wmax)  # go from Wmin to cutoff to Wmax.
    Y.ED = rep(1, npt)      # create a vector of (3) numbers.
    for(j in 1:npt) {
      Y.ED[j] = pred_En_D(X.W[j], 1, PREDEDEQ)  # compute corresponding Energy Density (J/g)
    }
    plot(X.W, Y.ED, type="l",xlab="Weight (g)",ylab="Energy Density (J/g)", lwd=1.5)
    abline(v = as.numeric(cutoff), lty = "dotted", col = "red") # vertical red dotted line at cutoff
  }else if(PREDEDEQ == 3) {
    n.pm1 = 20  # number of points, minus 1; so n.pm1 = 20 means 21 points will be used.
    X.f = exp(log(1000)/n.pm1)  # Find the X.f value which = 1000 when raised to the 20th power
    X.W <- X.f^(0:n.pm1)        # range of weights at even increments along a log(x) axis.
    Y.ED <- alpha1*X.W^beta1    # corresponding values of Energy Density (J/g)
    plot(X.W, Y.ED, type="l",xlab="Weight (g)",ylab="Energy Density (J/g)", xlog=TRUE, lwd=1.5)
  }
})  # end of plot of Predator Energy Density

output$mort <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation    
Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
Day_mort <- Mortality[,1] # Days
mort_types <- (ncol(Mortality))-1
last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
globalout_mort <- NULL
for(i in 1:mort_types){
  Mort <- Mortality[,i+1]
  Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate prey 1 energy density
  Mort <- Mort[First_day:Last_day]
  globalout_mort <- cbind(globalout_mort,Mort)
  Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
}
colnames(globalout_mort) <- names(Mortality)[-1]
globalout_mort_prob <- NULL
for(i in 1:(Last_day-First_day+1)){
  preys <- 1
  for(j in 1:mort_types){
    preys <- preys*globalout_mort[i,j]
  }
  mort_prob <- sum(globalout_mort[i,1:mort_types])-preys
  globalout_mort_prob <- rbind(globalout_mort_prob,sum(globalout_mort[i,1:mort_types])-preys)              
}  
colnames(globalout_mort_prob) <- "total"
globalout_mort <- cbind(First_day:Last_day,globalout_mort,globalout_mort_prob)
  plot(globalout_mort[1:nrow(globalout_mort),1],globalout_mort[1:nrow(globalout_mort),2],type="l",xlab="Day",ylab="Mortality (probability)",col=2,
       xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(globalout_mort[,2:(ncol(globalout_mort))]),max(globalout_mort[,2:(ncol(globalout_mort))])))
  lines(globalout_mort[1:nrow(globalout_mort),1],globalout_mort[1:nrow(globalout_mort),3],col=3)
  lines(globalout_mort[1:nrow(globalout_mort),1],globalout_mort[1:nrow(globalout_mort),4],col=4)
  legend("topleft",col=2:4,lty=1,legend=colnames(globalout_mort)[-1], bty = "n")
})

output$pop <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation      
Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
Day_mort <- Mortality[,1] # Days
mort_types <- (ncol(Mortality))-1
last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
globalout_mort <- NULL

for(j in 1:mort_types){
  Mort <- Mortality[,j+1]
  Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate mortality
  Mort <- Mort[First_day:Last_day]
  globalout_mort <- cbind(globalout_mort,Mort)
  Mortality <- read.csv(Mortality_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_mort) <- names(Mortality)[-1]
globalout_mort <- data.frame(globalout_mort)
globalout_mort$Nat_Mort_Int <- NA
globalout_mort$Fish_Mort_Int <- NA

days <- nrow(globalout_mort)
intervals <- 1
nat_int_start <- 1
nat_int_dur <- 0
fish_int_start <- 1
fish_int_dur <- 0

for(i in 1:(days-1)){
  nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],nat_int_dur+1,nat_int_dur+1)
  globalout_mort$Nat_Mort_Int[nat_int_start:i] <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],rep(nat_int_dur,nat_int_dur),NA)
  globalout_mort$Nat_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start,NA)
  globalout_mort$Nat_Mort_Int[nat_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start+1,globalout_mort$Nat_Mort_Int[nat_int_start:i])
  nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],0,nat_int_dur)
  nat_int_start <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],i+1,nat_int_start)
  
  fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],fish_int_dur+1,fish_int_dur+1)
  globalout_mort$Fish_Mort_Int[fish_int_start:i] <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],rep(fish_int_dur,fish_int_dur),NA)
  globalout_mort$Fish_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start,NA)
  globalout_mort$Fish_Mort_Int[fish_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start+1,globalout_mort$Fish_Mort_Int[fish_int_start:i])
  fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],0,fish_int_dur)
  fish_int_start <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],i+1,fish_int_start)
  
}

Individuals <- input$ind
globalout_individuals <- NULL

for(i in 1:(Last_day-First_day+1)){  
  Z <- exp(log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]+log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]-
             (log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]*log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]))
  Individuals <- Individuals*Z
  globalout_individuals <- rbind(globalout_individuals,data.frame(day=i,
                                                                  individuals=Individuals))
  
}
globalout_individuals$day <- First_day:Last_day

plot(globalout_individuals[,1],globalout_individuals[,2],type="l",xlab="Day",ylab="Population number",
     xlim=c(min(First_day),max(Last_day)),
     ylim=c(min(globalout_individuals[,2]),max(globalout_individuals[,2])))
})

output$repro <- renderPlot({
  Reproduction <- read.csv(Reproduction_File,head=TRUE,stringsAsFactors = FALSE)
  First_day   <- input$ID             ### First day of the simulation
  Last_day    <- input$FD             ### Last day of the simulation   
  Day <- Reproduction[,1] # Days
  Reproduction <- Reproduction[,2]  # Just use the Temp values, which are in column 2
  last_day <- tail(Day, n = 1)  # get the total number of days
  Dayz <- approx(Day,Reproduction, n = last_day, method="linear")$x
  Dayz <- Dayz[First_day:Last_day]
  Reproduction <- approx(Day,Reproduction, n = last_day, method="constant")$y # interpolate temperature data
  Reproduction <- Reproduction[First_day:Last_day]
  plot(Dayz,Reproduction,type="l",xlab="Day",ylab="Weight Lost to Reproduction (Proportion)")
})

output$cont_ae <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation     
Prey_ass <- read.csv(Contam_assim_File,head=TRUE,stringsAsFactors = FALSE)
Day_Prey_ass <- Prey_ass[,1] # Days
prey_items <- (ncol(Prey_ass))-1
last_day_prey_ass <- tail(Day_Prey_ass, n = 1)  # get the total number of days
globalout_Prey_ass <- NULL

for(i in 1:prey_items){
  Prey_ass <- Prey_ass[,i+1]  
  Prey_ass <- approx(Day_Prey_ass,Prey_ass, n = last_day_prey_ass,method="constant")$y # interpolate prey 1 energy density
  Prey_ass <- Prey_ass[First_day:Last_day]
  globalout_Prey_ass <- cbind(globalout_Prey_ass,Prey_ass)
  Prey_ass <- read.csv(Contam_assim_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Prey_ass) <- names(Prey_ass)[-1]

plot(First_day:Last_day,globalout_Prey_ass[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
     ylim=c(min(Prey_ass[,2:(ncol(Prey_ass))]),max(Prey_ass[,2:(ncol(Prey_ass))])),xlab="Day",ylab="Assimilation Efficiency (proportion)")
if(prey_items>=2){
  for(i in 2:prey_items){
    lines(First_day:Last_day,globalout_Prey_ass[,i],type="l",col=i+1)  
  }
}
legend("topleft",col=2:(prey_items+1),lty=1,legend=colnames(globalout_Prey_ass), bty = "n")
  
})

output$prey_cont_conc <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation       
Prey_conc <- read.csv(Prey_conc_File,head=TRUE,stringsAsFactors = FALSE)
Day_conc <- Prey_conc[,1] # Days
prey_items <- (ncol(Prey_conc))-1
last_day_conc <- tail(Day_conc, n = 1)  # get the total number of days

globalout_Prey_Conc <- NULL

for(i in 1:prey_items){
  Prey_Conc <- Prey_conc[,i+1]
  Prey_Conc <- approx(Day_conc,Prey_Conc, n = last_day_conc,method="linear")$y # interpolate prey 1 energy density
  Prey_Conc <- Prey_Conc[First_day:Last_day]
  globalout_Prey_Conc <- cbind(globalout_Prey_Conc,Prey_Conc)
  Prey_conc <- read.csv(Prey_conc_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Prey_Conc) <- names(Prey_conc)[-1]

  plot(First_day:Last_day,globalout_Prey_Conc[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Prey_conc[,2:(ncol(Prey_conc))]),max(Prey_conc[,2:(ncol(Prey_conc))])),xlab="Day",ylab="Prey Contaminant Concentration (mg/kg)")
  if(prey_items>=2){
    for(i in 2:prey_items){
      lines(First_day:Last_day,globalout_Prey_Conc[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items+1),lty=1,legend=colnames(globalout_Prey_Conc), bty = "n")
})

output$trans_eff <- renderPlot({
  First_day   <- input$ID             ### First day of the simulation
  Last_day    <- input$FD             ### Last day of the simulation      
  Trans_eff <- read.csv(Contam_trans_eff_File,head=TRUE,stringsAsFactors = FALSE)
  Day_Trans_eff <- Trans_eff[,1] # Days
  prey_items <- (ncol(Trans_eff))-1
  last_day_trans_eff <- tail(Day_Trans_eff, n=1)

  globalout_Trans_eff <-NULL
  
  for(i in 1:prey_items){
    Trans_eff <- Trans_eff[,i+1]  
    Trans_eff <- approx(Day_Trans_eff,Trans_eff, n = last_day_trans_eff,method="constant")$y # interpolate prey 1 energy density
    Trans_eff <- Trans_eff[First_day:Last_day]
    globalout_Trans_eff <- cbind(globalout_Trans_eff,Trans_eff)
    Trans_eff <- read.csv(Contam_trans_eff_File,head=TRUE,stringsAsFactors = FALSE)
  }
  colnames(globalout_Trans_eff) <- names(Trans_eff)[-1]
  
  plot(First_day:Last_day,globalout_Trans_eff[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Trans_eff[,2:(ncol(Trans_eff))]),max(Trans_eff[,2:(ncol(Trans_eff))])),xlab="Day",ylab="Transfer Efficiency (Proportion)")
  if(prey_items>=2){
    for(i in 2:prey_items){
      lines(First_day:Last_day,globalout_Trans_eff[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items+1),lty=1,legend=colnames(globalout_Trans_eff), bty = "n")
  
})  
output$phos_ae <- renderPlot({ 
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation     
Phos_Ae <- read.csv(Phos_Ae_File,head=TRUE,stringsAsFactors = FALSE)
Day_Phos_Ae <- Phos_Ae[,1] # Days
prey_items_nut <- (ncol(Phos_Ae))-1
last_day_Phos_Ae <- tail(Day_Phos_Ae, n = 1)  # get the total number of days
globalout_Phos_Ae <- NULL

for(i in 1:prey_items_nut){
  Phos_Ae <- Phos_Ae[,i+1]
  Phos_Ae <- approx(Day_Phos_Ae,Phos_Ae, n = last_day_Phos_Ae,method="linear")$y # interpolate prey 1 energy density
  Phos_Ae <- Phos_Ae[First_day:Last_day]
  globalout_Phos_Ae <- cbind(globalout_Phos_Ae,Phos_Ae)
  Phos_Ae <- read.csv(Phos_Ae_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Phos_Ae) <- names(Phos_Ae)[-1]
 
  plot(First_day:Last_day,globalout_Phos_Ae[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Phos_Ae[,2:(ncol(Phos_Ae))]),max(Phos_Ae[,2:(ncol(Phos_Ae))])),xlab="Day",ylab="Assimilation Efficiency (proportion)")
  if(prey_items_nut>=2){
    for(i in 2:prey_items_nut){
      lines(First_day:Last_day,globalout_Phos_Ae[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items_nut+1),lty=1,legend=colnames(globalout_Phos_Ae), bty = "n")
})

output$pred_phos_conc <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation       
Phos_Conc_Pred <- read.csv(Phos_Conc_Pred_File,head=TRUE,stringsAsFactors = FALSE)
Day_Phos_Conc_Pred <- Phos_Conc_Pred[,1] # Days
last_day_Phos_Conc_Pred <- tail(Day_Phos_Conc_Pred, n = 1)  # get the total number of days
globalout_Phos_Conc_Pred <- NULL
Phos_Conc_Pred <- Phos_Conc_Pred[,2]  
Phos_Conc_Pred <- approx(Day_Phos_Conc_Pred,Phos_Conc_Pred, n = last_day_Phos_Conc_Pred,method="constant")$y # interpolate prey 1 energy density
Phos_Conc_Pred <- Phos_Conc_Pred[First_day:Last_day]
globalout_Phos_Conc_Pred <- cbind(globalout_Phos_Conc_Pred,Phos_Conc_Pred)
colnames(globalout_Phos_Conc_Pred) <- names(Phos_Conc_Pred)[-1]

  plot(First_day:Last_day,globalout_Phos_Conc_Pred[,1],type="l",col=1,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Phos_Conc_Pred),max(Phos_Conc_Pred)),xlab="Day",ylab="Phosphorous Concentration (proportion)")
})

output$prey_phos_conc <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation       
Phos_Conc_Prey <- read.csv(Phos_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
Day_Phos_Conc_Prey <- Phos_Conc_Prey[,1] # Days
prey_items_nut <- (ncol(Phos_Conc_Prey))-1
last_day_Phos_Conc_Prey <- tail(Day_Phos_Conc_Prey, n = 1)  # get the total number of days
globalout_Phos_Conc_Prey <- NULL

for(i in 1:prey_items_nut){
  Phos_Conc_Prey <- Phos_Conc_Prey[,i+1]  
  Phos_Conc_Prey <- approx(Day_Phos_Conc_Prey,Phos_Conc_Prey, n = last_day_Phos_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Phos_Conc_Prey <- Phos_Conc_Prey[First_day:Last_day]
  globalout_Phos_Conc_Prey <- cbind(globalout_Phos_Conc_Prey,Phos_Conc_Prey)
  Phos_Conc_Prey <- read.csv(Phos_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Phos_Conc_Prey) <- names(Phos_Conc_Prey)[-1]

  plot(First_day:Last_day,globalout_Phos_Conc_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Phos_Conc_Prey[,2:(ncol(Phos_Conc_Prey))]),max(Phos_Conc_Prey[,2:(ncol(Phos_Conc_Prey))])),xlab="Day",ylab="Phosphorous Concentration (proportion of wet mass)")
  if(prey_items_nut>=2){
    for(i in 2:prey_items_nut){
      lines(First_day:Last_day,globalout_Phos_Conc_Prey[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items_nut+1),lty=1,legend=colnames(globalout_Phos_Conc_Prey), bty = "n")
})

output$nit_ae <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation     
Nit_Ae <- read.csv(Nit_Ae_File,head=TRUE,stringsAsFactors = FALSE)
Day_Nit_Ae <- Nit_Ae[,1] # Days
prey_items_nut <- (ncol(Nit_Ae))-1
last_day_Nit_Ae <- tail(Day_Nit_Ae, n = 1)  # get the total number of days
globalout_Nit_Ae <- NULL

for(i in 1:prey_items_nut){
  Nit_Ae <- Nit_Ae[,i+1]
  Nit_Ae <- approx(Day_Nit_Ae,Nit_Ae, n = last_day_Nit_Ae,method="linear")$y # interpolate prey 1 energy density
  Nit_Ae <- Nit_Ae[First_day:Last_day]
  globalout_Nit_Ae <- cbind(globalout_Nit_Ae,Nit_Ae)
  Nit_Ae <- read.csv(Nit_Ae_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Nit_Ae) <- names(Nit_Ae)[-1]

  plot(First_day:Last_day,globalout_Nit_Ae[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Nit_Ae[,2:(ncol(Nit_Ae))]),max(Nit_Ae[,2:(ncol(Nit_Ae))])),xlab="Day",ylab="Assimilation Efficiency (proportion)")
  if(prey_items_nut>=2){
    for(i in 2:prey_items_nut){
      lines(First_day:Last_day,globalout_Nit_Ae[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items_nut+1),lty=1,legend=colnames(globalout_Nit_Ae), bty = "n")
})

output$pred_nit_conc <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation    
Nit_Conc_Pred <- read.csv(Nit_Conc_Pred_File,head=TRUE,stringsAsFactors = FALSE)
Day_Nit_Conc_Pred <- Nit_Conc_Pred[,1] # Days
last_day_Nit_Conc_Pred <- tail(Day_Nit_Conc_Pred, n = 1)  # get the total number of days
globalout_Nit_Conc_Pred <- NULL
Nit_Conc_Pred <- Nit_Conc_Pred[,2]  
Nit_Conc_Pred <- approx(Day_Nit_Conc_Pred,Nit_Conc_Pred, n = last_day_Nit_Conc_Pred,method="constant")$y # interpolate prey 1 energy density
Nit_Conc_Pred <- Nit_Conc_Pred[First_day:Last_day]
globalout_Nit_Conc_Pred <- cbind(globalout_Nit_Conc_Pred,Nit_Conc_Pred)
colnames(globalout_Nit_Conc_Pred) <- names(Nit_Conc_Pred)[-1]

  plot(First_day:Last_day,globalout_Nit_Conc_Pred[,1],type="l",col=1,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Nit_Conc_Pred),max(Nit_Conc_Pred)),xlab="Day",ylab="Nitrogen Concentration (proportion of wet mass)")
})

output$prey_nit_conc <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation      
Nit_Conc_Prey <- read.csv(Nit_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
Day_Nit_Conc_Prey <- Nit_Conc_Prey[,1] # Days
prey_items_nut <- (ncol(Nit_Conc_Prey))-1
last_day_Nit_Conc_Prey <- tail(Day_Nit_Conc_Prey, n = 1)  # get the total number of days
globalout_Nit_Conc_Prey <- NULL

for(i in 1:prey_items_nut){
  Nit_Conc_Prey <- Nit_Conc_Prey[,i+1]  
  Nit_Conc_Prey <- approx(Day_Nit_Conc_Prey,Nit_Conc_Prey, n = last_day_Nit_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Nit_Conc_Prey <- Nit_Conc_Prey[First_day:Last_day]
  globalout_Nit_Conc_Prey <- cbind(globalout_Nit_Conc_Prey,Nit_Conc_Prey)
  Nit_Conc_Prey <- read.csv(Nit_Conc_Prey_File,head=TRUE,stringsAsFactors = FALSE)
}

colnames(globalout_Nit_Conc_Prey) <- names(Nit_Conc_Prey)[-1]

  plot(First_day:Last_day,globalout_Nit_Conc_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Nit_Conc_Prey[,2:(ncol(Nit_Conc_Prey))]),max(Nit_Conc_Prey[,2:(ncol(Nit_Conc_Prey))])),xlab="Day",ylab="Nitrogen Concentration (proportion of wet mass)")
  if(prey_items_nut>=2){
    for(i in 2:prey_items_nut){
      lines(First_day:Last_day,globalout_Nit_Conc_Prey[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items_nut+1),lty=1,legend=colnames(globalout_Nit_Conc_Prey), bty = "n")
})
})




