require(shiny)

parms <- read.csv("Parameters_official.csv") #  Read parameter values from .csv file

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
  
  ########################################################################
  ### Consumption parameters 
  ########################################################################
  
  CEQ <- parms[Sp,"CEQ"]  ### equation used 
  CA  <- parms[Sp,"CA"]  ### intercept for the allometric mass function
  CB  <- parms[Sp,"CB"]  ### slope for the allometric mass function
  CQ  <- parms[Sp,"CQ"] ### water temperature dependent coefficient of consumption
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
  RA   <-  parms[Sp,"RA"] ### specific weight of oxygen consumed by a 1 g fish at 0°C and zero swimming speed
  RB   <-  parms[Sp,"RB"] ### slope of the allometric mass function for standard metabolism
  RQ   <-  parms[Sp,"RQ"] ### Q10 rate at which the function increases over relatively low temperatures
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
  PREDED   <- parms[Sp,"ED"] ### predator energy density
  alpha1   <- parms[Sp,"Alpha1"] ### intercept for the allometric mass function for first size range
  beta1    <- parms[Sp,"Beta1"] ### slope of the allometric mass function for first size range
  cutoff   <- parms[Sp,"Cutoff"] ### end of first size range 
  alpha2   <- parms[Sp,"Alpha2"] ### intercept for the allometric mass function for second size range
  beta2    <- parms[Sp,"Beta2"] ### slope of the allometric mass function for second size range
  
  ########################################################################
  ### Consumption models
  ########################################################################
  
  Cf1T <- function(Temperature) { ### Temperature function equation 1 (Hanson et al. 1997; equation from Stewart et al. 1983)
    ft <- exp(CQ*Temperature)
    return(ft)
  }
  
  Cf2T <- function(Temperature) { ### Temperature function equation 2 (Hanson et al. 1997; equation from Kitchell et al. 1977)
    if (Temperature >=CTM) {ft  <-  0}  
    if (Temperature < CTM) { 
      V <- (CTM - Temperature) / (CTM - CTO)
      ft <- V^CX * exp(CX * (1 - V))
    }
    if(ft < 0) {ft  <-  0}
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
    return(ft)
  }
  
  
  consumption <- function(Temperature, W, p, CEQ) { ### Consumption function
    Cmax <- CA * W ^ CB 
    if(CEQ == 1) {ft = Cf1T(Temperature)}     
    if(CEQ == 2) {ft = Cf2T(Temperature)}  
    if(CEQ == 3) {ft = Cf3T(Temperature)}
    if(CEQ == 4) {ft = Cf4T(Temperature)}
    
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
    if(Temperature >  RTL) {VEL <- RK1 * W ^ RK4 * exp(RK5 * Temperature)}  
    if(Temperature <= RTL) {VEL <- ACT * W ^ RK4 * exp(BACT * Temperature)}  
    ACTIVITY <- exp(RTO * VEL)
    return(ACTIVITY)
  }
  
  Rf2T <- function(Temperature) { ### Temperature function equation 2 (Hanson et al. 1997; Kitchell et al. 1977)
    if (Temperature>=RTM) {ft <- 0.000001}  
    if (Temperature< RTM) {
      V <- (RTM - Temperature) / (RTM - RTO)
      ft <- V^RX * exp(RX * (1 - V))
    }
    if(ft < 0) {ft <- 0.000001}  
    return(ft)
  }
    
  respiration <- function(Temperature, W, REQ) { ### Respiration function
    Rmax <- RA * W ^ RB  
    if(REQ == 1) {
      ft <- Rf1T(Temperature)  
      ACTIVITY <- RACTf1T(W,Temperature) 
    }
    if(REQ == 2) {
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
    PFF = sum(globalout_Ind_Prey[i,]*globalout_Prey[i,])
    PF = ((PE-0.1)/0.9)*(1-PFF)+PFF  
    Eg = PF*C
    return(Eg)
  }
  
  egestion4 <- function(C,Temperature) { ### Egestion model from Elliott (1976)
    Eg = FA*(Temperature^FB)*C
    return(Eg)
  }
  
  egestion <- function(C, Temperature, p, EGEQ) {
    if(EGEQ == 1) {Eg <- egestion1(C)}
    if(EGEQ == 2) {Eg <- egestion2(C,Temperature,p)}
    if(EGEQ == 3) {Eg <- egestion3(C,Temperature,p)}
    if(EGEQ == 4) {Eg <- egestion4(C,Temperature)}
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
    if(EXEQ == 1) {U <- excretion1(C,Eg)}
    if(EXEQ == 2) {U <- excretion2(C,Temperature,p,Eg)}
    if(EXEQ == 3) {U <- excretion3(C,Temperature,p,Eg)}
    if(EXEQ == 4) {U <- excretion4(C,Temperature,Eg)}
    return(U)
  }
  
  ########################################################################
  ### Temperature 
  ########################################################################

  Temperature <- read.csv("Main Inputs/Temperature.csv") #  Read daily Temp values from .csv file
  Day <- Temperature[,1] # Days
  Temperature <- Temperature[,2]  # Just use the Temp values, which are in column 2
  last_day <- tail(Day, n = 1)  # get the total number of days
  Dayz <- approx(Day,Temperature, n = last_day, method="linear")$x
  Dayz <- Dayz[First_day:Last_day]
  Temperature <- approx(Day,Temperature, n = last_day, method="linear")$y # interpolate temperature data
  Temperature <- Temperature[First_day:Last_day]
  
  ########################################################################
  ### Diet proportions and energetical contribution 
  ########################################################################
  
  Diet_prop <- read.csv("Main Inputs/Diet_prop.csv",head=TRUE)
  Day_prey <- Diet_prop[,1] # Days
  Prey_E <- read.csv("Main Inputs/Prey_E.csv",head=TRUE)
  Day_Prey_E <- Prey_E[,1] # Days
  prey_items <- (ncol(Diet_prop))-1
  last_day_prey <- tail(Day_prey, n = 1)  # get the total number of days
  last_day_prey_E <- tail(Day_Prey_E, n = 1)  # get the total number of days
  
  globalout_Prey <- NULL
  globalout_Prey_E <- NULL
  
  for(i in 1:prey_items){
    Prey <- Diet_prop[,i+1]
    Prey <- approx(Day_prey,Prey, n = last_day_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Prey <- cbind(globalout_Prey,Prey)
    Diet_prop <- read.csv("Main Inputs/Diet_prop.csv",head=TRUE)
    Prey_E <- Prey_E[,i+1]  
    Prey_E <- approx(Day_Prey_E,Prey_E, n = last_day_prey_E,method="constant")$y # interpolate prey 1 energy density
    Prey_E <- Prey_E[First_day:Last_day]
    globalout_Prey_E <- cbind(globalout_Prey_E,Prey_E)
    Prey_E <- read.csv("Main Inputs/Prey_E.csv",head=TRUE)
  }
  
  colnames(globalout_Prey) <- names(Diet_prop)[-1]
  colnames(globalout_Prey_E) <- names(Prey_E)[-1]
  
  ########################################################################  
  ### Indigestible Prey 
  ########################################################################
  
  Ind_prey <- read.csv("Main Inputs/Indigestible_Prey.csv",head=TRUE)
  Day_ind_prey <- Ind_prey[,1] # Days
  Ind_prey_items <- (ncol(Ind_prey))-1
  last_day_ind_prey <- tail(Day_ind_prey, n = 1)  # get the total number of days
  
  globalout_Ind_Prey <- NULL
  
  for(i in 1:Ind_prey_items){
    Prey <- Ind_prey[,i+1]
    Prey <- approx(Day_ind_prey,Prey, n = last_day_ind_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Ind_Prey <- cbind(globalout_Ind_Prey,Prey)
    Ind_prey <- read.csv("Main Inputs/Indigestible_Prey.csv",head=TRUE)
  }
  
  colnames(globalout_Ind_Prey) <- names(Ind_prey)[-1]
  
  ########################################################################  
  ### Predator energy density 
  ########################################################################
  
  Predator_E <- read.csv("Main Inputs/Pred_E.csv",head=TRUE) 
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
  
  pred_En_D <- function(W,day,PREDEDEQ) {
    if(PREDEDEQ == 1) {return(Pred_E[day])}     
    if(PREDEDEQ == 2 && W<as.numeric(cutoff)) {return((as.numeric(alpha1) + as.numeric(beta1)*W))}  
    if(PREDEDEQ == 2 && W>=as.numeric(cutoff)) {return((as.numeric(alpha2) + as.numeric(beta2)*W))} 
    if(PREDEDEQ == 2 && W<as.numeric(cutoff) && as.numeric(beta1) == 0) {return((as.numeric(alpha1)))}  
    if(PREDEDEQ == 2 && W>=as.numeric(cutoff) && as.numeric(beta2) == 0) {return((as.numeric(alpha2)))} 
    if(PREDEDEQ == 3) {return(alpha1*W^beta1)}  
  }
   
  ########################################################################
  ### Mortality
  ########################################################################
  
  Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
  Day_mort <- Mortality[,1] # Days
  mort_types <- (ncol(Mortality))-1
  last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
  globalout_mort <- NULL
  
  for(i in 1:mort_types){
    Mort <- Mortality[,i+1]
    Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate mortality
    Mort <- Mort[First_day:Last_day]
    globalout_mort <- cbind(globalout_mort,Mort)
    Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
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
Individuals <- Ind
globalout_individuals <- NULL

for(i in 1:(Last_day-First_day+1)){  
Z <- log(1-as.numeric(globalout_mort[i,4]))  
Individuals <- Individuals*exp(Z*1/365)
globalout_individuals <- rbind(globalout_individuals,data.frame(day=globalout_mort[i,1],
                                                                individuals=Individuals))

}

########################################################################
### Reproduction
########################################################################

Reproduction <- read.csv("Sub-Models/Reproduction/Reproduction.csv",head=TRUE)
Day <- Reproduction[,1] # Days
Reproduction <- Reproduction[,2]  # Just use the Temp values, which are in column 2
last_day <- tail(Day, n = 1)  # get the total number of days
Dayz <- approx(Day,Reproduction, n = last_day, method="linear")$x
Dayz <- Dayz[First_day:Last_day]
Reproduction <- approx(Day,Reproduction, n = last_day, method="constant")$y # interpolate temperature data
Reproduction <- Reproduction[First_day:Last_day]
  
########################################################################
### Contaminant Accumulation 
########################################################################  



X_Pred <- input$init_pred_conc
zeta <- input$all_cons
Kcl <- input$dep_cons
Base_Temp <- input$base_temp_dep
CONTEQ <- input$cont_acc

Prey_conc <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv",head=TRUE)
Day_conc <- Prey_conc[,1] # Days
Prey_ass <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv",head=TRUE)
Day_Prey_ass <- Prey_ass[,1] # Days
Trans_eff <- read.csv("Sub-Models/Contaminant Accumulation/Transfer Efficiency.csv",head=TRUE)
Day_Trans_eff <- Trans_eff[,1] # Days
prey_items <- (ncol(Prey_conc))-1
last_day_conc <- tail(Day_conc, n = 1)  # get the total number of days
last_day_prey_ass <- tail(Day_Prey_ass, n = 1)  # get the total number of days
last_day_trans_eff <- tail(Day_Trans_eff, n=1)

globalout_Prey_Conc <- NULL
globalout_Prey_ass <- NULL
globalout_Trans_eff <-NULL

for(i in 1:prey_items){
  Prey_Conc <- Prey_conc[,i+1]
  Prey_Conc <- approx(Day_conc,Prey_Conc, n = last_day_conc,method="linear")$y # interpolate prey 1 energy density
  Prey_Conc <- Prey_Conc[First_day:Last_day]
  globalout_Prey_Conc <- cbind(globalout_Prey_Conc,Prey_Conc)
  Prey_conc <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv",head=TRUE)
  Prey_ass <- Prey_ass[,i+1]  
  Prey_ass <- approx(Day_Prey_ass,Prey_ass, n = last_day_prey_ass,method="constant")$y # interpolate prey 1 energy density
  Prey_ass <- Prey_ass[First_day:Last_day]
  globalout_Prey_ass <- cbind(globalout_Prey_ass,Prey_ass)
  Prey_ass <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv",head=TRUE)
  Trans_eff <- Trans_eff[,i+1]  
  Trans_eff <- approx(Day_Trans_eff,Trans_eff, n = last_day_trans_eff,method="constant")$y # interpolate prey 1 energy density
  Trans_eff <- Trans_eff[First_day:Last_day]
  globalout_Trans_eff <- cbind(globalout_Trans_eff,Trans_eff)
  Trans_eff <- read.csv("Sub-Models/Contaminant Accumulation/Transfer Efficiency.csv",head=TRUE)
}

colnames(globalout_Prey_Conc) <- names(Prey_conc)[-1]
colnames(globalout_Prey_ass) <- names(Prey_ass)[-1]
colnames(globalout_Trans_eff) <- names(Trans_eff)[-1]

pred_cont_conc <- function(C,W,X_Prey,X_Pred,TEx,X_ae,CONTEQ) {
  Kcl <- ifelse(CONTEQ == 2,Kcl,Kcl*2^((Temperature-Base_Temp)/10))
  Clearance <- W^zeta*X_Pred*Kcl
  Uptake <- ifelse(CONTEQ == 1,sum(C*X_Prey*TEx),sum(C*X_Prey*X_ae))
  X_Pred <- ifelse(CONTEQ == 1,Uptake,Uptake-Clearance)  
  return(c(Clearance,Uptake,X_Pred))     
}

########################################################################
### Nutrient Regeneration
########################################################################

Phos_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Ae.csv",head=TRUE)
Day_Phos_Ae <- Phos_Ae[,1] # Days
Phos_Conc_Pred <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Pred.csv",head=TRUE)
Day_Phos_Conc_Pred <- Phos_Conc_Pred[,1] # Days
Phos_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv",head=TRUE)
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

Nit_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Ae.csv",head=TRUE)
Day_Nit_Ae <- Nit_Ae[,1] # Days
Nit_Conc_Pred <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Pred.csv",head=TRUE)
Day_Nit_Conc_Pred <- Nit_Conc_Pred[,1] # Days
Nit_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv",head=TRUE)
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
  Phos_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Ae.csv",head=TRUE)
  
  Phos_Conc_Prey <- Phos_Conc_Prey[,i+1]  
  Phos_Conc_Prey <- approx(Day_Phos_Conc_Prey,Phos_Conc_Prey, n = last_day_Phos_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Phos_Conc_Prey <- Phos_Conc_Prey[First_day:Last_day]
  globalout_Phos_Conc_Prey <- cbind(globalout_Phos_Conc_Prey,Phos_Conc_Prey)
  Phos_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv",head=TRUE)
  
  Nit_Ae <- Nit_Ae[,i+1]
  Nit_Ae <- approx(Day_Nit_Ae,Nit_Ae, n = last_day_Nit_Ae,method="linear")$y # interpolate prey 1 energy density
  Nit_Ae <- Nit_Ae[First_day:Last_day]
  globalout_Nit_Ae <- cbind(globalout_Nit_Ae,Nit_Ae)
  Nit_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Ae.csv",head=TRUE)
  
  Nit_Conc_Prey <- Nit_Conc_Prey[,i+1]  
  Nit_Conc_Prey <- approx(Day_Nit_Conc_Prey,Nit_Conc_Prey, n = last_day_Nit_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Nit_Conc_Prey <- Nit_Conc_Prey[First_day:Last_day]
  globalout_Nit_Conc_Prey <- cbind(globalout_Nit_Conc_Prey,Nit_Conc_Prey)
  Nit_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv",head=TRUE)
}

colnames(globalout_Phos_Ae) <- names(Phos_Ae)[-1]
colnames(globalout_Phos_Conc_Pred) <- names(Phos_Conc_Pred)[-1]
colnames(globalout_Phos_Conc_Prey) <- names(Phos_Conc_Prey)[-1]
colnames(globalout_Nit_Ae) <- names(Nit_Ae)[-1]
colnames(globalout_Nit_Conc_Pred) <- names(Nit_Conc_Pred)[-1]
colnames(globalout_Nit_Conc_Prey) <- names(Nit_Conc_Prey)[-1]

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
########################################################################
### Growth Function using p-value
########################################################################

  grow <- function(Temperature, W, p, outpt, globalout_Prey, globalout_Prey_E) { # Growth as a function of temperature, weight, p-value, prey proportion and energy density and predator energy density
    globalout <- NULL # Create a blank dataframe to store outputs
    globaloutprey <- NULL # Create a blank dataframe to store outputs by prey type
    for(i in 1:Fin) { # Create a loop that estimates growth for the duration of the simulation (Fin)      
      Pred_E_i <- pred_En_D(W=W,day=i,PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
      Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
      Cons <- consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption in J/g
        sum(globalout_Prey[i,]*globalout_Prey_E[i,])
      Cons_prey_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in J
      (globalout_Prey[i,]*globalout_Prey_E[i,])*W))
      colnames(Cons_prey_J) <- paste(colnames(Cons_prey_J),"Joules", sep = " ")
      Cons_prey_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in g
      (globalout_Prey[i,]*W)))
      colnames(Cons_prey_G) <- paste(colnames(Cons_prey_G),"Grams", sep = " ")
      Cons_prey_pop_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in J
                                    (globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind))
      colnames(Cons_prey_pop_J) <- paste(colnames(Cons_prey_pop_J),"pop.Joules", sep = " ")
      Cons_prey_pop_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in g
                                    (globalout_Prey[i,]*W*Ind)))
      colnames(Cons_prey_pop_G) <- paste(colnames(Cons_prey_pop_G),"pop.Grams", sep = " ")
      Eg  <- egestion(C=Cons,Temperature=Temperature[i], p=p, EGEQ=EGEQ) # Egestion in J/g
      Ex  <- excretion(C=Cons, Eg=Eg,Temperature=Temperature[i], p=p, EXEQ=EXEQ) # Excretion in J/g
      SpecDA  <- SpDynAct(C=Cons,Eg=Eg) # Specific dynamic action in J/g 
      Res  <- respiration(Temperature=Temperature[i], W=W, REQ)*Oxycal # respiration in J/g 
      
      G <-  Cons - (Res + Eg + Ex + SpecDA) # Energy put towards growth in J/g
      
      egain  <-  (G * W)      # net energy gain in J
      
      if(PREDEDEQ == 3) {
        finalwt <- ((egain+(Pred_E_i*W))/alpha1)^(1/(beta1+1))
      }else if(PREDEDEQ == 2){
        finalwt <- (-alpha1 + sqrt(alpha1^2 + (4*beta1*(W*(alpha1+beta1*W)+egain))))/(2*beta1)
      }else{
        finalwt <- (egain+(Pred_E_i*W))/Pred_E_iplusone
      }
      #(egain+(Pred_E_i*W))/Pred_E_iplusone  # Predator weight at end of current day (g)
      
      weightgain  <-  finalwt-W		#change in g/day
      
       if(input$spawn==TRUE){ # Spwaning function
         spawn <- Reproduction[i]
       }else{
         spawn <- 0
       }
      
      if(input$pop_mort==TRUE){ 
      Ind2 <- Ind*exp(log(1-as.numeric(globalout_mort[i,4]))*1/365) # Population mortality
      }else{
        Ind2 <- Ind
      }
      
      Cons_cont <- consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)*globalout_Prey[i,]*W
      
       if(input$nut==TRUE){
       Phos <- phosphorous_allocation(C=Cons_cont,p_conc_prey=globalout_Phos_Conc_Prey[i,],AEp=globalout_Phos_Ae[i,],weightgain=weightgain,p_conc_pred=globalout_Phos_Conc_Pred[i,])
       Nit <- nitrogen_allocation(C=Cons_cont,n_conc_prey=globalout_Nit_Conc_Prey[i,],AEn=globalout_Nit_Ae[i,],weightgain=weightgain,n_conc_pred=globalout_Nit_Conc_Pred[i,])
       } else{
        Phos <- NA
        Nit <- NA
       }
        
       if(input$contaminant==TRUE){
         Cont <- pred_cont_conc(C=Cons_cont,W=W,X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,TEx=globalout_Trans_eff[i,],X_ae=globalout_Prey_ass[i,],CONTEQ=CONTEQ)  
         X_Pred <- Cont[3]
      } else {
        Cont <- NA
      }
    
      globalout<-rbind(globalout,cbind(data.frame(Day=Dayz[i],
                                            Temperature=Temperature[i],
                                            Starting.Weight=W,
                                            Weight=finalwt-(spawn*finalwt),
                                            Population.Number=Ind,
                                            Population.Biomass=finalwt*Ind,
                                            Specific.Growth.Rate.Joules=G,
                                            Specific.Consumption.Rate.Joules=Cons,
                                            Specific.Egestion.Rate=Eg,
                                            Specific.Excretion.Rate=Ex,
                                            Specific.Respiration.Rate=Res,
                                            Specific.SDA.Rate=SpecDA,
                                            Specific.Consumption.Rate.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                            Specific.Growth.Rate.Grams=G/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                            Initial.Predator.Energy.Density=Pred_E_i,
                                            Final.Predator.Energy.Density=Pred_E_iplusone,
                                            Mean.Prey.Energy.Density=sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                            Gross.Production.Grams=(Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i,
                                            Gross.Production.Joules=(Cons +Res + Eg + Ex + SpecDA)*W,
                                            Cum.Gross.Production.Grams=cumsum((Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i),
                                            Cum.Gross.Production.Joules=cumsum((Cons +Res + Eg + Ex + SpecDA)*W),
                                            Gametic.Production.Grams=spawn*finalwt,
                                            Gametic.Production.Joules=spawn*finalwt*Pred_E_i,
                                            Net.Production.Grams=weightgain,
                                            Net.Production.Joules=egain,
                                            Cum.Net.Production.Grams=cumsum(weightgain),
                                            Cum.Net.Production.Joules=cumsum(egain),
                                            Prey.Tot.Ind.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W, 
                                            Prey.Tot.Ind.Joules=Cons*W,
                                            Cum.Prey.Tot.Ind.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W), 
                                            Cum.Prey.Tot.Ind.Joules=cumsum(Cons*W),
                                            Prey.Tot.Pop.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind,
                                            Prey.Tot.Pop.Joules=Cons*W*Ind,
                                            Cum.Prey.Tot.Pop.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind),
                                            Cum.Prey.Tot.Pop.Joules=cumsum(Cons*W*Ind),
                                            Mortality.number=Ind-Ind2,
                                            Mortality.Grams=(Ind-Ind2)*W,
                                            Nitrogen.Egestion=Nit[4],
                                            Phosphorous.Egestion=Phos[4],
                                            N.to.P.Egestion=Nit[4]/Phos[4],
                                            Nitrogen.Excretion=Nit[3],
                                            Phosphorous.Excretion=Phos[3],
                                            N.to.P.Excretion=Nit[3]/Phos[3],
                                            Nitrogen.Consumption=Nit[1],
                                            Phosphorous.Consumption=Phos[1],
                                            N.to.P.Consumption=Nit[1]/Phos[1],
                                            Nitrogen.Growth=Nit[2],
                                            Phosphorous.Growth=Phos[2],
                                            N.to.P.Growth=Nit[2]/Phos[2],
                                            Contaminant.Uptake=Cont[1],
                                            Contaminant.Elimination=Cont[2],
                                            Contaminant.Predator.Concentration=Cont[3]/W),
                                            Cons_prey_J,Cons_prey_G,Cons_prey_pop_J,Cons_prey_pop_G))
      
      #globalout<-cbind(globalout,Cons_prey)
                                            
      W <- finalwt-(spawn*finalwt) # Weight at the end of the day serves as the starting weight for the next day
      Ind <- Ind2                
    }
    globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")] <- 
      cumsum( globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")])
    if(outpt == "vector") {return(globalout)} 
    #if(outpt == "vector") {return(globaloutprey)} 
    if(outpt == "final" && fit.to=="Weight")  {return(globalout[Fin,4])} 
    if(outpt == "final" && fit.to=="Consumption")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Starting.Weight']))}
    if(outpt == "final" && fit.to=="p-value")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Starting.Weight']))}
                            
  }
   

########################################################################
### Growth Function using ration
########################################################################

grow_ration <- function(Temperature, W, outpt, globalout_Prey, globalout_Prey_E) { # Growth as a function of temperature, weight, p-value, prey proportion and energy density and predator energy density
  globalout <- NULL # Create a blank dataframe to store outputs
  for(i in 1:Fin) { # Create a loop that estimates growth for the duration of the simulation (Fin)
    Pred_E_i <- pred_En_D(W=W,day=i,PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Cons <- Ration*sum(globalout_Prey[i,]*globalout_Prey_E[i,])
    Cons_p <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)* # Consumption in J/g
    sum(globalout_Prey[i,]*globalout_Prey_E[i,])
    p <- Cons/Cons_p
    Cons_prey_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in J
                                  (globalout_Prey[i,]*globalout_Prey_E[i,])*W))
    colnames(Cons_prey_J) <- paste(colnames(Cons_prey_J),"Joules", sep = " ")
    Cons_prey_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in g
                                  (globalout_Prey[i,]*W)))
    colnames(Cons_prey_G) <- paste(colnames(Cons_prey_G),"Grams", sep = " ")
    Cons_prey_pop_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in J
                                      (globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind))
    colnames(Cons_prey_pop_J) <- paste(colnames(Cons_prey_pop_J),"pop.Joules", sep = " ")
    Cons_prey_pop_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in g
                                      (globalout_Prey[i,]*W*Ind)))
    colnames(Cons_prey_pop_G) <- paste(colnames(Cons_prey_pop_G),"pop.Grams", sep = " ")
    Eg  <- egestion(C=Cons,Temperature=Temperature[i], p=p, EGEQ=EGEQ) # Egestion in J/g
    Ex  <- excretion(C=Cons, Eg=Eg,Temperature=Temperature[i], p=p, EXEQ=EXEQ) # Excretion in J/g
    SpecDA  <- SpDynAct(C=Cons,Eg=Eg) # Specific dynamic action in J/g 
    Res  <- respiration(Temperature=Temperature[i], W=W, REQ)*13388.8 # respiration in J/g 
    
    G <-  Cons - (Res + Eg + Ex + SpecDA) # Energy put towards growth in J/g
    
    egain  <-  (G * W)      # net energy gain in J
    
    delta_B <- Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])
    
    if(PREDEDEQ == 3) {
      finalwt <- ((egain+(Pred_E_i*W))/alpha1)^(1/(beta1+1))
    }else if(PREDEDEQ == 2){
      finalwt <- (-alpha1 + sqrt(alpha1^2 + (4*beta1*(W*(alpha1+beta1*W)+egain))))/(2*beta1)
    }else{
      finalwt <- (egain+(Pred_E_i*W))/Pred_E_iplusone
    }
    #finalwt  <-  (egain+(Pred_E_i*W))/Pred_E_iplusone  # Predator weight at end of current day (g)
    
    #dGrams = (G/Pred_E)  		# Weight (g) gained = Net energy gain (J) / Predator energy density (J) 
    
    weightgain  <-  finalwt-W		#change in g/day
    
    #finale <- (W*Pred_E) + egain # Predator energy density
    
    if(input$spawn==TRUE){ # Spwaning function
      spawn <- Reproduction[i]
    }else{
      spawn <- 0
    }
    
    if(input$pop_mort==TRUE){ 
      Ind2 <- Ind*exp(log(1-as.numeric(globalout_mort[i,4]))*1/365) # Population mortality
    }else{
      Ind2 <- Ind
    }
    
    Cons_cont <- Ration*globalout_Prey[i,]
    
    if(input$nut==TRUE){
      Phos <- phosphorous_allocation(C=Cons_cont,p_conc_prey=globalout_Phos_Conc_Prey[i,],AEp=globalout_Phos_Ae[i,],weightgain=weightgain,p_conc_pred=globalout_Phos_Conc_Pred[i,])
      Nit <- nitrogen_allocation(C=Cons_cont,n_conc_prey=globalout_Nit_Conc_Prey[i,],AEn=globalout_Nit_Ae[i,],weightgain=weightgain,n_conc_pred=globalout_Phos_Conc_Pred[i,])
    } else{
      Phos <- NA
      Nit <- NA
    }
    
    if(input$contaminant==TRUE){
      Cont <- pred_cont_conc(C=Cons_cont,W=W,X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,X_ae=globalout_Prey_ass[i,],CONTEQ=CONTEQ)  
      X_Pred <- Cont[3]
    } else {
      Cont <- NA
    }
    
    
    globalout<-rbind(globalout,cbind(data.frame(Day=Dayz[i],
                                          Temperature=Temperature[i],
                                          Starting.Weight=W,
                                          Weight=finalwt-(spawn*finalwt),
                                          Population.Number=Ind,
                                          Population.Biomass=finalwt*Ind,
                                          Specific.Growth.Rate.Joules=G,
                                          Specific.Consumption.Rate.Joules=Cons,
                                          Specific.Egestion.Rate=Eg,
                                          Specific.Excretion.Rate=Ex,
                                          Specific.Respiration.Rate=Res,
                                          Specific.SDA.Rate=SpecDA,
                                          Specific.Consumption.Rate.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Specific.Growth.Rate.Grams=G/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Initial.Predator.Energy.Density=Pred_E_i,
                                          Final.Predator.Energy.Density=Pred_E_iplusone,
                                          Mean.Prey.Energy.Density=sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Gross.Production.Grams=(Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i,
                                          Gross.Production.Joules=(Cons +Res + Eg + Ex + SpecDA)*W,
                                          Cum.Gross.Production.Grams=cumsum((Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i),
                                          Cum.Gross.Production.Joules=cumsum((Cons +Res + Eg + Ex + SpecDA)*W),
                                          Gametic.Production.Grams=spawn*finalwt,
                                          Gametic.Production.Joules=spawn*finalwt*Pred_E_i,
                                          Net.Production.Grams=weightgain,
                                          Net.Production.Joules=egain,
                                          Cum.Net.Production.Grams=cumsum(weightgain),
                                          Cum.Net.Production.Joules=cumsum(egain),
                                          Prey.Tot.Ind.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W, 
                                          Prey.Tot.Ind.Joules=Cons*W,
                                          Cum.Prey.Tot.Ind.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W), 
                                          Cum.Prey.Tot.Ind.Joules=cumsum(Cons*W),
                                          Prey.Tot.Pop.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind,
                                          Prey.Tot.Pop.Joules=Cons*W*Ind,
                                          Cum.Prey.Tot.Pop.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind),
                                          Cum.Prey.Tot.Pop.Joules=cumsum(Cons*W*Ind),
                                          Mortality.number=Ind-Ind2,
                                          Mortality.Grams=(Ind-Ind2)*W,
                                          Nitrogen.Egestion=Nit[4],
                                          Phosphorous.Egestion=Phos[4],
                                          N.to.P.Egestion=Nit[4]/Phos[4],
                                          Nitrogen.Excretion=Nit[3],
                                          Phosphorous.Excretion=Phos[3],
                                          N.to.P.Excretion=Nit[3]/Phos[3],
                                          Nitrogen.Consumption=Nit[1],
                                          Phosphorous.Consumption=Phos[1],
                                          N.to.P.Consumption=Nit[1]/Phos[1],
                                          Nitrogen.Growth=Nit[2],
                                          Phosphorous.Growth=Phos[2],
                                          N.to.P.Growth=Nit[2]/Phos[2],
                                          Contaminant.Uptake=Cont[1],
                                          Contaminant.Elimination=Cont[2],
                                          Contaminant.Predator.Concentration=Cont[3]/W),
                                          Cons_prey_J,Cons_prey_G,Cons_prey_pop_J,Cons_prey_pop_G))
    
    W <- finalwt-(spawn*finalwt) # Weight at the end of the day serves as the starting weight for the next day
    Ind <- Ind2
  } 
  globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")] <- 
    cumsum( globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")])
  if(outpt == "vector") {return(globalout)} 
  if(outpt == "final" && fit.to=="Weight")  {return(globalout[Fin,4])} 
  if(outpt == "final" && fit.to=="Consumption")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Initial.Weight']))}
  if(outpt == "final" && fit.to=="p-value")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Initial.Weight']))}
}

########################################################################
### Growth Function using fixed ration
########################################################################

grow_ration_prey <- function(Temperature, W, outpt, globalout_Prey, globalout_Prey_E) { # Growth as a function of temperature, weight, p-value, prey proportion and energy density and predator energy density
  globalout <- NULL # Create a blank dataframe to store outputs
  for(i in 1:Fin) { # Create a loop that estimates growth for the duration of the simulation (Fin)
    Pred_E_i <- pred_En_D(W=W,day=i,PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Cons <- Ration_prey*sum(globalout_Prey[i,]*globalout_Prey_E[i,])/W
    Cons_p <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)* # Consumption in J/g
      sum(globalout_Prey[i,]*globalout_Prey_E[i,])
    p <- Cons/Cons_p
    Cons_prey_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in J
                                  (globalout_Prey[i,]*globalout_Prey_E[i,])*W))
    colnames(Cons_prey_J) <- paste(colnames(Cons_prey_J),"Joules", sep = " ")
    Cons_prey_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in g
                                  (globalout_Prey[i,]*W)))
    colnames(Cons_prey_G) <- paste(colnames(Cons_prey_G),"Grams", sep = " ")
    Cons_prey_pop_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in J
                                      (globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind))
    colnames(Cons_prey_pop_J) <- paste(colnames(Cons_prey_pop_J),"pop.Joules", sep = " ")
    Cons_prey_pop_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in g
                                      (globalout_Prey[i,]*W*Ind)))
    colnames(Cons_prey_pop_G) <- paste(colnames(Cons_prey_pop_G),"pop.Grams", sep = " ")
    Eg  <- egestion(C=Cons,Temperature=Temperature[i], p=p, EGEQ=EGEQ) # Egestion in J/g
    Ex  <- excretion(C=Cons, Eg=Eg,Temperature=Temperature[i], p=p, EXEQ=EXEQ) # Excretion in J/g
    SpecDA  <- SpDynAct(C=Cons,Eg=Eg) # Specific dynamic action in J/g 
    Res  <- respiration(Temperature=Temperature[i], W=W, REQ)*13388.8 # respiration in J/g 
    
    G <-  Cons - (Res + Eg + Ex + SpecDA) # Energy put towards growth in J/g
    
    egain  <-  (G * W)      # net energy gain in J
    
    delta_B <- Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])
    
    if(PREDEDEQ == 3) {
      finalwt <- ((egain+(Pred_E_i*W))/alpha1)^(1/(beta1+1))
    }else if(PREDEDEQ == 2){
      finalwt <- (-alpha1 + sqrt(alpha1^2 + (4*beta1*(W*(alpha1+beta1*W)+egain))))/(2*beta1)
    }else{
      finalwt <- (egain+(Pred_E_i*W))/Pred_E_iplusone
    }
    #finalwt  <-  (egain+(Pred_E_i*W))/Pred_E_iplusone  # Predator weight at end of current day (g)
    
    #dGrams = (G/Pred_E)    	# Weight (g) gained = Net energy gain (J) / Predator energy density (J) 
    
    weightgain  <-  finalwt-W		#change in g/day
    
    #finale <- (W*Pred_E) + egain # Predator energy density
    
    if(input$spawn==TRUE){ # Spwaning function
      spawn <- Reproduction[i]
    }else{
      spawn <- 0
    }
    
    if(input$pop_mort==TRUE){ 
      Ind2 <- Ind*exp(log(1-as.numeric(globalout_mort[i,4]))*1/365) # Population mortality
    }else{
      Ind2 <- Ind
    }
    
    Cons_cont <- Ration*globalout_Prey[i,]
    
    if(input$nut==TRUE){
      Phos <- phosphorous_allocation(C=Cons_cont,p_conc_prey=globalout_Phos_Conc_Prey[i,],AEp=globalout_Phos_Ae[i,],weightgain=weightgain,p_conc_pred=globalout_Phos_Conc_Pred[i,])
      Nit <- nitrogen_allocation(C=Cons_cont,n_conc_prey=globalout_Nit_Conc_Prey[i,],AEn=globalout_Nit_Ae[i,],weightgain=weightgain,n_conc_pred=globalout_Phos_Conc_Pred[i,])
    } else{
      Phos <- NA
      Nit <- NA
    }
    
    if(input$contaminant==TRUE){
      Cont <- pred_cont_conc(C=Cons_cont,W=W,X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,X_ae=globalout_Prey_ass[i,],CONTEQ=CONTEQ)  
      X_Pred <- Cont[3]
    } else {
      Cont <- NA
    }
    
    
    globalout<-rbind(globalout,cbind(data.frame(Day=Dayz[i],
                                          Temperature=Temperature[i],
                                          Starting.Weight=W,
                                          Weight=finalwt-(spawn*finalwt),
                                          Population.Number=Ind,
                                          Population.Biomass=finalwt*Ind,
                                          Specific.Growth.Rate.Joules=G,
                                          Specific.Consumption.Rate.Joules=Cons,
                                          Specific.Egestion.Rate=Eg,
                                          Specific.Excretion.Rate=Ex,
                                          Specific.Respiration.Rate=Res,
                                          Specific.SDA.Rate=SpecDA,
                                          Specific.Consumption.Rate.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Specific.Growth.Rate.Grams=G/sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Initial.Predator.Energy.Density=Pred_E_i,
                                          Final.Predator.Energy.Density=Pred_E_iplusone,
                                          Mean.Prey.Energy.Density=sum(globalout_Prey[i,]*globalout_Prey_E[i,]),
                                          Gross.Production.Grams=(Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i,
                                          Gross.Production.Joules=(Cons +Res + Eg + Ex + SpecDA)*W,
                                          Cum.Gross.Production.Grams=cumsum((Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i),
                                          Cum.Gross.Production.Joules=cumsum((Cons +Res + Eg + Ex + SpecDA)*W),
                                          Gametic.Production.Grams=spawn*finalwt,
                                          Gametic.Production.Joules=spawn*finalwt*Pred_E_i,
                                          Net.Production.Grams=weightgain,
                                          Net.Production.Joules=egain,
                                          Cum.Net.Production.Grams=cumsum(weightgain),
                                          Cum.Net.Production.Joules=cumsum(egain),
                                          Prey.Tot.Ind.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W, 
                                          Prey.Tot.Ind.Joules=Cons*W,
                                          Cum.Prey.Tot.Ind.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W), 
                                          Cum.Prey.Tot.Ind.Joules=cumsum(Cons*W),
                                          Prey.Tot.Pop.Grams=Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind,
                                          Prey.Tot.Pop.Joules=Cons*W*Ind,
                                          Cum.Prey.Tot.Pop.Grams=cumsum(Cons/sum(globalout_Prey[i,]*globalout_Prey_E[i,])*W*Ind),
                                          Cum.Prey.Tot.Pop.Joules=cumsum(Cons*W*Ind),
                                          Mortality.number=Ind-Ind2,
                                          Mortality.Grams=(Ind-Ind2)*W,
                                          Nitrogen.Egestion=Nit[4],
                                          Phosphorous.Egestion=Phos[4],
                                          N.to.P.Egestion=Nit[4]/Phos[4],
                                          Nitrogen.Excretion=Nit[3],
                                          Phosphorous.Excretion=Phos[3],
                                          N.to.P.Excretion=Nit[3]/Phos[3],
                                          Nitrogen.Consumption=Nit[1],
                                          Phosphorous.Consumption=Phos[1],
                                          N.to.P.Consumption=Nit[1]/Phos[1],
                                          Nitrogen.Growth=Nit[2],
                                          Phosphorous.Growth=Phos[2],
                                          N.to.P.Growth=Nit[2]/Phos[2],
                                          Contaminant.Uptake=Cont[1],
                                          Contaminant.Elimination=Cont[2],
                                          Contaminant.Predator.Concentration=Cont[3]/W),
                                          Cons_prey_J,Cons_prey_G,Cons_prey_pop_J,Cons_prey_pop_G))
    
    W <- finalwt-(spawn*finalwt) # Weight at the end of the day serves as the starting weight for the next day
    Ind <- Ind2
  } 
  globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")] <- 
    cumsum( globalout[,c("Cum.Gross.Production.Grams","Cum.Gross.Production.Joules","Cum.Net.Production.Grams","Cum.Net.Production.Joules","Cum.Prey.Tot.Ind.Grams","Cum.Prey.Tot.Ind.Joules","Cum.Prey.Tot.Pop.Grams","Cum.Prey.Tot.Pop.Joules")])
  if(outpt == "vector") {return(globalout)} 
  if(outpt == "final" && fit.to=="Weight")  {return(globalout[Fin,4])} 
  if(outpt == "final" && fit.to=="Consumption")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Initial.Weight']))}
  if(outpt == "final" && fit.to=="p-value")  {return(sum(globalout[,'Specific.Consumption.Rate.Grams']*globalout[,'Initial.Weight']))}
}

########################################################################
### binary search algorithm for p-value 
########################################################################

  p <- 0.3 # Need some value to start the fitting process for p-value
  W.tol  <- 0.001  # Predicted W must be within W.tol of the specified W for an adequate p-fit.
  max.iter <- 15 # Max number of iterations of binary search
  
  fit.p <- function(p, IW, FW, W.tol, max.iter) {
    W      <- IW
    n.iter <- 0  # Counter for number of iterations
    p.max  <- 5.00  # current max
    p.min  <- 0.001  # current min
    outpt <- "final" # desire only the final weight value, not the full vector of weights
    # initialize W.p
    W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
    
    while((n.iter <= max.iter) & (abs(W.p-FW) > W.tol)) {
      n.iter <- n.iter + 1
      W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
      if(W.p > FW) {p.max <- p} else {p.min <- p}
      p <- p.min + (p.max - p.min)/2
      withProgress(message = 'Calculating ...',value = NULL,{
      #Sys.sleep(0)
      }
      )
    }
    return(p)
  }
   
########################################################################
### Model run 
########################################################################

if(fit.to=="Ration"){ 
  W1.p <- grow_ration(Temperature=Temperature,W=Init_W,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p)

}else if(fit.to=="Ration_prey"){ 
  W1.p <- grow_ration_prey(Temperature=Temperature,W=Init_W,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p
}else if(fit.to=="p-value"){ 
  p <- p_value
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p)
}else{ 
  FW <-ifelse(fit.to=="Weight",Final_W,eaten)
  p <- 0.5  
  p <- fit.p(p, Init_W, FW, W.tol, max.iter)
  W1.p <- grow(Temperature=Temperature,W=Init_W,p=p,outpt="vector",globalout_Prey=globalout_Prey, globalout_Prey_E=globalout_Prey_E)
  W1.p #<- cbind(W1.p,W2.p) 
  rownames(W1.p) <- NULL
}

########################################################################
### Outputs
########################################################################

  output$summary <- renderTable({ 
    Model()
    parms <- read.csv("Parameters_official.csv") #  Read parameter values from .csv file
    p <- ifelse(input$fitto=="Ration",NA,format(round(p,3), nsmall = 3))
    Growth <- format(round(W1.p[(input$FD-input$ID+1),4],3), nsmall = 3)
    Consom <- format(round(sum(W1.p[,"Specific.Consumption.Rate.Grams"]*W1.p[,"Starting.Weight"]),3), nsmall = 3)
      
    
    Parameter <- c("p-value","Total consumption (g)","Final weight (g)")
    Value <- c(p,Consom,Growth)
    as.data.frame(cbind(Parameter,Value))
  
  },include.rownames=FALSE)
  
   W1.p 

})

  output$table <- renderTable({
    W1.p <- Model() 
    #cumsum(W1.p[,c("Cum.Gross.Production.Grams")])
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
  plot( W1.p[,input$xaxis],W1.p[,input$yaxis],xlim=c(min(W1.p[,input$xaxis]),max(W1.p[,input$xaxis]))
       ,ylim=c(min(W1.p[,input$yaxis]),max(W1.p[,input$yaxis])),xlab=input$xaxis,ylab=input$yaxis,type="l",col="red")
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
  Temperature <- read.csv("Main Inputs/Temperature.csv") #  Read daily Temp values from .csv file
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
  Diet_prop <- read.csv("Main Inputs/Diet_prop.csv",head=TRUE)
  Day_prey <- Diet_prop[,1] # Days
  prey_items <- (ncol(Diet_prop))-1
  last_day_prey <- tail(Day_prey, n = 1)  # get the total number of days
  globalout_Prey <- NULL
  for(i in 1:prey_items){
    Prey <- Diet_prop[,i+1]
    Prey <- approx(Day_prey,Prey, n = last_day_prey,method="linear")$y # interpolate prey 1 energy density
    Prey <- Prey[First_day:Last_day]
    globalout_Prey <- cbind(globalout_Prey,Prey)
    Diet_prop <- read.csv("Main Inputs/Diet_prop.csv",head=TRUE)
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
  Prey_E <- read.csv("Main Inputs/Prey_E.csv",head=TRUE)
  Day_Prey_E <- Prey_E[,1] # Days
  prey_items <- (ncol(Prey_E))-1
  last_day_prey_E <- tail(Day_Prey_E, n = 1)  # get the total number of days
  globalout_Prey_E <- NULL
  for(i in 1:prey_items){
    Prey_E <- Prey_E[,i+1]  
    Prey_E <- approx(Day_Prey_E,Prey_E, n = last_day_prey_E,method="constant")$y # interpolate prey 1 energy density
    Prey_E <- Prey_E[First_day:Last_day]
    globalout_Prey_E <- cbind(globalout_Prey_E,Prey_E)
    Prey_E <- read.csv("Main Inputs/Prey_E.csv",head=TRUE)
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
Ind_prey <- read.csv("Main Inputs/Indigestible_Prey.csv",head=TRUE)
Day_ind_prey <- Ind_prey[,1] # Days
Ind_prey_items <- (ncol(Ind_prey))-1
last_day_ind_prey <- tail(Day_ind_prey, n = 1)  # get the total number of days
globalout_Ind_Prey <- NULL
for(i in 1:Ind_prey_items){
  Prey <- Ind_prey[,i+1]
  Prey <- approx(Day_ind_prey,Prey, n = last_day_ind_prey,method="linear")$y # interpolate prey 1 energy density
  Prey <- Prey[First_day:Last_day]
  globalout_Ind_Prey <- cbind(globalout_Ind_Prey,Prey)
  Ind_prey <- read.csv("Main Inputs/Indigestible_Prey.csv",head=TRUE)
}
colnames(globalout_Ind_Prey) <- names(Ind_prey)[-1]
  plot(First_day:Last_day,globalout_Ind_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Ind_prey[,2:(ncol(Ind_prey))]),max(Ind_prey[,2:(ncol(Ind_prey))])),xlab="Day",ylab="Indigestible Prey (%)")
  if(Ind_prey_items>=2){
    for(i in 2:Ind_prey_items){
      lines(First_day:Last_day,globalout_Ind_Prey[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(Ind_prey_items+1),lty=1,legend=colnames(globalout_Ind_Prey), bty = "n")
})

output$pred_ED <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation  
Predator_E <- read.csv("Main Inputs/Pred_E.csv",head=TRUE) 
Day_pred <- Predator_E[,1] # Days
Pred_E <- Predator_E[,2]  # Just use the predator energy values, which are in column 2
last_day_pred <- tail(Day_pred, n = 1)  # get the total number of days + 1
Dayz_pred <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$x
Dayz_pred <- Dayz_pred[First_day:Last_day]
Pred_E <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$y # interpolate temperature data
plot(Dayz_pred,Pred_E[First_day:Last_day],type="l",xlab="Day",ylab="Energy Density (J/g)")
})

output$mort <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation    
Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
Day_mort <- Mortality[,1] # Days
mort_types <- (ncol(Mortality))-1
last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
globalout_mort <- NULL
for(i in 1:mort_types){
  Mort <- Mortality[,i+1]
  Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate prey 1 energy density
  Mort <- Mort[First_day:Last_day]
  globalout_mort <- cbind(globalout_mort,Mort)
  #     colnames(globalout_mort[,i]) 
  #     colnames(globalout_mort[,i]) <- names(Mortality[i+1])
  Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
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
  plot(globalout_mort[1:nrow(globalout_mort),2],type="l",xlab="Day",ylab="Mortality (probability)",col=2,
       xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(globalout_mort[,2:(ncol(globalout_mort))]),max(globalout_mort[,2:(ncol(globalout_mort))])))
  lines(globalout_mort[1:nrow(globalout_mort),3],col=3)
  lines(globalout_mort[1:nrow(globalout_mort),4],col=4)
  legend("topleft",col=2:4,lty=1,legend=colnames(globalout_mort)[-1], bty = "n")
})

output$pop <- renderPlot({
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation      
Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
Day_mort <- Mortality[,1] # Days
mort_types <- (ncol(Mortality))-1
last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
globalout_mort <- NULL
for(i in 1:mort_types){
  Mort <- Mortality[,i+1]
  Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate prey 1 energy density
  Mort <- Mort[First_day:Last_day]
  globalout_mort <- cbind(globalout_mort,Mort)
  Mortality <- read.csv("Sub-Models/Mortality/Mortality.csv",head=TRUE)
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
Individuals <- input$ind 
globalout_individuals <- NULL
for(i in 1:(Last_day-First_day+1)){ 
Z <- log(1-as.numeric(globalout_mort[i,4]))  
Individuals <- Individuals*exp(Z*1/365)
globalout_individuals <- rbind(globalout_individuals,data.frame(day=i,
                                                                individuals=Individuals))
}
plot(globalout_individuals[,1],globalout_individuals[,2],type="l",xlab="Day",ylab="Population number",
     xlim=c(min(First_day),max(Last_day)),
     ylim=c(min(globalout_individuals[,2]),max(globalout_individuals[,2])))
})

output$repro <- renderPlot({
  Reproduction <- read.csv("Sub-Models/Reproduction/Reproduction.csv",head=TRUE)
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
Prey_ass <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv",head=TRUE)
Day_Prey_ass <- Prey_ass[,1] # Days
prey_items <- (ncol(Prey_ass))-1
last_day_prey_ass <- tail(Day_Prey_ass, n = 1)  # get the total number of days
globalout_Prey_ass <- NULL

for(i in 1:prey_items){
  Prey_ass <- Prey_ass[,i+1]  
  Prey_ass <- approx(Day_Prey_ass,Prey_ass, n = last_day_prey_ass,method="constant")$y # interpolate prey 1 energy density
  Prey_ass <- Prey_ass[First_day:Last_day]
  globalout_Prey_ass <- cbind(globalout_Prey_ass,Prey_ass)
  Prey_ass <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Assimilation.csv",head=TRUE)
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
Prey_conc <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv",head=TRUE)
Day_conc <- Prey_conc[,1] # Days
prey_items <- (ncol(Prey_conc))-1
last_day_conc <- tail(Day_conc, n = 1)  # get the total number of days

globalout_Prey_Conc <- NULL

for(i in 1:prey_items){
  Prey_Conc <- Prey_conc[,i+1]
  Prey_Conc <- approx(Day_conc,Prey_Conc, n = last_day_conc,method="linear")$y # interpolate prey 1 energy density
  Prey_Conc <- Prey_Conc[First_day:Last_day]
  globalout_Prey_Conc <- cbind(globalout_Prey_Conc,Prey_Conc)
  Prey_conc <- read.csv("Sub-Models/Contaminant Accumulation/Contaminant Concentration.csv",head=TRUE)
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

output$phos_ae <- renderPlot({ 
First_day   <- input$ID             ### First day of the simulation
Last_day    <- input$FD             ### Last day of the simulation     
Phos_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Ae.csv",head=TRUE)
Day_Phos_Ae <- Phos_Ae[,1] # Days
prey_items_nut <- (ncol(Phos_Ae))-1
last_day_Phos_Ae <- tail(Day_Phos_Ae, n = 1)  # get the total number of days
globalout_Phos_Ae <- NULL

for(i in 1:prey_items_nut){
  Phos_Ae <- Phos_Ae[,i+1]
  Phos_Ae <- approx(Day_Phos_Ae,Phos_Ae, n = last_day_Phos_Ae,method="linear")$y # interpolate prey 1 energy density
  Phos_Ae <- Phos_Ae[First_day:Last_day]
  globalout_Phos_Ae <- cbind(globalout_Phos_Ae,Phos_Ae)
  Phos_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Ae.csv",head=TRUE)
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
Phos_Conc_Pred <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Pred.csv",head=TRUE)
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
Phos_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv",head=TRUE)
Day_Phos_Conc_Prey <- Phos_Conc_Prey[,1] # Days
prey_items_nut <- (ncol(Phos_Conc_Prey))-1
last_day_Phos_Conc_Prey <- tail(Day_Phos_Conc_Prey, n = 1)  # get the total number of days
globalout_Phos_Conc_Prey <- NULL

for(i in 1:prey_items_nut){
  Phos_Conc_Prey <- Phos_Conc_Prey[,i+1]  
  Phos_Conc_Prey <- approx(Day_Phos_Conc_Prey,Phos_Conc_Prey, n = last_day_Phos_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Phos_Conc_Prey <- Phos_Conc_Prey[First_day:Last_day]
  globalout_Phos_Conc_Prey <- cbind(globalout_Phos_Conc_Prey,Phos_Conc_Prey)
  Phos_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Phos_Conc_Prey.csv",head=TRUE)
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
Nit_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Ae.csv",head=TRUE)
Day_Nit_Ae <- Nit_Ae[,1] # Days
prey_items_nut <- (ncol(Nit_Ae))-1
last_day_Nit_Ae <- tail(Day_Nit_Ae, n = 1)  # get the total number of days
globalout_Nit_Ae <- NULL

for(i in 1:prey_items_nut){
  Nit_Ae <- Nit_Ae[,i+1]
  Nit_Ae <- approx(Day_Nit_Ae,Nit_Ae, n = last_day_Nit_Ae,method="linear")$y # interpolate prey 1 energy density
  Nit_Ae <- Nit_Ae[First_day:Last_day]
  globalout_Nit_Ae <- cbind(globalout_Nit_Ae,Nit_Ae)
  Nit_Ae <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Ae.csv",head=TRUE)
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
Nit_Conc_Pred <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Pred.csv",head=TRUE)
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
Nit_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv",head=TRUE)
Day_Nit_Conc_Prey <- Nit_Conc_Prey[,1] # Days
prey_items_nut <- (ncol(Nit_Conc_Prey))-1
last_day_Nit_Conc_Prey <- tail(Day_Nit_Conc_Prey, n = 1)  # get the total number of days
globalout_Nit_Conc_Prey <- NULL

for(i in 1:prey_items_nut){
  Nit_Conc_Prey <- Nit_Conc_Prey[,i+1]  
  Nit_Conc_Prey <- approx(Day_Nit_Conc_Prey,Nit_Conc_Prey, n = last_day_Nit_Conc_Prey,method="constant")$y # interpolate prey 1 energy density
  Nit_Conc_Prey <- Nit_Conc_Prey[First_day:Last_day]
  globalout_Nit_Conc_Prey <- cbind(globalout_Nit_Conc_Prey,Nit_Conc_Prey)
  Nit_Conc_Prey <- read.csv("Sub-Models/Nutrient Regeneration/Nit_Conc_Prey.csv",head=TRUE)
}

colnames(globalout_Nit_Conc_Prey) <- names(Nit_Conc_Prey)[-1]

  plot(First_day:Last_day,globalout_Nit_Conc_Prey[,1],type="l",col=2,xlim=c(min(First_day),max(Last_day)),
       ylim=c(min(Nit_Conc_Prey),max(Nit_Conc_Prey)),xlab="Day",ylab="Nitrogen Concentration (proportion of wet mass)")
  if(prey_items_nut>=2){
    for(i in 2:prey_items_nut){
      lines(First_day:Last_day,globalout_Nit_Conc_Prey[,i],type="l",col=i+1)  
    }
  }
  legend("topleft",col=2:(prey_items_nut+1),lty=1,legend=colnames(globalout_Nit_Conc_Prey), bty = "n")
})

#withProgress(message = "Computing Data", value = 1, {
   #Sys.sleep(0.25)
})

#})


