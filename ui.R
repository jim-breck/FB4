require(shiny)
parms <- read.csv("Parameters_official.csv") #  Read parameter values from .csv file
Diet_prop <- read.csv("Main inputs/Diet_prop.csv",head=TRUE)

choicesSpecies <- setNames(1:nrow(parms),parms$Species)
choicePreyJoules <- names(Diet_prop[,-1])
choicePreyJoules <- paste(choicePreyJoules,"Joules", sep = " ")
choicePreyGrams <- names(Diet_prop[,-1])
choicePreyGrams <- paste(choicePreyGrams,"Grams", sep = " ")

choicePreyPopJoules <- names(Diet_prop[,-1])
choicePreyPopJoules <- paste(choicePreyPopJoules,"pop.Joules", sep = " ")
choicePreyPopGrams <- names(Diet_prop[,-1])
choicePreyPopGrams <- paste(choicePreyPopGrams,"pop.Grams", sep = " ")

shinyUI(navbarPage("Fish Bioenergetics 4.0",           
  tabPanel("Initial Settings", 
           sidebarLayout(
             sidebarPanel(
      selectInput("spec", 
                  label = "Species",
                  choices = c("",choicesSpecies),
                  selected = NULL),
      
      numericInput("ID",
                   label = "Initial Day",
                   value = NA),
      
      numericInput("FD",
                   label = "Final Day",
                   value = NA),
      
      numericInput("InW", 
                   label = "Initial Weight (g)", 
                   value = NA),
      
      numericInput("oxycal", 
                   label = "Oxycalorific Coefficient (J/g O2)", 
                   value = 13560),
           
      radioButtons("fitto", 
                   label = h4("Fit to:"), 
                   choices = list("Final Weight (g of wet predator body weight)" = "Weight",
                                  "Consumption (g of total prey wet weight)" = "Consumption",
                                  "Ration (g of daily wet prey weight)" = "Ration_prey",
                                  "Ration (% of daily wet predator weight)" = "Ration",
                                  "p-value (proportion of Cmax)"="p-value"),                                 
                   selected = NA),
 
      numericInput("FinW", 
                   label = NA, 
                   value = NA)
      
),
mainPanel(tableOutput("parameters"))  

)),
  
  tabPanel("Input Files",
  tabsetPanel(
  tabPanel("Temperature",plotOutput("temp")),  
  tabPanel("Diet Proportions", plotOutput("diet_prop")),
  tabPanel("Prey Energy Density",plotOutput("prey_ED")),
  tabPanel("Predator Energy Density",plotOutput("pred_ED")),
  tabPanel("Indigestible Prey",plotOutput("indigest_prey"))
  )
),    
  navbarMenu("Sub-Models",
  tabPanel("Population",
           sidebarLayout(
             sidebarPanel(
           checkboxInput("pop_mort", 
                         label = h4("Mortality"), 
                         value = FALSE),
           
           numericInput("ind",
                        label = "Initial Population Size",
                        value = 1)),
           mainPanel(
             plotOutput("mort"),
             plotOutput("pop")
           )
           )
           ),
  tabPanel("Reproduction",
           sidebarLayout(
             sidebarPanel(
           checkboxInput("spawn", 
                        label = h4("Spawning"), 
                        value = FALSE)),
           mainPanel(
             plotOutput("repro")
           )
           )
           ),
             
  tabPanel("Nutrient Regeneration",
           sidebarLayout(
             sidebarPanel(
           checkboxInput("nut", 
                         label = h4("Nutrient Regeneration"), 
                         value = FALSE)
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Phosphorous Assimilation Efficiency",plotOutput("phos_ae")),
               tabPanel("Prey Phosphorous Concentration",plotOutput("prey_phos_conc")),
               tabPanel("Predator Phosphorous Concentration",plotOutput("pred_phos_conc")),
               tabPanel("Nitrogen Assimilation Efficiency",plotOutput("nit_ae")),
               tabPanel("Prey Nitrogen Concentration",plotOutput("prey_nit_conc")),
               tabPanel("Predator Nitrogen Concentration",plotOutput("pred_nit_conc"))
             )
           )
           )
  ),
  
  tabPanel("Contaminant Accumulation",
           sidebarLayout(
             sidebarPanel(
           checkboxInput("contaminant", 
                         label = h4("Contaminant Accumulation"), 
                         value = FALSE),
           radioButtons("cont_acc", 
                        label = "Contaminant Analysis Model", 
                        choices = list("Net Assimilation Efficiency (Model 1)" = 1,
                                       "Gross Assimilation Efficiency + Temperature- and Size-Dependent Depuration (Model 2)" = 2),
                        selected = 1),
#            fileInput("cont_conc_prey", label = "Contaminant Concentration in Prey Items"),
#            fileInput("cont_ass", label = "Contaminant Assimilation Efficiencies in Prey Items"),
           numericInput("init_pred_conc",
                        label = "Initial Predator Concentration (ppm)",
                        value = NA)
),
mainPanel(
  tabsetPanel(
    tabPanel("Contaminant Assimilation Efficiency",plotOutput("cont_ae")),
    tabPanel("Prey Contaminant Concentration",plotOutput("prey_cont_conc")),
    tabPanel("Contaminant Transfer Efficiency",plotOutput("trans_eff"))
  )
)
)
)),
  
tabPanel("Output",     
           sidebarLayout(
          sidebarPanel(   
            
      h3("Table Output"), 
      
      numericInput("int", 
                   label = "Interval (days)", 
                   value = 1),
      
      selectizeInput(
        "var1", "Individual Variables (click in box for options)", choices = list("Day"="Day", 
                                           "Temperature"="Temperature",
                                           "Weight (g)"="Weight",
                                           "Net Production (g)"="Net.Production.Grams",
                                           "Net Production (J)"="Net.Production.Joules",
                                           "Cumulative Net Production (g)"="Cum.Net.Production.Grams",
                                           "Cumulative Net Production (J)"="Cum.Net.Production.Joules",
                                           "Gross Production (g)"="Gross.Production.Grams",
                                           "Gross Production (J)"="Gross.Production.Joules",
                                           "Cumulative Gross Production (g)"="Cum.Gross.Production.Grams",
                                           "Cumulative Gross Production (J)"="Cum.Gross.Production.Joules",
                                           "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.Joules",
                                           "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.Grams",
                                           "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.Joules",
                                           "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.Grams",
                                           "Consumption (g)"="Prey.Tot.Ind.Grams",
                                           "Consumption (J)"="Prey.Tot.Ind.Joules",
                                           "Cumulative Consumption (g)"="Cum.Prey.Tot.Ind.Grams",
                                           "Cumulative Consumption (J)"="Cum.Prey.Tot.Ind.Joules",
                                           "Consumption by Prey (J)"=choicePreyJoules,
                                           "Consumption by Prey (g)"=choicePreyGrams,
                                           "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate",
                                           "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate",
                                           "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate",
                                           "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate",
                                           "Starting Predator Energy Density (J/d)"="Initial.Predator.Energy.Density",
                                           "Ending Predator Energy Density (J/d)"="Final.Predator.Energy.Density",
                                           "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density",
                                           "Gametic Production (g)"="Gametic.Production.Grams",
                                           "Gametic Production (J)"="Gametic.Production.Joules"),
                                           multiple = TRUE,selected = c("Day","Temperature","Weight")),
      
      selectizeInput(
        "var2", "Population Variables", choices = list(
                                                                      "Population Number"="Population.Number",
                                                                      "Population Biomass (g)"="Population.Biomass",
                                                                      "Consumption by Population (g)"="Prey.Tot.Pop.Grams",
                                                                      "Consumption by Population (J)"="Prey.Tot.Pop.Joules",
                                                                      "Cumulative Consumption by Population (g)"="Cum.Prey.Tot.Pop.Grams",
                                                                      "Cumulative Consumption by Population (J)"="Cum.Prey.Tot.Pop.Joules",
                                                                      "Population Consumption by Prey (J)"=choicePreyPopJoules,
                                                                      "Population Consumption by Prey (g)"=choicePreyPopGrams,
                                                                      "Mortality Number"="Mortality.number",
                                                                      "Mortality Biomass (g)"="Mortality.Grams"),
        multiple = TRUE,selected = c()),
      
      selectizeInput(
        "var3", "Nutrient Regeneration Variables", choices = list( 
                                                                      "Nitrogen Egestion (g)"="Nitrogen.Egestion",
                                                                      "Phosphorous Egestion (g)"="Phosphorous.Egestion",
                                                                      "N:P Egestion (mass ratio)"="N.to.P.Egestion",
                                                                      "Nitrogen Excretion (g)"="Nitrogen.Excretion",
                                                                      "Phosphorous Excretion (g)"="Phosphorous.Excretion",
                                                                      "N:P Excretion (mass ratio)"="N.to.P.Excretion",
                                                                      "Nitrogen Consumption (g)"="Nitrogen.Consumption",
                                                                      "Phosphorous Consumption (g)"="Phosphorous.Consumption",
                                                                      "N:P Consumption (mass ratio)"="N.to.P.Consumption",
                                                                      "Nitrogen Growth (g)"="Nitrogen.Growth",
                                                                      "Phosphorous Growth (g)"="Phosphorous.Growth",
                                                                      "N:P Growth (mass ratio)"="N.to.P.Growth"),
        multiple = TRUE,selected = c()),
      
      selectizeInput(
        "var4", "Contaminant Analysis Variables", choices = list(
                                                                      "Clearance Rate (/d)"="Clearance.Rate",
                                                                      "Contaminant Uptake (ug/d)"="Contaminant.Uptake",
                                                                      "Contaminant Burden (ug)"="Contaminant.Burden",
                                                                      "Contaminant Predator Concentration (ppm)"="Contaminant.Predator.Concentration"),
        multiple = TRUE,selected = c()),

      div(style="height: 40px;",downloadButton('downloadData', 'Download Table')),
      
      h3("Plot Output"),
      
      selectInput("xaxis", 
                  label = "X Axis",
                  choices = list("Day"="Day", 
                                 "Temperature"="Temperature",
                                 "Weight (g)"="Weight",
                                 "Population Number"="Population.Number",
                                 "Population Biomass (g)"="Population.Biomass",
                                 "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.Joules",
                                 "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.Joules",
                                 "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate",
                                 "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate",
                                 "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate",
                                 "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate",
                                 "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.Grams",
                                 "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.Grams",
                                 "Initial Predator Energy Density (J/d)"="Initial.Predator.Energy.Density",
                                 "Final Predator Energy Density (J/d)"="Final.Predator.Energy.Density",
                                 "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density",
                                 "Gross Production (g)"="Gross.Production.Grams",
                                 "Gross Production (J)"="Gross.Production.Joules",
                                 "Cumulative Gross Production (g)"="Cum.Gross.Production.Grams",
                                 "Cumulative Gross Production (J)"="Cum.Gross.Production.Joules",
                                 "Gametic Production (g)"="Gametic.Production.Grams",
                                 "Gametic Production (J)"="Gametic.Production.Joules",
                                 "Net Production (g)"="Net.Production.Grams",
                                 "Net Production (J)"="Net.Production.Joules",
                                 "Cumulative Net Production (g)"="Cum.Net.Production.Grams",
                                 "Cumulative Net Production (J)"="Cum.Net.Production.Joules",
                                 "Prey Tot Ind (g)"="Prey.Tot.Ind.Grams",
                                 "Prey Tot Ind (J)"="Prey.Tot.Ind.Joules",
                                 "Cumulative Prey Tot Ind (g)"="Cum.Prey.Tot.Ind.Grams",
                                 "Cumulative Prey Tot Ind (J)"="Cum.Prey.Tot.Ind.Joules",
                                 "Prey Tot Pop (g)"="Prey.Tot.Pop.Grams",
                                 "Prey Tot Pop (J)"="Prey.Tot.Pop.Joules",
                                 "Cumulative Prey Tot Pop (g)"="Cum.Prey.Tot.Pop.Grams",
                                 "Cumulative Prey Tot Pop (J)"="Cum.Prey.Tot.Pop.Joules",
                                 "Mortality number"="Mortality.number",
                                 "Mortality (g)"="Mortality.Grams", 
                                 "Nitrogen Egestion (g)"="Nitrogen.Egestion",
                                 "Phosphorous Egestion (g)"="Phosphorous.Egestion",
                                 "N to P Egestion (mass ratio)"="N.to.P.Egestion",
                                 "Nitrogen Excretion (g)"="Nitrogen.Excretion",
                                 "Phosphorous Excretion (g)"="Phosphorous.Excretion",
                                 "N to P Excretion (mass ratio)"="N.to.P.Excretion",
                                 "Nitrogen Consumption (g)"="Nitrogen.Consumption",
                                 "Phosphorous Consumption (g)"="Phosphorous.Consumption",
                                 "N to P Consumption (mass ratio)"="N.to.P.Consumption",
                                 "Nitrogen Growth (g)"="Nitrogen.Growth",
                                 "Phosphorous Growth (g)"="Phosphorous.Growth",
                                 "N to P Growth (mass ratio)"="N.to.P.Growth",
                                 "Clearance Rate (/d)"="Clearance.Rate",
                                 "Contaminant Uptake (ug/d)"="Contaminant.Uptake",
                                 "Contaminant Burden (ug)"="Contaminant.Burden",
                                 "Contaminant Predator Concentration (ppm)"="Contaminant.Predator.Concentration"),
                  selected = "Day", multiple=FALSE),
 
      selectInput("yaxis", 
                  label = "Y Axis",
                  choices = list("Day"="Day", 
                                 "Temperature"="Temperature",
                                 "Weight (g)"="Weight",
                                 "Population Number"="Population.Number",
                                 "Population Biomass (g)"="Population.Biomass",
                                 "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.Joules",
                                 "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.Joules",
                                 "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate",
                                 "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate",
                                 "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate",
                                 "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate",
                                 "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.Grams",
                                 "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.Grams",
                                 "Initial Predator Energy Density (J/d)"="Initial.Predator.Energy.Density",
                                 "Final Predator Energy Density (J/d)"="Final.Predator.Energy.Density",
                                 "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density",
                                 "Gross Production (g)"="Gross.Production.Grams",
                                 "Gross Production (J)"="Gross.Production.Joules",
                                 "Cumulative Gross Production (g)"="Cum.Gross.Production.Grams",
                                 "Cumulative Gross Production (J)"="Cum.Gross.Production.Joules",
                                 "Gametic Production (g)"="Gametic.Production.Grams",
                                 "Gametic Production (J)"="Gametic.Production.Joules",
                                 "Net Production (g)"="Net.Production.Grams",
                                 "Net Production (J)"="Net.Production.Joules",
                                 "Cumulative Net Production (g)"="Cum.Net.Production.Grams",
                                 "Cumulative Net Production (J)"="Cum.Net.Production.Joules",
                                 "Prey Tot Ind (g)"="Prey.Tot.Ind.Grams",
                                 "Prey Tot Ind (J)"="Prey.Tot.Ind.Joules",
                                 "Cumulative Prey Tot Ind (g)"="Cum.Prey.Tot.Ind.Grams",
                                 "Cumulative Prey Tot Ind (J)"="Cum.Prey.Tot.Ind.Joules",
                                 "Prey Tot Pop (g)"="Prey.Tot.Pop.Grams",
                                 "Prey Tot Pop (J)"="Prey.Tot.Pop.Joules",
                                 "Cumulative Prey Tot Pop (g)"="Cum.Prey.Tot.Pop.Grams",
                                 "Cumulative Prey Tot Pop (J)"="Cum.Prey.Tot.Pop.Joules",
                                 "Mortality number"="Mortality.number",
                                 "Mortality (g)"="Mortality.Grams", 
                                 "Nitrogen Egestion (g)"="Nitrogen.Egestion",
                                 "Phosphorous Egestion (g)"="Phosphorous.Egestion",
                                 "N to P Egestion (mass ratio)"="N.to.P.Egestion",
                                 "Nitrogen Excretion (g)"="Nitrogen.Excretion",
                                 "Phosphorous Excretion (g)"="Phosphorous.Excretion",
                                 "N to P Excretion (mass ratio)"="N.to.P.Excretion",
                                 "Nitrogen Consumption (g)"="Nitrogen.Consumption",
                                 "Phosphorous Consumption (g)"="Phosphorous.Consumption",
                                 "N to P Consumption (mass ratio)"="N.to.P.Consumption",
                                 "Nitrogen Growth (g)"="Nitrogen.Growth",
                                 "Phosphorous Growth (g)"="Phosphorous.Growth",
                                 "N to P Growth (mass ratio)"="N.to.P.Growth",
                                 "Clearance Rate (/d)"="Clearance.Rate",
                                 "Contaminant Uptake (ug/d)"="Contaminant.Uptake",
                                 "Contaminant Burden (ug)"="Contaminant.Burden",
                                 "Contaminant Predator Concentration (ppm)"="Contaminant.Predator.Concentration"),
                  selected = "Temperature", multiple=FALSE),
 
      selectInput("yaxis2", 
                  label = "Y Axis 2",
                  choices = list("Day"="Day", 
                                 "Temperature"="Temperature",
                                 "Weight (g)"="Weight",
                                 "Population Number"="Population.Number",
                                 "Population Biomass (g)"="Population.Biomass",
                                 "Specific Growth Rate (J/g/d)"="Specific.Growth.Rate.Joules",
                                 "Specific Consumption Rate (J/g/d)"="Specific.Consumption.Rate.Joules",
                                 "Specific Egestion Rate (J/g/d)"="Specific.Egestion.Rate",
                                 "Specific Excretion Rate (J/g/d)"="Specific.Excretion.Rate",
                                 "Specific Respiration Rate (J/g/d)"="Specific.Respiration.Rate",
                                 "Specific SDA Rate (J/g/d)"="Specific.SDA.Rate",
                                 "Specific Consumption Rate (g/g/d)"="Specific.Consumption.Rate.Grams",
                                 "Specific Growth Rate (g/g/d)"="Specific.Growth.Rate.Grams",
                                 "Initial Predator Energy Density (J/d)"="Initial.Predator.Energy.Density",
                                 "Final Predator Energy Density (J/d)"="Final.Predator.Energy.Density",
                                 "Mean Prey Energy Density (J/g)"="Mean.Prey.Energy.Density",
                                 "Gross Production (g)"="Gross.Production.Grams",
                                 "Gross Production (J)"="Gross.Production.Joules",
                                 "Cumulative Gross Production (g)"="Cum.Gross.Production.Grams",
                                 "Cumulative Gross Production (J)"="Cum.Gross.Production.Joules",
                                 "Gametic Production (g)"="Gametic.Production.Grams",
                                 "Gametic Production (J)"="Gametic.Production.Joules",
                                 "Net Production (g)"="Net.Production.Grams",
                                 "Net Production (J)"="Net.Production.Joules",
                                 "Cumulative Net Production (g)"="Cum.Net.Production.Grams",
                                 "Cumulative Net Production (J)"="Cum.Net.Production.Joules",
                                 "Prey Tot Ind (g)"="Prey.Tot.Ind.Grams",
                                 "Prey Tot Ind (J)"="Prey.Tot.Ind.Joules",
                                 "Cumulative Prey Tot Ind (g)"="Cum.Prey.Tot.Ind.Grams",
                                 "Cumulative Prey Tot Ind (J)"="Cum.Prey.Tot.Ind.Joules",
                                 "Prey Tot Pop (g)"="Prey.Tot.Pop.Grams",
                                 "Prey Tot Pop (J)"="Prey.Tot.Pop.Joules",
                                 "Cumulative Prey Tot Pop (g)"="Cum.Prey.Tot.Pop.Grams",
                                 "Cumulative Prey Tot Pop (J)"="Cum.Prey.Tot.Pop.Joules",
                                 "Mortality number"="Mortality.number",
                                 "Mortality (g)"="Mortality.Grams", 
                                 "Nitrogen Egestion (g)"="Nitrogen.Egestion",
                                 "Phosphorous Egestion (g)"="Phosphorous.Egestion",
                                 "N to P Egestion (mass ratio)"="N.to.P.Egestion",
                                 "Nitrogen Excretion (g)"="Nitrogen.Excretion",
                                 "Phosphorous Excretion (g)"="Phosphorous.Excretion",
                                 "N to P Excretion (mass ratio)"="N.to.P.Excretion",
                                 "Nitrogen Consumption (g)"="Nitrogen.Consumption",
                                 "Phosphorous Consumption (g)"="Phosphorous.Consumption",
                                 "N to P Consumption (mass ratio)"="N.to.P.Consumption",
                                 "Nitrogen Growth (g)"="Nitrogen.Growth",
                                 "Phosphorous Growth (g)"="Phosphorous.Growth",
                                 "N to P Growth (mass ratio)"="N.to.P.Growth",
                                 "Clearance Rate (/d)"="Clearance.Rate",
                                 "Contaminant Uptake (ug/d)"="Contaminant.Uptake",
                                 "Contaminant Burden (ug)"="Contaminant.Burden",
                                 "Contaminant Predator Concentration (ppm)"="Contaminant.Predator.Concentration"),
                  selected = "Weight", multiple=FALSE)
      ),
           
      mainPanel(
        tabsetPanel(
          tabPanel("Table",tableOutput("table")),
          #tabPanel("Table by Prey",tableOutput("tableprey")),
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary",tableOutput("summary"))

)
)
)
), 
 tabPanel(
   img(src = "FB4.png", height = 50, width = 40))
))