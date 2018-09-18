# PEP outputs calculations
# Pricing also comes from http://www.paho.org/hq/index.php?option=com_content&view=article&id=1864&Itemid=4135&lang=en

source("R/scenario_params.R") # parameters and functions for gavi support and phasing

# Function that returns number of vials and costs of PEP given clinic throughput, PEP regimen, and
# according to whether patient completes course
PEPoutputs = function(regimen, country, GAVI_status){

  # Index for identifying PEP scenario
  complete_urban <- which(vacc$throughput=="High" & vacc$regimen== regimen & vacc$completeness=="Complete")
  imperfect_urban <- which(vacc$throughput=="High" & vacc$regimen== regimen & vacc$completeness=="Incomplete")
  complete_rural <- which(vacc$throughput=="Low" & vacc$regimen== regimen & vacc$completeness=="Complete")
  imperfect_rural <- which(vacc$throughput=="Low" & vacc$regimen== regimen & vacc$completeness=="Incomplete")

  n_vials_complete_urban <- vacc$vial[complete_urban]
  n_vials_imperfect_urban <- vacc$vial[imperfect_urban]
  n_vials_complete_rural <- vacc$vial[complete_rural]
  n_vials_imperfect_rural <- vacc$vial[imperfect_rural]

  n_vials_complete <- n_vials_complete_urban*data$prop_urban[which(data$country==country)]+
    n_vials_complete_rural*(1-data$prop_urban[which(data$country==country)])
  n_vials_imperfect <- n_vials_imperfect_urban*data$prop_urban[which(data$country==country)]+
    n_vials_imperfect_rural*(1-data$prop_urban[which(data$country==country)])

  if(GAVI_status=="none"){
    cost_complete <- n_vials_complete*data$price_per_vial[which(data$country==country)] +
    data$cost_first_visit[which(data$country==country)] +
    data$cost_followup_visit[which(data$country==country)]*(vacc$nvisit[complete_urban]-1)

  cost_imperfect <- n_vials_complete*data$price_per_vial[which(data$country==country)] +
    data$cost_first_visit[which(data$country==country)] +
    data$cost_followup_visit[which(data$country==country)]*(vacc$nvisit[imperfect_urban]-1)

   } else{
    price_per_vial<-gavi_vaccine_price
    cost_complete <- n_vials_complete*price_per_vial +
      data$cost_first_visit[which(data$country==country)] +
      data$cost_followup_visit[which(data$country==country)]*(vacc$nvisit[complete_urban]-1)

    cost_imperfect <- n_vials_complete*price_per_vial +
      data$cost_first_visit[which(data$country==country)] +
      data$cost_followup_visit[which(data$country==country)]*(vacc$nvisit[imperfect_urban]-1)
  }

  output <- data.frame(vials_complete = n_vials_complete,
                       vials_imperfect = n_vials_imperfect,
                       costs_complete = cost_complete,
                       costs_imperfect= cost_imperfect)
  return(output)
}

# Gavi support introduced
PEP_ts = function(phase, VaxRegimen, country){
  support = gavi_intro(phase) # Time series of Gavi support
  regimen = rep(VaxRegimen, length(support)) # Start with SQ regimen
  regimen[which(support=="support")] <- "IPC" # Replace with Gavi support regimen IPC

  pep_outputs = data.frame(vials_complete = rep(NA, length(support)), # Create a data frame for pep outputs
                           vials_imperfect = rep(NA, length(support)),
                           costs_complete = rep(NA, length(support)),
                           costs_imperfect = rep(NA, length(support)))

  for(i in 1:length(support)){ # Run through years of Gavi support to create PEP outputs
    pep_outputs[i,] = PEPoutputs(regimen[i], country, as.character(support[i]))
  }
  pep_outputs
}

# Function to create a timeseries of proportion of ID regimens
prop_regimen = function(phase, prop_ID){
  support=gavi_intro(phase)
  pID = rep(1, length(support))
  pID[which(support == "none")] <- prop_ID
  pID
  }

# Equine RIG prices (1000 IU vial) over time from PAHO
# 2014-2018: $35; $35; $38.5; $40; $41.5 # therefore assume price of RIG in 2020 is $44.5
# Vaccine prices over time from PAHO (vero cells)
# 2013-2018: $12.8

# UNICEF have not previously used RIG before
