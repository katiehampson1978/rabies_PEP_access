#######################################################################
# PEP outputs calculations
# Created by Katie Hampson - 31 Jul 17
# Edited on 22/08/17 by Laura C. - high throughput in urban population, low throughput in rural population

# Function that returns number of vials and costs of PEP given clinic throughput, PEP regimen, and
# according to whether patient completes course

PEPoutputs = function(regimen, country, GAVI_status){
  # index for identifying PEP scenario
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
    price_per_vial<- data$price_per_vial
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
# regimen = VaxRegimen
# PEPoutputs("IM", "Tanzania", "none")
# PEPoutputs("IPC", "Tanzania", "none")

#######################################################################
