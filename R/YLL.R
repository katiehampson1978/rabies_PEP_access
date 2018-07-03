# YLL calculations
# Created by Katie Hampson - 26 jul 2017, edited 31 Jul 17
# Currently uses the DALY rabies estimates at 5y intervals ~i.e. prob of dying from rabies at each interval
# If use Annual WHO LE tables - adjust DALY rabies estimates

# Previously required (but this step now removed):
# library(LifeTable) # https://github.com/jdthorpe/LifeTable

# Breaks defaults to five years

YLLcalc <- function(DALYtable, LTvalues, cause, discount = 0.03, C = 0.1658, Beta = 0.04, alpha = 1, breaks = "5y"){
  # Calculates YLL (age-weighted and discounted) from rabies or Adverse Events (AE) from Nerve Tissue Vaccines (NTVs)
  # Inputs: mortality rate (country-specific) and cause of death (rabies or NTV) and interval for life tables
  #         If Time discounting: discount = 0.03
  #         If Age-weighting (alpha=1): C = 0.1658 & Beta = 0.04. If no age-weighting (alpha=0): C = 1 & Beta = 0
  # Returns: Annual YLL for avg death in country of interest (to be multiplied by the number of deaths)
  if(breaks == "annual"){interval = rep(1, length(LTvalues))}
    else {interval=c(1, 4, rep(5,19))}

  LE <- rep(LTvalues, interval)
  curr_age <- seq(from = 0, by = 1, length.out = length(LE))
  death_age <- round(curr_age + LE)

  cost <- sapply(1:(length(curr_age)), function(i){
    ages <- curr_age[i]:death_age[i]
    future <- (1:length(ages))-1
    disc_year <- exp(-discount*(future))
    weighting = ifelse(alpha==0, 1, C*ages^alpha*exp(-Beta*ages))
    return(sum(disc_year*weighting))
  })

  death_pc_prop <- DALYtable$death_pc/sum(DALYtable$death_pc)
  burden_year <- rep(death_pc_prop/interval, interval)
  # ylls = list(yll = sum(cost*burden_year), U5yll = sum(cost[1:5]*burden_year[1:5])))
  return(sum(cost*burden_year))
  # return(ylls)
}

# TESTING
# WHO_LE <- read.csv("data/LifeTables/WHO_LE.csv")  # WHO Annex B http://www.who.int/healthinfo/global_burden_disease/GlobalDALYmethods_2000_2015.pdf?ua=1
# DALYrabies <- read.csv("data/LifeTables/DALY_params_rabies_TZ_1yr_breaks.csv") # for WHO_LE
# DALYrabies <- read.csv("data/LifeTables/DALY_params_rabies_TZ_ageclass.csv") # for GBD2010
# # DALYrabies <- read.csv("data/DALY_params_rabies.csv")  # Probability of rabies death at each age class
#
# YLLcalc(DALYtable=DALYrabies_input, LTvalues=GBD2010$LE, cause="rabies", discount=0.03, C=1, Beta=0, alpha=0, breaks="5y")  # check not discounting twice!
# YLLcalc(DALYtable=DALYrabies_input, LTvalues=LE2020$value[which(LE2020$country_code=="TZA")], cause="rabies", discount=0.03, C=1, Beta=0, alpha=0, breaks="5y")  # check not discounting twice!
# YLLcalc(DALYtable=DALYrabies_input, LTvalues=LE2020$value[which(LE2020$country_code=="CUB")], cause="rabies", discount=0.03, C=1, Beta=0, alpha=0, breaks="5y")  # check not discounting twice!
# YLLcalc(DALYtable=DALYrabies_input, LTvalues=LE2020$value[which(LE2020$country_code=="SLE")], cause="rabies", discount=0.03, C=1, Beta=0, alpha=0, breaks="5y")  # check not discounting twice!


# FUNCTION FOR UNDER 5 YLL!
YLLage <- function(DALYtable, LTvalues, cause, discount = 0.03, alpha = 1, Uage = 5){
  # Calculates YLL from rabies given an age cut off e.g. U5s - not age weighted
  #         If Time discounting: discount = 0.03
  # Returns: U5 YLL for a rabies case * proportion of cases U5 for that country - to be multiplied by country specific deaths
  interval=c(1, 4, rep(5,19))
  LE <- rep(LTvalues, interval)
  curr_age <- seq(from = 0, by = 1, length.out = length(LE))
  death_age <- round(curr_age + LE)

  cost <- sapply(1:(length(curr_age)), function(i){
    ages <- curr_age[i]:death_age[i]
    future <- (1:length(ages))-1
    disc_year <- exp(-discount*(future))
    return(sum(disc_year))
  })

  death_pc_prop <- DALYtable$death_pc/sum(DALYtable$death_pc)
  burden_year <- rep(death_pc_prop/interval, interval)

  # return(sum(cost*burden_year))
  return(sum(cost[1:Uage]*burden_year[1:Uage]))
  # sum(cost[1:Uage])
  # sum(burden_year[1:Uage])
}

# TESTING
# YLLage(DALYtable=DALYrabies_input, LTvalues=LE2020$value[which(LE2020$country_code=="SLE")], discount=0.03, Uage = 5)  # check not discounting twice!


