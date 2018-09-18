# YLL calculations
# Currently uses the DALY rabies estimates at 5y intervals ~i.e. prob of dying from rabies at each interval
# If use Annual WHO LE tables - adjust DALY rabies estimates

# Previously required (but this step now removed):
# library(LifeTable) # https://github.com/jdthorpe/LifeTable

# Breaks defaults to five years

# TOTAL YLL
YLLcalc <- function(DALYtable, LTvalues, cause, discount = 0.03, C = 0.1658,
                    Beta = 0.04, alpha = 1, breaks = "5y"){
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

  return(sum(cost*burden_year))
}

# UNDER 5 YLL
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

  return(sum(cost[1:Uage]*burden_year[1:Uage]))
}
