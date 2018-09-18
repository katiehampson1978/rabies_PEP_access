# Sensitivity analysis

# RIG_status can be either no RIG ("none"), RIG to high risk bites ("high risk"), or RIG to all ("all")
# GAVI_status can be either no GAVI support ("none"), GAVI support for access ("access"), or
# GAVI support for access and seeking - either indiscriminate or judicious (IBCM programme implemented)

decision_tree_draw<-function(country, horizon, GAVI_status, DogVax_TF, VaxRegimen,
                             DALYrabies, LE, RIG_status, discount, breaks, IBCM){

  # Ensure all inputs are correct:
  if(!GAVI_status%in%c("none","base","low","high")){
    stop("'GAVI_status' must take one of the following arguments: 'none','base','low','high'.")
  }
  if(!is.logical(DogVax_TF)){stop("DogVax_TF must be either true or false.")}

  if(!RIG_status %in% c("none","high risk","all")){stop("'RIG_status' must take one of the following arguments: 'none', 'high risk', or 'all'.")}

  if(!VaxRegimen %in% c("Updated TRC","IPC")){stop("'Vaccine regimen' must take one of the following arguments: 'Updated TRC', or 'IPC'.")}

  if(!is.logical(IBCM)){stop("IBCM must be either true or false")}


  # Set conditions:
  # 1. country
  index = which(data$country==country)
  GBP = as.character(data$Business.Plan.Phase[index])

  # if necessary, select country-specific LE
  if(is.data.frame(LE)){
    country_code = as.character(data$CODE[which(data$country==country)])
    LE <- LE$value[which(LE$country_code==country_code)]
  }
  #print(LE)

  # 2. GAVI support
  prop_ID <- data$ID[index]

  if(GAVI_status!="none") {
    if(GBP=="I"){gavi_phase = gavi_phaseI}
    if(GBP=="II"){gavi_phase = gavi_phaseII}
    if(GBP=="III"){gavi_phase = gavi_phaseIII}
    if(GBP=="0"){gavi_phase = gavi_phaseI} # Gavi-eligible countries in GBP0 (americas) move to phase I for Gavi
  } else {
    gavi_phase = gavi_phase0 # STATUS QUO re: type of PEP used
  }
  NTV_TF = (country == "Ethiopia") # With no GAVI support, Ethiopia uses NTV

  # 3. Bite incidence - from suspect rabid dogs
  # Mean bites per rabid dog
  rabid_dog_bites = 0.38

  # create human pop table for the horizon of interest: 2020 - 2035
  pop2020_col <- which(colnames(data)=="pop2020") # lets take 2018 and 2019 as endemic
  pop_cols <- c(pop2020_col:(pop2020_col+(horizon-1))) # MINUS 1 because you are adding horizon i.e. creates an extra yrs!
  pop <- data[index, pop_cols]

  # Create dog table for the horizon of interest
  dogs <- dogs[which(dogs$country == country),] # make sure data and dogs have same countries
  dog2020_col <- which(colnames(dogs)=="dogpop2020")
  dogpop_cols <- c(dog2020_col:(dog2020_col + (horizon-1)))
  dogpop <- dogs[,dogpop_cols]

  if(DogVax_TF==T) {
    # Select a random number from 0-100 to select an incidence trajectory, i.e. a column in elimination_traj
    elim_index <- round(runif(1, 1, 100))

    # Create elimination trajectory for the time horizon (-1 because first year will be endemic susp_bite_inc)
    Y1 = grep("2020", names(elimination_traj))
    yr_index = Y1:(Y1+horizon-1)
    trajectories = elimination_traj[which(elimination_traj$phase==GBP), yr_index]
    ET <- unlist(trajectories[elim_index,])

    # Translate into a COUNTRY-SPECIFIC rabid dog bite incidence i.e. bites/ population
    susp_bite_inc_traj = as.numeric((dogpop * rabid_dog_bites * ET)/ pop)
    NAs = which(is.na(susp_bite_inc_traj)) ##  Fill in bite incidence for endemic years
    if(length(NAs)==0){ susp_bite_inc <- unlist(susp_bite_inc_traj)
    } else {
      susp_bite_inc_traj[NAs] <- (dogpop[1:length(NAs)] * rabid_dog_bites * rnorm(length(NAs), mean = rabies$inc, sd = rabies$sd))/pop[1:length(NAs)]
      susp_bite_inc <- unlist(susp_bite_inc_traj)
    }

    # Else if no dog vaccination:
  } else {
    susp_bite_inc <-  (dogpop * rabid_dog_bites * rnorm(horizon, mean = rabies$inc, sd = rabies$sd))/pop
  }

  # 4. Bite incidence - from non-suspect dogs
  mean <- data$bite_inc_non_susp[index]
  non_susp_bite_inc <- rep(rtriangle(n=1, a=mean-0.2*mean, b=mean+0.2*mean), horizon) # assume mean +/- 20% uncertainty

  # 5. Probability suspected dog bite is rabid
  # Country-specific, (no change if if DogVax == T bc this is captured in rabid dog bite incidence!)
  prop_bites_rabid <- rbinom(n=1, size=data$p_rabid_n[index], prob=data$p_rabid[index])/data$p_rabid_n[index]

  # 6. Probability of developing rabies if bitten by rabid dog - Differences due to age and bite site ignored - separate analysis justifies this
  prob_transmission <- mixture_model(1)

  if(GAVI_status=="base") {
    # 7. Health seeking
    prob_seek_pep_susp <- gavi_phasing(gavi_phase, p_SQ = data$p_seek[index], p_step$base, p_increment, p_seek_cap$base)
    prob_seek_pep_non <- gavi_phasing(phase = gavi_phase, p_SQ = data$pseek_norm_SQ[index], step = p_step$base, increment = 0, cap = data$pseek_norm_SQ[index]+p_step$base)

    # 8. Access to PEP
    prob_receive_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_receive[index], step = p_step$base, increment = p_increment, cap = p_receive_cap$base) #### With GAVI support, PEP is always available

    # 9. Rate of PEP completion
    prob_complete_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_complete[index], step = p_step$base, increment = p_increment, cap = p_complete_cap$base)

  }else if(GAVI_status=="low") {
    prob_seek_pep_susp <- gavi_phasing(gavi_phase, p_SQ = data$p_seek[index], p_step$low, p_increment, p_seek_cap$low)
    prob_seek_pep_non <- gavi_phasing(phase = gavi_phase, p_SQ = data$pseek_norm_SQ[index], step = p_step$low, increment = 0, cap = data$pseek_norm_SQ[index]+p_step$low)
    prob_receive_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_receive[index], step = p_step$low, increment = p_increment, cap = p_receive_cap$low) #### With GAVI support, PEP is always available
    prob_complete_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_complete[index], step = p_step$low, increment = p_increment, cap = p_complete_cap$low)

  }else if(GAVI_status=="high"){
    prob_seek_pep_susp <- gavi_phasing(gavi_phase, p_SQ = data$p_seek[index], p_step$high, p_increment, p_seek_cap$high)
    prob_seek_pep_non <- gavi_phasing(phase = gavi_phase, p_SQ = data$pseek_norm_SQ[index], step = p_step$high, increment = 0, cap = data$pseek_norm_SQ[index]+p_step$high)
    prob_receive_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_receive[index], step = p_step$high, increment = p_increment, cap = p_receive_cap$high) #### With GAVI support, PEP is always available
    prob_complete_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_complete[index], step = p_step$high, increment = p_increment, cap = p_complete_cap$high)

  }else{
    prob_seek_pep_susp <- rbinom(n=horizon, size=data$p_seek_n[index], prob=data$p_seek[index])/data$p_seek_n[index]
    prob_seek_pep_non <- data$pseek_norm_SQ[index] # STATUS QUO
    prob_receive_pep <- rbinom(n=1, size=data$p_receive_n[index], prob=data$p_receive[index])/data$p_receive_n[index]
    prob_complete_pep <- rbinom(n=1, size=data$p_complete_n[index], prob=data$p_complete[index])/data$p_complete_n[index]
  }

  # 10. Access to RIG
  prop_RIG_urban = 0.15
  RIG_risk = data$prop_urban[index] * prop_RIG_urban
  if(RIG_status=="none"){ prob_RIG <-0
  }else{prob_RIG <- RIG_risk}

  # Alternative:
  # Probability of survival given avg number of doses
  # Everyone gets avg number of doses without GAVI support
  # With GAVI support there is complete adherence

  # 11. Probability that PEP prevents rabies
  # Depends on completion and RIG, and may differ for IM vs ID - see PEP simulations & Tz data analysis
  bin_confint_no_rigrisk <- Hmisc::binconf(params$p_prevent_given_complete_n, n=params$p_prevent_given_complete_n+1)[1] # Added n+1 so that some failures possible
  bin_confint_rigrisk <- 1

  prob_prevent_rabies_given_complete_pep <- rbinom(n=1, size=params$p_prevent_given_complete_n, # Current cut-off is 1 visit
                                                   prob=bin_confint_no_rigrisk)/params$p_prevent_given_complete_n

  prob_prevent_rabies_given_complete_pep_RIG <- ((1-RIG_risk) * prob_prevent_rabies_given_complete_pep) + (RIG_risk * bin_confint_rigrisk) # No marginal survival benefit of RIG

  prob_prevent_rabies_given_imperfect_pep <- rbinom(n=1, size=params$p_prevent_given_imperfect_n, # Current cut-off is 1 visit
                                                    prob=params$p_prevent_given_imperfect)/params$p_prevent_given_imperfect_n

  if(NTV_TF){ # IF NNTVs used - need to work out timeseries of p_prevent (w/o RIG, complete/incomplete)
    prob_prevent_rabies_given_complete_pep_NTV <- rbinom(n=1, size=params$p_prevent_given_complete_NTV_n,
                                                     prob=params$p_prevent_given_complete_NTV)/params$p_prevent_given_complete_NTV_n
    prob_prevent_rabies_given_imperfect_pep_NTV <- rbinom(n=1, size=params$p_prevent_given_insufficient_NTV_n, # Current cut-off is 1 visit
                                                      prob=params$p_prevent_given_insufficient_NTV)/params$p_prevent_given_insufficient_NTV_n

    prob_prevent_NTV_gavi_complete <- rep(prob_prevent_rabies_given_complete_pep_NTV, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support
    prob_prevent_NTV_gavi_complete_RIG <- rep(prob_prevent_rabies_given_complete_pep_NTV, length(gavi_intro(gavi_phase)))
    prob_prevent_NTV_gavi_imperfect <- rep(prob_prevent_rabies_given_imperfect_pep_NTV, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support

    prob_prevent_NTV_gavi_complete[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep
    prob_prevent_NTV_gavi_complete_RIG[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep_RIG
    prob_prevent_NTV_gavi_imperfect[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_imperfect_pep
  }

  # 12. Vaccine usage
  # If intramuscular, assume Essen 4-dose regimen
  # Some proportion of PEP is ID, some proportion is IM

  ts1 = 1-prop_regimen(gavi_phase, prop_ID) # IM
  ts2 = prop_regimen(gavi_phase, prop_ID) # ID
  pep = (ts1*PEP_ts(gavi_phase, "IM", country)) + (ts2* PEP_ts(gavi_phase, VaxRegimen, country))

  # In Ethiopia, some proportion of PEP is ID, some proportion is NTV
  if(NTV_TF){
    if(GAVI_status == "none"){pep = PEP_ts(gavi_phase0, "NTV", country)  # baseline with no Gavi support - continue to use NTVs!
    } else { pep = PEP_ts(gavi_phase, "NTV", country)  # baseline with no Gavi support - continue to use NTVs!
    }
  }

  # 13. Population/ demographic data for country
  population <- pop

  ################################
  # Calculate decision tree:
  ################################

  # Measures in the decision tree
  susp_bites = susp_bite_inc * population
  non_susp_bites = non_susp_bite_inc * population

  # IBCM
  # No IBCM means patients of bites by healthy and rabid dogs are treated the same
  if(IBCM == F) {
    patients = (susp_bites * prob_seek_pep_susp) + (non_susp_bites * prob_seek_pep_non)
    prob_receive_pep_IBCM = prob_receive_pep
  } else {
    # IBCM means patients of bites by healthy dogs are given a risk assessment
    # If rabies is still endemic - treat just 50% of healthy bite patients
    # If rabies eliminated - treat just 10% of healthy bite patients
    prob_receive_pep_IBCM = rep(prob_receive_pep[1], length(prob_receive_pep))
    prob_receive_pep_IBCM[which(gavi_intro(gavi_phase)=="support")] <- IBCM_endemic
    prob_receive_pep_IBCM[which(susp_bite_inc==0)] <- IBCM_elim
    patients = (susp_bites * prob_seek_pep_susp) + (non_susp_bites * prob_seek_pep_non * prob_receive_pep_IBCM)
    }

  # Human rabies deaths & deaths averted
  if(prob_RIG == 0){
    if(NTV_TF){
      prob_prevent_rabies_given_complete_pep <-  prob_prevent_NTV_gavi_complete
      prob_prevent_rabies_given_imperfect_pep <-  prob_prevent_NTV_gavi_imperfect
    }
    human_rabies_deaths = susp_bites * prop_bites_rabid * prob_transmission * # Top half of tree - those infected
      ((prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * (1-prob_prevent_rabies_given_complete_pep)) + # Those who complete PEP
         (prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep)  * (1-prob_prevent_rabies_given_imperfect_pep)) + # Those who do not complete PEP
         (prob_seek_pep_susp * (1-prob_receive_pep)) + # Those who seek PEP but don't receive it
         (1-prob_seek_pep_susp)) # Those who don't seek PEP

    human_rabies_deaths_averted = susp_bites * prop_bites_rabid * prob_transmission *
      ((prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * prob_prevent_rabies_given_complete_pep) +
         (prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep) * prob_prevent_rabies_given_imperfect_pep))

  } else {
    if(NTV_TF){
      prob_prevent_rabies_given_complete_pep <-  prob_prevent_NTV_gavi_complete_RIG
      prob_prevent_rabies_given_imperfect_pep <-  prob_prevent_NTV_gavi_imperfect
    } else {
      prob_prevent_rabies_given_complete_pep <- rep(prob_prevent_rabies_given_complete_pep, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support
      prob_prevent_rabies_given_complete_pep[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep_RIG
    }

    human_rabies_deaths = susp_bites * prop_bites_rabid * prob_transmission * # Top half of tree - those infected
      (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * (1-prob_prevent_rabies_given_complete_pep) + # Those who complete PEP
         prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep)  * (1-prob_prevent_rabies_given_imperfect_pep) + # Those who do not complete PEP
         prob_seek_pep_susp * (1-prob_receive_pep) + # Those who seek PEP but don't receive it
         (1-prob_seek_pep_susp)) # Those who don't seek PEP

    human_rabies_deaths_averted = susp_bites * prop_bites_rabid * prob_transmission *
      (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * prob_prevent_rabies_given_complete_pep +
         prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep) * prob_prevent_rabies_given_imperfect_pep)
  }

  U5_prop_deaths = sum(DALYrabies$death_pc[1:2]/sum(DALYrabies$death_pc))

  # Vaccine vials used per year
  vials_per_year = (patients * prob_receive_pep * prob_complete_pep * pep$vials_complete) +
    (patients * prob_receive_pep * (1-prob_complete_pep) * pep$vials_imperfect)

  # Costs of PEP - discounted
  future <- (1:horizon)-1
  cost_PEP_per_year = patients * (prob_receive_pep * (prob_complete_pep * pep$costs_complete +
                                                        (1-prob_complete_pep) * pep$costs_imperfect)) * exp(-discount*future)

  # Courses of RIG per year - and costs discounted
  p_RIG <- rep(0, length(gavi_intro(gavi_phase))) # create p_RIG adjusted for RIG support
  p_RIG[which(gavi_intro(gavi_phase)=="support")] <- prob_RIG
  courses_RIG_per_year = patients * prob_receive_pep * p_RIG
  cost_RIG = gavi_RIG_price
  cost_RIG_per_year = courses_RIG_per_year * cost_RIG * exp(-discount*future)

  # Introductory grant costings
  intro <- rep(0, length(gavi_intro(gavi_phase))) # intro timeline
  intro[which(gavi_intro(gavi_phase)=="support")[1]] <- gavi_intro_grant # Gavi Introduction grant
  intro_costs <- intro * exp(-discount*future)

  # TOTAL COSTS
  cost_per_year = cost_PEP_per_year + cost_RIG_per_year + intro_costs   ##

  # DALYs for rabies (discounted YLL)
  YLL_rabies_case = YLLcalc(DALYtable=DALYrabies, LTvalues=LE, cause="rabies", discount=discount, C=1, Beta=0, alpha=0, breaks=breaks) * exp(-discount*future) # check not discounting twice!
  YLL_U5 = YLLage(DALYtable=DALYrabies, LTvalues=LE, discount=discount, Uage = 5) * exp(-discount*future)
  YLL_rabies = human_rabies_deaths * YLL_rabies_case
  YLL_averted = human_rabies_deaths_averted * YLL_rabies_case
  YLL_U5_averted = human_rabies_deaths_averted * YLL_U5

  # Vaccinated and fully vaccinated persons
  vaccinated = patients * prob_receive_pep
  fully_vaccinated = vaccinated * prob_complete_pep

  years<-2020:2070

  ## p_seek:suspect and p_seek:healthy. for GAVI scenario 1 (status quo) is the same across years
  p_seek_rabid <- prob_seek_pep_susp
  p_seek_healthy <- prob_seek_pep_non

  ## p_receive. for GAVI scenario 1 (status quo) is the same across years
  p_receive <- prob_receive_pep
  p_receive_RIG <- p_RIG

  ## p_complete. for GAVI scenario 1 (status quo) is the same across years
  p_complete <- prob_complete_pep

  ## "Target population".
  population <- as.data.frame(t(population))
  rownames(population) <- NULL

  # target1: genuinely rabid exposed persons
  exposure_inc <- susp_bite_inc
  TargetPopulation_rabid <- as.numeric(unlist(exposure_inc * t(population)))

  # target2: persons bitten by healthy animals
  healthy_exposure_inc <- non_susp_bite_inc
  TargetPopulation_healthy <- as.numeric(unlist(healthy_exposure_inc * population))

  # RETURN RESULTS
  return(cbind.data.frame(year=years[1:horizon],
                          human_rabies_deaths = as.numeric(human_rabies_deaths),
                          U5_rabies_deaths = as.numeric(human_rabies_deaths* U5_prop_deaths),
                          vials_per_year = as.numeric(vials_per_year),
                          cost_per_year = as.numeric(cost_per_year),
                          human_rabies_deaths_averted = as.numeric(human_rabies_deaths_averted),
                          U5_rabies_deaths_averted = as.numeric(human_rabies_deaths_averted * U5_prop_deaths),
                          YLL_rabies = as.numeric(YLL_rabies),
                          YLL_averted = as.numeric(YLL_averted),
                          YLL_averted_U5 = as.numeric(YLL_U5_averted),
                          vaccinated = as.numeric(vaccinated),
                          fully_vaccinated = as.numeric(fully_vaccinated),
                          p_seek_rabid = as.numeric(p_seek_rabid),
                          p_seek_healthy = as.numeric(p_seek_healthy),
                          p_receive = as.numeric(p_receive),
                          p_receive_IBCM = prob_receive_pep_IBCM,
                          p_receive_RIG = p_RIG,
                          p_complete = as.numeric(p_complete),
                          TargetPopulation_rabid = as.numeric(TargetPopulation_rabid),
                          TargetPopulation_healthy = as.numeric(TargetPopulation_healthy),
                          RIG = as.numeric(courses_RIG_per_year),
                          gavi_support = gavi_intro(gavi_phase)))

}

################## DRAWS ###################################
decision_tree_ndraw<-function(ndraw, country, horizon, GAVI_status, DogVax_TF, VaxRegimen, DALYrabies, LE, RIG_status, discount, breaks, IBCM)
{
  draws<-vector("list", ndraw)
  for(i in 1:ndraw)
  {
    draws[[i]] <- cbind.data.frame(decision_tree_draw(country=country, horizon=horizon, GAVI_status=GAVI_status,
                                                      DogVax_TF=DogVax_TF, VaxRegimen=VaxRegimen,
                                                      DALYrabies=DALYrabies,
                                                      LE=LE, RIG_status=RIG_status, discount=discount, breaks=breaks,
                                                      IBCM=IBCM), iter=i)

  }

  draws<-do.call(rbind, draws)
  return(draws)
}
