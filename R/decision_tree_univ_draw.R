# Edited on 22/08/17 by Laura C - new decision tree structure (suspect v non-suspect bites), ID as proportion, NTV for Ethiopia
# Edited on 22/01/18 by Katie H - incorporate all changes in decision tree R code

# vary="p_rabid"
# country="Tanzania"
# horizon=length(2020:2035)
# GAVI_status="none"
# DogVax_TF=T
# VaxRegimen="Updated TRC"
# DALYrabies=DALYrabies_input
# LE=LE2020
# RIG_status="none"
# discount=0.03
# breaks="5yr"
# IBCM=FALSE

decision_tree_univ_draw<-function(vary,
                                  country, horizon,
                                  GAVI_status, DogVax_TF,
                                  VaxRegimen, DALYrabies, LE, RIG_status, discount, breaks, IBCM){

  # Vary indicates which variable will NOT be held constant
  # VARY: suspect_bite_incidence, non_suspect_bite_incidence, p_rabid, p_transmission
  # DO NOT VARY GAVI SCENARIOS: p_seek, p_receive, p_complete, p_prevent, prop_RIG_urban
  #          later incorporate variability in: rabid_dog_bites

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
  } # print(LE)

  # 2. GAVI support
  prop_ID <- data$ID[index]

  if(GAVI_status!="none") {
    if(GBP=="I"){gavi_phase = gavi_phaseI}
    if(GBP=="II"){gavi_phase = gavi_phaseII}
    if(GBP=="III"){gavi_phase = gavi_phaseIII}
    if(GBP=="0"){gavi_phase = gavi_phaseI} # Gavi-eligible countries in GBP0 (americas) move to phase I for Gavi
  } else { gavi_phase = gavi_phase0 }# STATUS QUO re: type of PEP used
  NTV_TF = (country == "Ethiopia") # With no GAVI support, Ethiopia uses NTV
  # print(NTV_TF)

  # 3. Bite incidence - from suspect rabid dogs
  # create human pop table for the horizon of interest: 2020 - 2035
  pop2020_col <- which(colnames(data)=="pop2020") # lets take 2018 and 2019 as endemic
  pop_cols <- c(pop2020_col:(pop2020_col+(horizon-1))) # MINUS 1 because you are adding horizon i.e. creates an extra yrs!
  pop <- data[index, pop_cols]

  # create dog table for the horizon of interest
  # dogs <- dplyr::filter(dogs, country %in% as.character(data$country)) # make sure data and dogs have same countries
  dogs <- dogs[which(dogs$country == country),] # make sure data and dogs have same countries
  dog2020_col <- which(colnames(dogs)=="dogpop2020")
  dogpop_cols <- c(dog2020_col:(dog2020_col + (horizon-1)))
  dogpop <- dogs[,dogpop_cols]

  # mean bites per rabid dog
  rabid_dog_bites = 0.38 #             could introduce this as a variable to also vary....

    if(DogVax_TF==T) { # IF DOG VACCINATION PROGRAMME:

    # create elimination trajectory for the time horizon (-1 because first year will be endemic susp_bite_inc)
    Y1 = grep("2020", names(elimination_traj))
    yr_index = Y1:(Y1+horizon-1)
    trajectories = elimination_traj[which(elimination_traj$phase==GBP), yr_index]

    #         
    if(vary=="suspect_bite_incidence"){ # select a random number from 0-100 to select an incidence trajectory, i.e. a column in elimination_traj
      elim_index <- round(runif(1, 1, 100))
      ET <- unlist(trajectories[elim_index,])
    } else { ET <- unlist(apply(trajectories, 2, mean))}

    # translate into a COUNTRY-SPECIFIC rabid dog bite incidence i.e. bites/ population
    susp_bite_inc_traj = as.numeric((dogpop * rabid_dog_bites * ET)/ pop)
    NAs = which(is.na(susp_bite_inc_traj)) ##  Fill in bite incidence for endemic years

    #         
    if(vary=="suspect_bite_incidence"){ # fill in variable or mean endemic bite incidence!
      if(length(NAs)>0){susp_bite_inc_traj[NAs] <- (dogpop[1:length(NAs)] * rabid_dog_bites * rnorm(length(NAs), mean = rabies$inc, sd = rabies$sd))/pop[1:length(NAs)]}
    } else {
      if(length(NAs)>0){ susp_bite_inc_traj[NAs] <- rep(data$bite_inc_susp[index], length(NAs)) }
    }
    susp_bite_inc <- unlist(susp_bite_inc_traj)

    # OR.............
  } else { # IF NO DOG VACCINATION PROGRAMME:
    #         
    if(vary=="suspect_bite_incidence"){ # fill in variable or mean endemic bite incidence!
      susp_bite_inc <-  (dogpop * rabid_dog_bites * rnorm(horizon, mean = rabies$inc, sd = rabies$sd))/pop
    } else { susp_bite_inc <- rep(data$bite_inc_susp[index], horizon)}
  }
  susp_bite_inc <- unlist(susp_bite_inc)

    # 4. Bite incidence - from non-suspect dogs
    mean <- data$bite_inc_non_susp[index]
    #         
    if(vary=="non_suspect_bite_incidence"){ # fill in variable or mean healthy dog bite incidence!
      non_susp_bite_inc <- rep(rtriangle(n=1, a=mean-0.2*mean, b=mean+0.2*mean), horizon) # assume mean +/- 20% uncertainty
    } else {non_susp_bite_inc <- rep(mean, horizon)}
    # print(non_susp_bite_inc)

    # 5. Probability suspected dog bite is rabid
    #         
    if(vary=="p_rabid"){ #### Varying p_rabid
      prop_bites_rabid <- rbinom(n=1, size=data$p_rabid_n[index], prob=data$p_rabid[index])/data$p_rabid_n[index]
    }else{ prop_bites_rabid <- data$p_rabid[index] } #### Holding p_rabid constant
    # print(prop_bites_rabid)

    # 6. Probability of developing rabies if bitten by rabid dog - Differences due to age and bite site ignored - separate analysis justifies this
    #         
    if(vary=="p_transmission"){
      prob_transmission <- mixture_model(1) #### Varying p_transmission
    }else{ #### Holding p_transmission constant
      prob_transmission <- (transmission$prob_head * transmission$prob_death_given_head) +
        (transmission$prob_arm * transmission$prob_death_given_arm) +
        (transmission$prob_trunk * transmission$prob_death_given_trunk) +
        (transmission$prob_leg * transmission$prob_death_given_leg)
      }
    # print(prob_transmission)

    # Gavi scenarios:
    if(GAVI_status=="base") {
      # 7. Health seeking
      prob_seek_pep_susp <- gavi_phasing(gavi_phase, p_SQ = data$p_seek[index], p_step$base, p_increment, p_seek_cap$base)
      prob_seek_pep_non <- gavi_phasing(phase = gavi_phase, p_SQ = data$pseek_norm_SQ[index], step = p_step$base, increment = 0, cap = data$pseek_norm_SQ[index]+p_step$base)
      # print(prob_seek_pep_susp); print(prob_seek_pep_non)

      # 8. Access to PEP
      prob_receive_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_receive[index], step = p_step$base, increment = p_increment, cap = p_receive_cap$base) #### With GAVI support, PEP is always available

      # 9. Rate of PEP completion
      prob_complete_pep <- gavi_phasing(phase = gavi_phase, p_SQ = data$p_complete[index], step = p_step$base, increment = p_increment, cap = p_complete_cap$base)

    }else if(GAVI_status=="low") { warning("Univariate analysis only for base GAVI support.")
    } else if(GAVI_status=="high"){ warning("Univariate analysis only for base GAVI support.")
      }else{
        prob_seek_pep_susp <- rbinom(n=horizon, size=data$p_seek_n[index], prob=data$p_seek[index])/data$p_seek_n[index]
        prob_seek_pep_non <- data$pseek_norm_SQ[index]
        prob_receive_pep <- rbinom(n=1, size=data$p_receive_n[index], prob=data$p_receive[index])/data$p_receive_n[index]
        prob_complete_pep <- rbinom(n=1, size=data$p_complete_n[index], prob=data$p_complete[index])/data$p_complete_n[index]
      }

    # 10. Access to RIG
    prop_RIG_urban = 0.15
    RIG_risk = data$prop_urban[index] * prop_RIG_urban # transmission$prob_head+transmission$prob_arm
    if(RIG_status=="none"){ prob_RIG <-0 # NONE
    }else{prob_RIG <- RIG_risk}

    # 11. Probability that PEP prevents rabies
    # Depends on completion and RIG, and may differ for IM vs ID - see PEP simulations & Tz data analysis
    if(vary=="p_prevent"){
      bin_confint_no_rigrisk <- Hmisc::binconf(params$p_prevent_given_complete_n, n=params$p_prevent_given_complete_n)
      bin_confint_rigrisk <- 1 # Hmisc::binconf(params$p_prevent_given_complete_n*10, n=params$p_prevent_given_complete_n*10) # reduce chance of death x 10!

      prob_prevent_rabies_given_complete_pep <- rtriangle(n=1, a=bin_confint_no_rigrisk[2], b=bin_confint_no_rigrisk[3])

      prob_prevent_rabies_given_complete_pep_RIG <- ((1-RIG_risk) * prob_prevent_rabies_given_complete_pep) + (RIG_risk * bin_confint_rigrisk) # No marginal survival benefit of RIG

      prob_prevent_rabies_given_imperfect_pep <- rbinom(n=1, size=params$p_prevent_given_imperfect_n, # Current cut-off is 1 visit
                                                        prob=params$p_prevent_given_imperfect)/params$p_prevent_given_imperfect_n

      if(NTV_TF){ # IF NTVs used - need to work out timeseries of p_prevent (w/o RIG, complete/incomplete)
        prob_prevent_rabies_given_complete_pep_NTV <- rbinom(n=1, size=params$p_prevent_given_complete_NTV_n,
                                                             prob=params$p_prevent_given_complete_NTV)/params$p_prevent_given_complete_NTV_n
        prob_prevent_rabies_given_imperfect_pep_NTV <- rbinom(n=1, size=params$p_prevent_given_insufficient_NTV_n, # Current cut-off is 1 visit
                                                              prob=params$p_prevent_given_insufficient_NTV)/params$p_prevent_given_insufficient_NTV_n

        prob_prevent_NTV_complete <- rep(prob_prevent_rabies_given_complete_pep_NTV, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support
        prob_prevent_NTV_complete_RIG <- rep(prob_prevent_rabies_given_complete_pep_NTV, length(gavi_intro(gavi_phase)))
        prob_prevent_NTV_imperfect <- rep(prob_prevent_rabies_given_imperfect_pep_NTV, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support

        prob_prevent_NTV_complete[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep
        prob_prevent_NTV_complete_RIG[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep_RIG
        prob_prevent_NTV_imperfect[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_imperfect_pep
      }
      # print(c(prob_prevent_NTV_gavi_complete, prob_prevent_NTV_gavi_complete_RIG, prob_prevent_NTV_gavi_imperfect))

    } else {
      bin_confint_no_rigrisk <- Hmisc::binconf(params$p_prevent_given_complete_n, n=params$p_prevent_given_complete_n)
      bin_confint_rigrisk <- 1 # Hmisc::binconf(params$p_prevent_given_complete_n*10, n=params$p_prevent_given_complete_n*10) # reduce chance of death x 10!

      prob_prevent_rabies_given_complete_pep <- mean(rtriangle(n=1000000, a=bin_confint_no_rigrisk[2], b=bin_confint_no_rigrisk[3]))
      prob_prevent_rabies_given_complete_pep_RIG <- ((1-RIG_risk) * prob_prevent_rabies_given_complete_pep) + (RIG_risk * bin_confint_rigrisk)
      prob_prevent_rabies_given_imperfect_pep <- params$p_prevent_given_imperfect

      prob_prevent_NTV_complete_RIG <- rep(params$p_prevent_given_complete_NTV, horizon) # No difference between protection w & w/o RIG for NTVs!
      prob_prevent_NTV_complete <- rep(params$p_prevent_given_complete_NTV, horizon)
      prob_prevent_NTV_imperfect <- rep(params$p_prevent_given_insufficient_NTV, horizon)
      prob_prevent_NTV_complete_RIG[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep_RIG
      prob_prevent_NTV_complete[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep
      prob_prevent_NTV_imperfect[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_imperfect_pep
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
    # print(pep)


    # 13. Population/ demographic data for country
    population_columns <- data[index,grep("pop20", colnames(data))]
    offst <- which(colnames(population_columns)=="pop2020")
    population <- population_columns[1:horizon+(offst-1)]


    ################################
    # Calculate decision tree:
    ################################
    # Measures in the decision tree
    susp_bites = susp_bite_inc * population
    # print(susp_bite_inc)
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
    if(RIG_risk == 0){
      if(NTV_TF){
        prob_prevent_rabies_given_complete_pep <-  prob_prevent_NTV_complete ##           
        prob_prevent_rabies_given_imperfect_pep <- prob_prevent_NTV_imperfect ##          
      }

      human_rabies_deaths = susp_bites * prop_bites_rabid * prob_transmission * #### Top half of tree - those infected
        (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * (1-prob_prevent_rabies_given_complete_pep) + #### Those who complete PEP
           prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep)  * (1-prob_prevent_rabies_given_imperfect_pep) + #### Those who do not complete PEP
           prob_seek_pep_susp * (1-prob_receive_pep) + #### Those who seek PEP but don't receive it
           (1-prob_seek_pep_susp)) #### Those who don't seek PEP

      human_rabies_deaths_averted = susp_bites * prop_bites_rabid * prob_transmission *
        (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * prob_prevent_rabies_given_complete_pep +
           prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep) * prob_prevent_rabies_given_imperfect_pep)

    } else {
      if(NTV_TF){
        prob_prevent_rabies_given_complete_pep <-  prob_prevent_NTV_complete ##           
        prob_prevent_rabies_given_imperfect_pep <- prob_prevent_NTV_imperfect ##          

      } else {
        prob_prevent_rabies_given_complete_pep <- rep(prob_prevent_rabies_given_complete_pep, length(gavi_intro(gavi_phase))) # create p_prevent variable adjusted for RIG support
        prob_prevent_rabies_given_complete_pep[which(gavi_intro(gavi_phase)=="support")] <- prob_prevent_rabies_given_complete_pep_RIG
      }

      human_rabies_deaths = susp_bites * prop_bites_rabid * prob_transmission * #### Top half of tree - those infected
        (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * (1-prob_prevent_rabies_given_complete_pep) + #### Those who complete PEP
           prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep)  * (1-prob_prevent_rabies_given_imperfect_pep) + #### Those who do not complete PEP
           prob_seek_pep_susp * (1-prob_receive_pep) + #### Those who seek PEP but don't receive it
           (1-prob_seek_pep_susp)) #### Those who don't seek PEP

      human_rabies_deaths_averted = susp_bites * prop_bites_rabid * prob_transmission *
        (prob_seek_pep_susp * prob_receive_pep * prob_complete_pep * prob_prevent_rabies_given_complete_pep +
           prob_seek_pep_susp * prob_receive_pep * (1-prob_complete_pep) * prob_prevent_rabies_given_imperfect_pep)
    }
    U5_prop_deaths = sum(DALYrabies$death_pc[1:2]/sum(DALYrabies$death_pc))
    # print(human_rabies_deaths)


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
    cost_RIG = gavi_RIG_price #  data$price_per_vial_RIG[index]
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

    # TargetPopulation_rabid <- as.numeric(unlist(exposure_inc * population * p_seek_rabid))
    TargetPopulation_rabid <- as.numeric(unlist(exposure_inc * t(population)))

    # target2: persons bitten by healthy animals
    healthy_exposure_inc <- non_susp_bite_inc
    TargetPopulation_healthy <- as.numeric(unlist(healthy_exposure_inc * population))

    # RETURN RESULTS
    return(cbind.data.frame(year=years[1:horizon],
                            human_rabies_deaths = as.numeric(human_rabies_deaths),
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
                            gavi_support = gavi_intro(gavi_phase)
                            #, pep_vials = pep$vials_complete # Check on vials per patient!
    )
    )

  }

################################################################################
################################################################################

# TESTING
# decision_tree_univ_draw(vary="suspect_bite_incidence", country="Bangladesh", horizon=length(2020:2035),
                        # GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
                        # DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none",
                        # discount=0.03, breaks="5yr", IBCM=FALSE)

# decision_tree_univ_draw(vary="p_rabid", country="Ethiopia", horizon=length(2020:2035),
#                         GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
#                         DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="high risk",
#                         discount=0.03, breaks="5yr", IBCM=FALSE)
#
################################################################################
################################################################################

decision_tree_univ_ndraw<-function(ndraw, vary, country, horizon,
                                   GAVI_status, DogVax_TF, VaxRegimen,
                                   DALYrabies, LE, RIG_status,
                                   discount, breaks, IBCM){
  draws<-vector("list",ndraw)
  for(i in 1:ndraw){
    draws[[i]]<-cbind.data.frame(decision_tree_univ_draw(vary=vary, country=country, horizon=horizon,
                                                         GAVI_status=GAVI_status, DogVax_TF=DogVax_TF, VaxRegimen=VaxRegimen,
                                                         DALYrabies=DALYrabies, LE=LE, RIG_status=RIG_status,
                                                         discount=discount, breaks=breaks, IBCM = IBCM),
                                 iter=i)
    }
  draws<-do.call("rbind",draws)
  return(draws)
}

################################################################################
# ################################################################################
# # # # # # # TESTING
# decision_tree_univ_ndraw(ndraw = 10,
#                     vary = "p_rabid",
#                     country = "Bangladesh",
#                     horizon = 16,
#                     GAVI_status = "none",
#                     DogVax_TF = F,
#                     VaxRegimen = "Updated TRC",
#                     DALYrabies = DALYrabies_input,
#                     LE = LE2020,
#                     RIG_status = "none",
#                     discount = 0.03,
#                     breaks = "5y",
#                     IBCM = TRUE)

# decision_tree_univ_ndraw(ndraw = 10,
#                          vary = "test",
#                          country = "Ethiopia",
#                          horizon = 16,
#                          GAVI_status = "none",
#                          DogVax_TF = F,
#                          VaxRegimen = "Updated TRC",
#                          DALYrabies = DALYrabies_input,
#                          LE = LE2020,
#                          RIG_status = "none",
#                          discount = 0.03,
#                          breaks = "5y",
#                          IBCM = TRUE)
# # #
################################################################################
################################################################################

#
# decision_tree_univ_ndraw(ndraw=10, vary="bite_incidence", country="Tanzania",
#                          horizon=10, GAVI_status="none", DogVax_TF=F,
#                          VaxRegimen=VaxRegimen, DALYrabies=DALYrabies, LE=LE,
#                          RIG_status=RIG_status, discount=discount, breaks=breaks)



