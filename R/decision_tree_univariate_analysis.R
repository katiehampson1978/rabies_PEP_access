# Univariate analysis
# VARY: suspect_bite_incidence, non_suspect_bite_incidence, p_rabid, p_transmission, p_prevent

univariate_analysis <- function(ndraw, horizon, GAVI_status, DogVax_TF, VaxRegimen,
                                DALYrabies, LE, RIG_status, discount, breaks, IBCM){

  variables<-c("suspect_bite_incidence","non_suspect_bite_incidence","p_rabid","p_transmission","p_prevent")

  results<-vector("list", length(variables)*nrow(data))

  for(j in 1:length(variables)){
    for(i in 1:nrow(data)){
      set.seed(5) # set the seed before running all the draws for this country - same random numbers will be drawn for each country
      index<-nrow(data)*(j-1)+i
      results[[index]]<-cbind.data.frame(decision_tree_univ_ndraw(ndraw=ndraw, vary=variables[j], country=data$country[i],
                                                                  horizon=horizon, GAVI_status=GAVI_status, DogVax_TF=DogVax_TF,
                                                                  VaxRegimen=VaxRegimen, DALYrabies=DALYrabies, LE=LE,
                                                                  RIG_status=RIG_status, discount=discount, breaks=breaks, IBCM = IBCM),
                                         country=data$country[i], vary=variables[j])
      # Print % completion to console
      print(paste(round(100/nrow(data)*i, digits=1), "%", sep=" "))
    }
    # Print variable
    print(variables[j])
  }

  out<-do.call(rbind.data.frame, results)

  # Label data.frame names properly
  colnames(out) <- c("year","total_deaths","total_vials","total_cost","total_deaths_averted","U5_deaths_averted",
                     "total_YLL","total_YLL_averted","YLL_averted_U5","vaccinated","fully_vaccinated",
                     "p_seek_rabid", "p_seek_healthy", "p_receive", "p_receive_IBCM", "p_receive_RIG", "p_complete",
                     "TargetPopulation_rabid","TargetPopulation_healthy","RIG","Gavi_support",
                     "iter","country","variable")
  return(out)
}


############ DETERMINISTIC CHECK!
univariate_analysis_check <- function(ndraw, horizon, GAVI_status, DogVax_TF, VaxRegimen,
                                DALYrabies, LE, RIG_status, discount, breaks, IBCM){

  variables<-c("test")

  results<-vector("list", length(variables)*nrow(data))

  for(j in 1:length(variables)){
    for(i in 1:nrow(data)){
      set.seed(5) # set the seed before running all the draws for this country - same random numbers will be drawn for each country
      index<-nrow(data)*(j-1)+i
      results[[index]]<-cbind.data.frame(decision_tree_univ_ndraw(ndraw=ndraw, vary=variables[j], country=data$country[i],
                                                                  horizon=horizon, GAVI_status=GAVI_status, DogVax_TF=DogVax_TF,
                                                                  VaxRegimen=VaxRegimen, DALYrabies=DALYrabies, LE=LE,
                                                                  RIG_status=RIG_status, discount=discount, breaks=breaks, IBCM = IBCM),
                                         country=data$country[i], vary=variables[j])
      # Print % completion to console
      print(paste(round(100/nrow(data)*i, digits=1), "%", sep=" "))
    }
    # Print variable
    print(variables[j])
  }

  out<-do.call(rbind.data.frame, results)

  # Label data.frame names properly
  colnames(out) <- c("year","total_deaths","total_vials","total_cost","total_deaths_averted","U5_deaths_averted",
                     "total_YLL","total_YLL_averted","YLL_averted_U5","vaccinated","fully_vaccinated",
                     "p_seek_rabid", "p_seek_healthy", "p_receive", "p_receive_IBCM", "p_receive_RIG", "p_complete",
                     "TargetPopulation_rabid","TargetPopulation_healthy","RIG","Gavi_support",
                     "iter","country","variable")
  return(out)
}

################################################################################
################################################################################

# TESTING
# univariate_analysis(ndraw=5, horizon=length(2020:2035), GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
#                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=0, breaks="5yr", IBCM=FALSE)
# n=5
# scenario_a3_1 <- univariate_analysis_check(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                      DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# scenario_a4 <- univariate_analysis_check(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                            DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="high risk", discount=discount, breaks="5yr", IBCM=FALSE)
#
# sum(scenario_a4$total_deaths)/n  -  sum(scenario_a3_1$total_deaths)/n
# sum(scenario_a3_1$total_deaths_averted)/n - sum(scenario_a4$total_deaths_averted)/n
# sum(scenario_a3_1$total_cost)/n - sum(scenario_a4$total_cost)/n


################################################################################
################################################################################


