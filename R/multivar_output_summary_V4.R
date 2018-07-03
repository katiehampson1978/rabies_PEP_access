########################
# country_horizon_iter #
########################
# SUMMARIZE BY ITERATION FOR ENTIRE HORIZON - ONLY NECESSARY FOR COUNTRY!
country_horizon_iter <- function(model_output){
    sums = model_output %>%
    dplyr::group_by(country, scenario, iter) %>%
    dplyr::summarise(cluster = unique(cluster),
                     total_deaths = sum(total_deaths),
                     U5_deaths = sum(U5_deaths),
                     total_vials = sum(total_vials),
                     total_cost = sum(total_cost),
                     total_deaths_averted = sum(total_deaths_averted),
                     U5_deaths_averted = sum(U5_deaths_averted),
                     total_YLL = sum(total_YLL),
                     total_YLL_averted = sum(total_YLL_averted),
                     YLL_averted_U5 = sum(YLL_averted_U5),
                     vaccinated = sum(vaccinated),
                     fully_vaccinated = sum(fully_vaccinated),
                     RIG = sum(RIG),
                     gavi_2018=unique(gavi_2018))
  sums$cost_per_death_averted <-  sums$total_cost/sums$total_deaths_averted
  sums$cost_per_YLL_averted <-  sums$total_cost/sums$total_YLL_averted
  sums$deaths_averted_per_100k_vaccinated <-  sums$total_deaths_averted/sums$vaccinated/100000
  return(sums)
}
# test = country_horizon_iter(out)

############################
# multivar_country_summary #
############################
# FOR COUNTRIES summarize over entire time horizon or by year
multivar_country_summary = function(model_output, year){
  if(year==TRUE){arg_setting = list(quo(year), quo(country), quo(scenario))}
  if(year==FALSE){arg_setting = list(quo(country), quo(scenario))}

    sums <- model_output %>%
    dplyr::group_by(!!! arg_setting) %>% # Does not include model_output!
    dplyr::summarise(
      cluster = unique(cluster),
      total_deaths_lci = quantile(total_deaths, 0.025),
      total_deaths_uci = quantile(total_deaths, 0.975),
      total_deaths = quantile(total_deaths, 0.5),

      U5_deaths_lci = quantile(U5_deaths, 0.025),
      U5_deaths_uci = quantile(U5_deaths, 0.975),
      U5_deaths = quantile(U5_deaths, 0.5),

      total_vials_lci = quantile(total_vials, 0.025),
      total_vials_uci = quantile(total_vials, 0.975),
      total_vials = quantile(total_vials, 0.5),

      total_cost_lci = quantile(total_cost, 0.025),
      total_cost_uci = quantile(total_cost, 0.975),
      total_cost = quantile(total_cost, 0.5),

      total_deaths_averted_lci = quantile(total_deaths_averted, 0.025),
      total_deaths_averted_uci = quantile(total_deaths_averted, 0.975),
      total_deaths_averted = quantile(total_deaths_averted, 0.5),

      U5_deaths_averted_lci = quantile(U5_deaths_averted, 0.025),
      U5_deaths_averted_uci = quantile(U5_deaths_averted, 0.975),
      U5_deaths_averted = quantile(U5_deaths_averted, 0.5),

      total_YLL_lci = quantile(total_YLL, 0.025),
      total_YLL_uci = quantile(total_YLL, 0.975),
      total_YLL = quantile(total_YLL, 0.5),

      total_YLL_averted_lci = quantile(total_YLL_averted, 0.025),
      total_YLL_averted_uci = quantile(total_YLL_averted, 0.975),
      total_YLL_averted = quantile(total_YLL_averted, 0.5),

      U5_YLL_averted_lci = quantile(YLL_averted_U5, 0.025),
      U5_YLL_averted_uci = quantile(YLL_averted_U5, 0.975),
      U5_YLL_averted = quantile(YLL_averted_U5, 0.5),

      vaccinated_lci = quantile(vaccinated, 0.025),
      vaccinated_uci = quantile(vaccinated, 0.975),
      vaccinated = quantile(vaccinated, 0.5),

      fully_vaccinated_lci = quantile(fully_vaccinated, 0.025),
      fully_vaccinated_uci = quantile(fully_vaccinated, 0.975),
      fully_vaccinated = quantile(fully_vaccinated, 0.5),

      RIG_lci = quantile(RIG, 0.025),
      RIG_uci = quantile(RIG, 0.975),
      RIG = quantile(RIG, 0.5),

      # cost_per_death_averted_lci = quantile(cost_per_death_averted, 0.025),
      # cost_per_death_averted_uci = quantile(cost_per_death_averted, 0.975),
      # cost_per_death_averted = mean(cost_per_death_averted),
      #
      # cost_per_YLL_averted_lci = quantile(cost_per_YLL_averted, 0.025),
      # cost_per_YLL_averted_uci = quantile(cost_per_YLL_averted, 0.975),
      # cost_per_YLL_averted = mean(cost_per_YLL_averted),
      #
      # deaths_averted_per_100k_vaccinated_lci = quantile(deaths_averted_per_100k_vaccinated, 0.025),
      # deaths_averted_per_100k_vaccinated_uci = quantile(deaths_averted_per_100k_vaccinated, 0.975),
      # deaths_averted_per_100k_vaccinated = mean(deaths_averted_per_100k_vaccinated),

      gavi_2018=unique(gavi_2018),
      gavi_support=ifelse(length(arg_setting)==3, unique(Gavi_support), NA)
      )

    sums$cost_per_death_averted_lci <-  sums$total_cost_lci/sums$total_deaths_averted_uci
    sums$cost_per_death_averted_uci <-  sums$total_cost_uci/sums$total_deaths_averted_lci
    sums$cost_per_death_averted <-  sums$total_cost/sums$total_deaths_averted

    sums$cost_per_YLL_averted_lci <-  sums$total_cost_lci/sums$total_YLL_averted_uci
    sums$cost_per_YLL_averted_uci <-  sums$total_cost_uci/sums$total_YLL_averted_lci
    sums$cost_per_YLL_averted <-  sums$total_cost/sums$total_YLL_averted

    sums$deaths_averted_per_100k_vaccinated_lci <-  sums$total_deaths_averted_lci/sums$vaccinated_uci/100000
    sums$deaths_averted_per_100k_vaccinated_uci <-  sums$total_deaths_averted_uci/sums$vaccinated_lci/100000
    sums$deaths_averted_per_100k_vaccinated <-  sums$total_deaths_averted/sums$vaccinated/100000

  return(sums)
}
#
# test2 = multivar_country_summary(test, year = FALSE)
# test3 = multivar_country_summary(out, year = TRUE)

####################
# multivar_summary #
####################
multivar_summary = function(model_output, year, setting){
  # summary over entire time horizon or by year - by cluster or globally
  if(year==TRUE & setting == "cluster"){arg_setting = list(quo(year), quo(cluster), quo(scenario))}
  if(year==FALSE & setting == "cluster"){arg_setting = list(quo(cluster), quo(scenario))}
  if(year==TRUE & setting == "global"){arg_setting = list(quo(year), quo(scenario))}
  if(year==FALSE & setting == "global"){arg_setting = list(quo(scenario))}

  sums <- model_output %>%
    dplyr::group_by(!!! arg_setting) %>%
    dplyr::summarise(
      total_deaths_lci = sum(total_deaths_lci),
      total_deaths_uci = sum(total_deaths_uci),
      total_deaths = sum(total_deaths),

      U5_deaths_lci = sum(U5_deaths_lci),
      U5_deaths_uci = sum(U5_deaths_uci),
      U5_deaths = sum(U5_deaths),

      total_vials_lci = sum(total_vials_lci),
      total_vials_uci = sum(total_vials_uci),
      total_vials = sum(total_vials),

      total_cost_lci = sum(total_cost_lci),
      total_cost_uci = sum(total_cost_uci),
      total_cost = sum(total_cost),

      total_deaths_averted_lci = sum(total_deaths_averted_lci),
      total_deaths_averted_uci = sum(total_deaths_averted_uci),
      total_deaths_averted = sum(total_deaths_averted),

      U5_deaths_averted_lci = sum(U5_deaths_averted_lci),
      U5_deaths_averted_uci = sum(U5_deaths_averted_uci),
      U5_deaths_averted = sum(U5_deaths_averted),

      total_YLL_lci = sum(total_YLL_lci),
      total_YLL_uci = sum(total_YLL_uci),
      total_YLL = sum(total_YLL),

      total_YLL_averted_lci = sum(total_YLL_averted_lci),
      total_YLL_averted_uci = sum(total_YLL_averted_uci),
      total_YLL_averted = sum(total_YLL_averted),

      U5_YLL_averted_lci = sum(U5_YLL_averted_lci),
      U5_YLL_averted_uci = sum(U5_YLL_averted_uci),
      U5_YLL_averted = sum(U5_YLL_averted),

      vaccinated_lci = sum(vaccinated_lci),
      vaccinated_uci = sum(vaccinated_uci),
      vaccinated = sum(vaccinated),

      fully_vaccinated_lci = sum(fully_vaccinated_lci),
      fully_vaccinated_uci = sum(fully_vaccinated_uci),
      fully_vaccinated = sum(fully_vaccinated),

      RIG_lci = sum(RIG_lci),
      RIG_uci = sum(RIG_uci),
      RIG = sum(RIG),

      # cost_per_death_averted_lci = quantile(cost_per_death_averted_lci, 0.025),
      # cost_per_death_averted_uci = quantile(cost_per_death_averted_uci, 0.975),
      # cost_per_death_averted = quantile(cost_per_death_averted, 0.5),
      #
      # cost_per_YLL_averted_lci = quantile(cost_per_YLL_averted_lci, 0.025)
      # cost_per_YLL_averted_uci = quantile(cost_per_YLL_averted_uci, 0.975)
      # cost_per_YLL_averted = quantile(cost_per_YLL_averted, 0.5)
      #
      # deaths_averted_per_100k_vaccinated_lci = quantile(deaths_averted_per_100k_vaccinated_lci, 0.025)
      # deaths_averted_per_100k_vaccinated_uci = quantile(deaths_averted_per_100k_vaccinated_uci, 0.975)
      # deaths_averted_per_100k_vaccinated = quantile(deaths_averted_per_100k_vaccinated, 0.5)

      cost_per_death_averted_lci = total_cost_lci/total_deaths_averted_uci,
      cost_per_death_averted_uci = total_cost_uci/total_deaths_averted_lci,
      cost_per_death_averted = total_cost/total_deaths_averted,

      cost_per_YLL_averted_lci = total_cost_lci/total_YLL_averted_uci,
      cost_per_YLL_averted_uci = total_cost_uci/total_YLL_averted_lci,
      cost_per_YLL_averted = total_cost/total_YLL_averted,

      deaths_averted_per_100k_vaccinated_lci = total_deaths_averted_lci/vaccinated_uci/100000,
      deaths_averted_per_100k_vaccinated_uci = total_deaths_averted_uci/vaccinated_lci/100000,
      deaths_averted_per_100k_vaccinated = total_deaths_averted/vaccinated/100000)

  # sums$cost_per_death_averted_lci = sums$total_cost_lci/sums$total_deaths_averted_uci
  # sums$cost_per_death_averted_uci = sums$total_cost_uci/sums$total_deaths_averted_lci
  # sums$cost_per_death_averted = sums$total_cost/sums$total_deaths_averted
  #
  # sums$cost_per_YLL_averted_lci = sums$total_cost_lci/sums$total_YLL_averted_uci
  # sums$cost_per_YLL_averted_uci = sums$total_cost_uci/sums$total_YLL_averted_lci
  # sums$cost_per_YLL_averted = sums$total_cost/sums$total_YLL_averted
  #
  # sums$deaths_averted_per_100k_vaccinated_lci = sums$total_deaths_averted_lci/sums$vaccinated_uci/100000
  # sums$deaths_averted_per_100k_vaccinated_uci = sums$total_deaths_averted_uci/sums$vaccinated_lci/100000
  # sums$deaths_averted_per_100k_vaccinated = sums$total_deaths_averted/sums$vaccinated/100000

  return(sums)
}

# test4 = multivar_summary(test2, year=FALSE, setting ="cluster")
# test5 = multivar_summary(test2, year=FALSE, setting ="global")
