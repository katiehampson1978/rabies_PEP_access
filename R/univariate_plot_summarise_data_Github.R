# Functions to summarise univariate data for plot

summarise_univar_data <- function(horizon_dataframe, lci, uci){

  horizon_dataframe$scenario <- as.character(horizon_dataframe$scenario)
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a1"] <- "Scenario 1 \n Status Quo"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a3_1"] <- "Scenario 2 \n Improved PEP access"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a5_1"] <- "Scenario 4b Dog vax \n Improved PEP access"
  
  #' * TOTAL_DEATHS *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE),
              lci=mean(total_deaths_lci, na.rm=TRUE),
              uci=mean(total_deaths_uci, na.rm=TRUE),
              metric="total_deaths")
  final_df = summed
  
  #' * TOTAL_VIALS *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_vials, na.rm=TRUE),
              lci=mean(total_vials_lci, na.rm=TRUE),
              uci=mean(total_vials_uci, na.rm=TRUE),
              metric="total_vials")
  final_df <- rbind(final_df, summed)
  
  #' * TOTAL_COST *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_cost, na.rm=TRUE),
              lci=mean(total_cost_lci, na.rm=TRUE),
              uci=mean(total_cost_uci, na.rm=TRUE),
              metric="total_cost")
  final_df <- rbind(final_df, summed)
  
  #' * TOTAL_DEATHS_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE),
              lci=mean(total_deaths_averted_lci, na.rm=TRUE),
              uci=mean(total_deaths_averted_uci, na.rm=TRUE),
              metric="total_deaths_averted")
  final_df <- rbind(final_df, summed)
  
  #' * U5_DEATHS_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE),
              lci=mean(U5_deaths_averted_lci, na.rm=TRUE),
              uci=mean(U5_deaths_averted_uci, na.rm=TRUE),
              metric="U5_deaths_averted")
  final_df <- rbind(final_df, summed)
  
  #' * TOTAL_YLL *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE),
              lci=mean(total_YLL_lci, na.rm=TRUE),
              uci=mean(total_YLL_uci, na.rm=TRUE),
              metric="total_YLL")
  final_df <- rbind(final_df, summed)
  
  #' * TOTAL_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE),
              lci=mean(total_YLL_averted_lci, na.rm=TRUE),
              uci=mean(total_YLL_averted_uci, na.rm=TRUE),
              metric="total_YLL_averted")
  final_df <- rbind(final_df, summed)
  
  #' * U5_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(U5_YLL_averted, na.rm=TRUE),
              lci=mean(U5_YLL_averted_lci, na.rm=TRUE),
              uci=mean(U5_YLL_averted_uci, na.rm=TRUE),
              metric="U5_YLL_averted")
  final_df <- rbind(final_df, summed)
  
  #' * VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE),
              lci=mean(vaccinated_lci, na.rm=TRUE),
              uci=mean(vaccinated_uci, na.rm=TRUE),
              metric="vaccinated")
  final_df <- rbind(final_df, summed)
  
  #' * FULLY_VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE),
              lci=mean(fully_vaccinated_lci, na.rm=TRUE),
              uci=mean(fully_vaccinated_uci, na.rm=TRUE),
              metric="fully_vaccinated")
  final_df <- rbind(final_df, summed)
  
  #' * COST_PER_DEATH_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE),
              lci=mean(cost_per_death_averted_lci, na.rm=TRUE),
              uci=mean(cost_per_death_averted_uci, na.rm=TRUE),
              metric="cost_per_death_averted")
  final_df <- rbind(final_df, summed)
  
  #' * COST_PER_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE),
              lci=mean(cost_per_YLL_averted_lci, na.rm=TRUE),
              uci=mean(cost_per_YLL_averted_uci, na.rm=TRUE),
              metric="cost_per_YLL_averted")
  final_df <- rbind(final_df, summed)
  
  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(scenario, variable) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE),
              lci=mean(deaths_averted_per_100k_vaccinated_lci, na.rm=TRUE),
              uci=mean(deaths_averted_per_100k_vaccinated_uci, na.rm=TRUE),
              metric="deaths_averted_per_100k_vaccinated")
  final_df <- rbind(final_df, summed)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)

}
