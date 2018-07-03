############.###################
# summarise_multivariate_data #
###############################

summarise_multivariate_data <- function(horizon_dataframe, setting, lci, uci){

  horizon_dataframe$scenario <- as.character(horizon_dataframe$scenario)
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a1"] <- "1"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a2"] <- "4a"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a3_1"] <- "2"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a4"] <- "3"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a5_1"] <- "4b"
  horizon_dataframe$scenario[horizon_dataframe$scenario=="a5_2"] <- "4c"

  # Take only required scenarios (above)
  req_scenarios <- c("1", "2", "3", "4a", "4b", "4c")
  horizon_dataframe <- horizon_dataframe[which(horizon_dataframe$scenario %in% req_scenarios),]
  sort(unique(horizon_dataframe$scenario))

  # Set up arguments for dplyr summaries
  if(setting == "country"){arg_setting = list(quo(country), quo(scenario))}
  if(setting == "cluster"){arg_setting = list(quo(cluster), quo(scenario))}
  if(setting == "global"){arg_setting = list(quo(scenario))}

 #' * TOTAL_DEATHS *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE),
              lci=mean(total_deaths_lci, na.rm=TRUE),
              uci=mean(total_deaths_uci, na.rm=TRUE),
              metric="total_deaths")
  final_df = summed

  #' * TOTAL_VIALS *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_vials, na.rm=TRUE),
              lci=mean(total_vials_lci, na.rm=TRUE),
              uci=mean(total_vials_uci, na.rm=TRUE),
              metric="total_vials")
  final_df <- rbind(final_df, summed)

  #' * TOTAL_COST *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_cost, na.rm=TRUE),
              lci=mean(total_cost_lci, na.rm=TRUE),
              uci=mean(total_cost_uci, na.rm=TRUE),
              metric="total_cost")
  final_df <- rbind(final_df, summed)

  #' * TOTAL_DEATHS_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE),
              lci=mean(total_deaths_averted_lci, na.rm=TRUE),
              uci=mean(total_deaths_averted_uci, na.rm=TRUE),
              metric="total_deaths_averted")
  final_df <- rbind(final_df, summed)

  #' * U5_DEATHS_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE),
              lci=mean(U5_deaths_averted_lci, na.rm=TRUE),
              uci=mean(U5_deaths_averted_uci, na.rm=TRUE),
              metric="U5_deaths_averted")
  final_df <- rbind(final_df, summed)

  #' * TOTAL_YLL *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE),
              lci=mean(total_YLL_lci, na.rm=TRUE),
              uci=mean(total_YLL_uci, na.rm=TRUE),
              metric="total_YLL")
  final_df <- rbind(final_df, summed)

  #' * TOTAL_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE),
              lci=mean(total_YLL_averted_lci, na.rm=TRUE),
              uci=mean(total_YLL_averted_uci, na.rm=TRUE),
              metric="total_YLL_averted")
  final_df <- rbind(final_df, summed)

  #' * U5_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(U5_YLL_averted, na.rm=TRUE),
              lci=mean(U5_YLL_averted_lci, na.rm=TRUE),
              uci=mean(U5_YLL_averted_uci, na.rm=TRUE),
              metric="U5_YLL_averted")
  final_df <- rbind(final_df, summed)

  #' * VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE),
              lci=mean(vaccinated_lci, na.rm=TRUE),
              uci=mean(vaccinated_uci, na.rm=TRUE),
              metric="vaccinated")
  final_df <- rbind(final_df, summed)

  #' * FULLY_VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE),
              lci=mean(fully_vaccinated_lci, na.rm=TRUE),
              uci=mean(fully_vaccinated_uci, na.rm=TRUE),
              metric="fully_vaccinated")
  final_df <- rbind(final_df, summed)

  #' * COST_PER_DEATH_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE),
              lci=mean(cost_per_death_averted_lci, na.rm=TRUE),
              uci=mean(cost_per_death_averted_uci, na.rm=TRUE),
              metric="cost_per_death_averted")
  final_df <- rbind(final_df, summed)

  #' * COST_PER_YLL_AVERTED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE),
              lci=mean(cost_per_YLL_averted_lci, na.rm=TRUE),
              uci=mean(cost_per_YLL_averted_uci, na.rm=TRUE),
              metric="cost_per_YLL_averted")
  final_df <- rbind(final_df, summed)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  summed <- horizon_dataframe %>%
    group_by(!!! arg_setting) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE),
              lci=mean(deaths_averted_per_100k_vaccinated_lci, na.rm=TRUE),
              uci=mean(deaths_averted_per_100k_vaccinated_uci, na.rm=TRUE),
              metric="deaths_averted_per_100k_vaccinated")
  final_df <- rbind(final_df, summed)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)
}

###################
# fix_metric_name #
###################
fix_metric_name <- function(dataframe){

  # Read metric column as caharacter
  dataframe$metric <- as.character(dataframe$metric)

  # Improve names visually for plot
  dataframe$metric[dataframe$metric=="cost_per_death_averted"] <- "Cost per death averted"
  dataframe$metric[dataframe$metric=="cost_per_YLL_averted"] <- "Cost per DALY averted"
  dataframe$metric[dataframe$metric=="cost_per_DALY_averted"] <- "Cost per DALY averted"
  dataframe$metric[dataframe$metric=="deaths_averted_per_100k_vaccinated"] <- "Deaths averted per 100k vaccinated"
  dataframe$metric[dataframe$metric=="fully_vaccinated"] <- "Fully vaccinated"
  dataframe$metric[dataframe$metric=="total_cost"] <- "Total cost"
  dataframe$metric[dataframe$metric=="total_deaths"] <- "Total deaths"
  dataframe$metric[dataframe$metric=="total_deaths_averted"] <- "Total deaths averted"
  dataframe$metric[dataframe$metric=="total_vials"] <- "Total vials"
  dataframe$metric[dataframe$metric=="total_YLL"] <- "Total DALY"
  dataframe$metric[dataframe$metric=="total_DALY"] <- "Total DALY"
  dataframe$metric[dataframe$metric=="total_YLL_averted"] <- "Total DALY averted"
  dataframe$metric[dataframe$metric=="total_DALY_averted"] <- "Total DALY averted"
  dataframe$metric[dataframe$metric=="U5_deaths_averted"] <- "U5 deaths averted"
  dataframe$metric[dataframe$metric=="vaccinated"] <- "Vaccinated"
  dataframe$metric[dataframe$metric=="U5_YLL_averted"] <- "U5 DALY averted"
  dataframe$metric[dataframe$metric=="DALY_averted_U5"] <- "U5 DALY averted"

  return(dataframe)
}

#########################
# arrange_factor_levels #
#########################
arrange_factor_levels <- function(dataframe, type){

  if(type=="multi"){
    levels(dataframe$metric)[levels(dataframe$metric)=="total_deaths"] <- "B) Total deaths"
    levels(dataframe$metric)[levels(dataframe$metric)=="total_YLL"] <- "A) Total DALYs (x1000)"
    levels(dataframe$metric)[levels(dataframe$metric)=="cost_per_death_averted"] <- "D) Cost per death averted"
    levels(dataframe$metric)[levels(dataframe$metric)=="cost_per_YLL_averted"] <- "C) Cost per DALY averted"
    
    new_order <- c("A) Total DALYs (x1000)", "B) Total deaths", "C) Cost per DALY averted", "D) Cost per death averted")
  } else if(type=="uni"){
    levels(dataframe$metric)[levels(dataframe$metric)=="total_deaths"] <- "B) Total deaths (x1000)"
    levels(dataframe$metric)[levels(dataframe$metric)=="total_YLL"] <- "A) Total DALYs (x1000)"
    levels(dataframe$metric)[levels(dataframe$metric)=="cost_per_death_averted"] <- "D) Cost per death averted"
    levels(dataframe$metric)[levels(dataframe$metric)=="cost_per_YLL_averted"] <- "C) Cost per DALY averted"
    
    new_order <- c("A) Total DALYs (x1000)", "B) Total deaths (x1000)", "C) Cost per DALY averted", "D) Cost per death averted")
  }
  
  dataframe$metric <- as.factor(dataframe$metric)
  dataframe <- arrange(transform(dataframe,
                                 metric=factor(metric, levels=new_order)), metric)

  return(dataframe)
}
