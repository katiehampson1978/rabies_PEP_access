################################################################################
#           MULTIVARIATE PLOT - GENERATE MEAN AND CIS FOR EACH METRIC          #
################################################################################

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

summarise_multivariate_data_cluster <- function(multivariate_for_plot, lci,uci){

  # Sum across years for each run
  summed <- multivariate_for_plot %>%
    group_by(cluster, scenario, iter) %>%
    summarise(total_deaths = sum(total_deaths),
              total_vials = sum(total_vials),
              total_cost = sum(total_cost),
              total_deaths_averted = sum(total_deaths_averted),
              U5_deaths_averted = sum(U5_deaths_averted),
              total_YLL = sum(total_YLL),
              total_YLL_averted = sum(total_YLL_averted),
              YLL_averted_U5 = sum(YLL_averted_U5),
              vaccinated = sum(vaccinated),
              fully_vaccinated = sum(fully_vaccinated),
              #cost_per_death_averted = sum(cost_per_death_averted),
              cost_per_death_averted = sum(total_cost)/sum(total_deaths_averted),
              #cost_per_YLL_averted = sum(cost_per_YLL_averted),
              cost_per_YLL_averted = sum(total_cost)/sum(total_YLL_averted),
              #deaths_averted_per_100k_vaccinated = sum(deaths_averted_per_100k_vaccinated))
              deaths_averted_per_100k_vaccinated = sum(total_deaths_averted)/(sum(vaccinated)/100000))


  #' * TOTAL_DEATHS *
  # Summarise means - quantiles won't calculate with this for some reason - done below!
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE))

  # Summarise CIs
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_deaths, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_deaths"
  final_df = cluster_summary

  #' * TOTAL_VIALS *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_vials, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_vials, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_vials, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_vials"
  final_df <- rbind(final_df, cluster_summary)

  #' * TOTAL_COST *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_cost, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_cost, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_cost, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_cost"
  final_df <- rbind(final_df, cluster_summary)

  #' * TOTAL_DEATHS_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths_averted, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_deaths_averted"
  final_df <- rbind(final_df, cluster_summary)

  #' * U5_DEATHS_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(U5_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(U5_deaths_averted, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "U5_deaths_averted"
  final_df <- rbind(final_df, cluster_summary)

  #' * TOTAL_YLL *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_YLL, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_YLL"
  final_df <- rbind(final_df, cluster_summary)

  #' * TOTAL_YLL_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(total_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL_averted, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "total_YLL_averted"
  final_df <- rbind(final_df, cluster_summary)

  #' * U5_YLL_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(YLL_averted_U5, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(YLL_averted_U5, probs=lci_p, na.rm=TRUE),
              uci = quantile(YLL_averted_U5, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "YLL_averted_U5"
  final_df <- rbind(final_df, cluster_summary)

  #' * VACCINATED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(vaccinated, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "vaccinated"
  final_df <- rbind(final_df, cluster_summary)

  #' * FULLY_VACCINATED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(fully_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(fully_vaccinated, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "fully_vaccinated"
  final_df <- rbind(final_df, cluster_summary)

  #' * COST_PER_DEATH_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(cost_per_death_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_death_averted, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "cost_per_death_averted"
  final_df <- rbind(final_df, cluster_summary)

  #' * COST_PER_YLL_AVERTED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(cost_per_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_YLL_averted, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "cost_per_YLL_averted"
  final_df <- rbind(final_df, cluster_summary)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  cluster_mean <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE))
  cluster_cis <- summed %>%
    group_by(cluster, scenario) %>%
    summarise(lci = quantile(deaths_averted_per_100k_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(deaths_averted_per_100k_vaccinated, probs=uci_p, na.rm=TRUE))
  cluster_summary <- left_join(cluster_mean, cluster_cis, by=c("cluster", "scenario"))
  cluster_summary$metric <- "deaths_averted_per_100k_vaccinated"
  final_df <- rbind(final_df, cluster_summary)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)

}

summarise_multivariate_data_global_gavi <- function(multivariate_for_plot, lci,uci){

  # Sum across years for each run
  summed <- multivariate_for_plot %>%
    group_by(scenario, iter) %>%
    summarise(total_deaths = sum(total_deaths),
              total_vials = sum(total_vials),
              total_cost = sum(total_cost),
              total_deaths_averted = sum(total_deaths_averted),
              U5_deaths_averted = sum(U5_deaths_averted),
              total_YLL = sum(total_YLL),
              total_YLL_averted = sum(total_YLL_averted),
              YLL_averted_U5 = sum(YLL_averted_U5),
              vaccinated = sum(vaccinated),
              fully_vaccinated = sum(fully_vaccinated),
              #cost_per_death_averted = sum(cost_per_death_averted),
              cost_per_death_averted = sum(total_cost)/sum(total_deaths_averted),
              #cost_per_YLL_averted = sum(cost_per_YLL_averted),
              cost_per_YLL_averted = sum(total_cost)/sum(total_YLL_averted),
              #deaths_averted_per_100k_vaccinated = sum(deaths_averted_per_100k_vaccinated))
              deaths_averted_per_100k_vaccinated = sum(total_deaths_averted)/(sum(vaccinated)/100000))


  #' * TOTAL_DEATHS *
  # Summarise means - quantiles won't calculate with this for some reason - done below!
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE))

  # Summarise CIs
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_deaths, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_deaths"
  final_df = global_summary

  #' * TOTAL_VIALS *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_vials, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_vials, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_vials, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_vials"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_COST *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_cost, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_cost, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_cost, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_cost"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(U5_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(U5_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "U5_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_YLL, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_YLL"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_YLL_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(YLL_averted_U5, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(YLL_averted_U5, probs=lci_p, na.rm=TRUE),
              uci = quantile(YLL_averted_U5, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "YLL_averted_U5"
  final_df <- rbind(final_df, global_summary)

  #' * VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * FULLY_VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(fully_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(fully_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "fully_vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_DEATH_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(cost_per_death_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_death_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "cost_per_death_averted"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(cost_per_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "cost_per_YLL_averted"
  final_df <- rbind(final_df, global_summary)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(deaths_averted_per_100k_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(deaths_averted_per_100k_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "deaths_averted_per_100k_vaccinated"
  final_df <- rbind(final_df, global_summary)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)

}

summarise_multivariate_data_global_gavi_2018 <- function(multivariate_for_plot, lci,uci){

  # Select on countries that are gavi eligible in 2018
  limited_multivariate_for_plot <- multivariate_for_plot[which(multivariate_for_plot$gavi_2018==TRUE),]

  # Sum across years for each run
  summed <- limited_multivariate_for_plot %>%
    group_by(scenario, iter) %>%
    summarise(total_deaths = sum(total_deaths),
              total_vials = sum(total_vials),
              total_cost = sum(total_cost),
              total_deaths_averted = sum(total_deaths_averted),
              U5_deaths_averted = sum(U5_deaths_averted),
              total_YLL = sum(total_YLL),
              total_YLL_averted = sum(total_YLL_averted),
              YLL_averted_U5 = sum(YLL_averted_U5),
              vaccinated = sum(vaccinated),
              fully_vaccinated = sum(fully_vaccinated),
              #cost_per_death_averted = sum(cost_per_death_averted),
              cost_per_death_averted = sum(total_cost)/sum(total_deaths_averted),
              #cost_per_YLL_averted = sum(cost_per_YLL_averted),
              cost_per_YLL_averted = sum(total_cost)/sum(total_YLL_averted),
              #deaths_averted_per_100k_vaccinated = sum(deaths_averted_per_100k_vaccinated))
              deaths_averted_per_100k_vaccinated = sum(total_deaths_averted)/(sum(vaccinated)/100000))


  #' * TOTAL_DEATHS *
  # Summarise means - quantiles won't calculate with this for some reason - done below!
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE))

  # Summarise CIs
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_deaths, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_deaths"
  final_df = global_summary

  #' * TOTAL_VIALS *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_vials, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_vials, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_vials, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_vials"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_COST *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_cost, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_cost, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_cost, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_cost"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(U5_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(U5_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "U5_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_YLL, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_YLL"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(total_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "total_YLL_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(YLL_averted_U5, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(YLL_averted_U5, probs=lci_p, na.rm=TRUE),
              uci = quantile(YLL_averted_U5, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "YLL_averted_U5"
  final_df <- rbind(final_df, global_summary)

  #' * VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * FULLY_VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(fully_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(fully_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "fully_vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_DEATH_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(cost_per_death_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_death_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "cost_per_death_averted"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(cost_per_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "cost_per_YLL_averted"
  final_df <- rbind(final_df, global_summary)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  global_mean <- summed %>%
    group_by(scenario) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(scenario) %>%
    summarise(lci = quantile(deaths_averted_per_100k_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(deaths_averted_per_100k_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("scenario"))
  global_summary$metric <- "deaths_averted_per_100k_vaccinated"
  final_df <- rbind(final_df, global_summary)

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
