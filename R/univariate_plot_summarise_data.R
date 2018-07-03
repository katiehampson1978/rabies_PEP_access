# Functions to summarise univariate data for plot

summarise_univar_data_global_gavi <- function(univariate_for_plot, lci,uci){

  # Sum across years for each run
  summed <- univariate_for_plot %>%
    group_by(variable, iter) %>%
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
    group_by(variable) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE))

  # Summarise CIs
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_deaths, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_deaths"
  final_df = global_summary

  #' * TOTAL_VIALS *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_vials, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_vials, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_vials, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_vials"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_COST *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_cost, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_cost, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_cost, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_cost"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(U5_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(U5_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "U5_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_YLL, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_DALY"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_DALY_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(YLL_averted_U5, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(YLL_averted_U5, probs=lci_p, na.rm=TRUE),
              uci = quantile(YLL_averted_U5, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "DALY_averted_U5"
  final_df <- rbind(final_df, global_summary)

  #' * VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * FULLY_VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(fully_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(fully_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "fully_vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_DEATH_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(cost_per_death_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_death_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "cost_per_death_averted"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(cost_per_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "cost_per_DALY_averted"
  final_df <- rbind(final_df, global_summary)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(deaths_averted_per_100k_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(deaths_averted_per_100k_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "deaths_averted_per_100k_vaccinated"
  final_df <- rbind(final_df, global_summary)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)

}

summarise_univar_data_global_gavi_2018 <- function(univariate_for_plot, lci,uci){

  # Select on countries that are gavi eligible in 2018
  limited_univar_for_plot <- univariate_for_plot[which(univariate_for_plot$gavi_2018==TRUE),]

  # Sum across years for each run
  summed <- limited_univar_for_plot %>%
    group_by(variable, iter) %>%
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
    group_by(variable) %>%
    summarise(m = mean(total_deaths, na.rm=TRUE))

  # Summarise CIs
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_deaths, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_deaths"
  final_df = global_summary

  #' * TOTAL_VIALS *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_vials, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_vials, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_vials, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_vials"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_COST *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_cost, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_cost, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_cost, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_cost"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_DEATHS_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(U5_deaths_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(U5_deaths_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(U5_deaths_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "U5_deaths_averted"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_YLL, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_YLL, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_DALY"
  final_df <- rbind(final_df, global_summary)

  #' * TOTAL_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(total_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(total_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(total_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "total_DALY_averted"
  final_df <- rbind(final_df, global_summary)

  #' * U5_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(YLL_averted_U5, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(YLL_averted_U5, probs=lci_p, na.rm=TRUE),
              uci = quantile(YLL_averted_U5, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "DALY_averted_U5"
  final_df <- rbind(final_df, global_summary)

  #' * VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * FULLY_VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(fully_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(fully_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(fully_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "fully_vaccinated"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_DEATH_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(cost_per_death_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(cost_per_death_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_death_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "cost_per_death_averted"
  final_df <- rbind(final_df, global_summary)

  #' * COST_PER_YLL_AVERTED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(cost_per_YLL_averted, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(cost_per_YLL_averted, probs=lci_p, na.rm=TRUE),
              uci = quantile(cost_per_YLL_averted, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "cost_per_DALY_averted"
  final_df <- rbind(final_df, global_summary)

  #' * DEATHS_AVERTED_PER_100K_VACCINATED *
  global_mean <- summed %>%
    group_by(variable) %>%
    summarise(m = mean(deaths_averted_per_100k_vaccinated, na.rm=TRUE))
  global_cis <- summed %>%
    group_by(variable) %>%
    summarise(lci = quantile(deaths_averted_per_100k_vaccinated, probs=lci_p, na.rm=TRUE),
              uci = quantile(deaths_averted_per_100k_vaccinated, probs=uci_p, na.rm=TRUE))
  global_summary <- left_join(global_mean, global_cis, by=c("variable"))
  global_summary$metric <- "deaths_averted_per_100k_vaccinated"
  final_df <- rbind(final_df, global_summary)

  final_df$metric <- as.character(final_df$metric)
  final_df$metric <- as.factor(final_df$metric)

  return(final_df)

}
