#################
# scenario_prep #
#################

# This function alters the charcter name of the scenarios from Gavi to Paper:
# Scenario A1 -> 1
# Scenario A2 -> 4a
# Scenario A3_1 -> 2
# Scenario A4 -> 3
# Scenario A5_1 -> 4b
# Scenario A5_2 -> 4c
scenario_prep <- function(dataframe){

  dataframe$scenario[dataframe$scenario=="a1"] <- "1"
  dataframe$scenario[dataframe$scenario=="a2"] <- "4a"
  dataframe$scenario[dataframe$scenario=="a3_1"] <- "2"
  dataframe$scenario[dataframe$scenario=="a4"] <- "3"
  dataframe$scenario[dataframe$scenario=="a5_1"] <- "4b"
  dataframe$scenario[dataframe$scenario=="a5_2"] <- "4c"

  return(dataframe)
}

#####################################
# stacked_incremental_cost_analysis #
#####################################

# This function takes two "scenario names" as arguments (or 3 if provided), and
# produces an incremental cost analysis.
stacked_incremental_cost_analysis <- function(global_df, cluster_df, scenario_1, scenario_2, scenario_3=NULL){

  # Create global row
  global_row <- data.frame(Level="all countries",
                           total_deaths_1 = global_df$total_deaths[global_df$scenario==scenario_1],
                           total_cost_1 = global_df$total_cost[global_df$scenario==scenario_1],
                           total_deaths_2 = global_df$total_deaths[global_df$scenario==scenario_2],
                           total_cost_2 = global_df$total_cost[global_df$scenario==scenario_2])

  # Create cluster row
  cluster_rows <- data.frame(Level=unique(cluster_df$cluster),
                             total_deaths_1 = cluster_df$total_deaths[cluster_df$scenario==scenario_1],
                             total_cost_1 = cluster_df$total_cost[cluster_df$scenario==scenario_1],
                             total_deaths_2 = cluster_df$total_deaths[cluster_df$scenario==scenario_2],
                             total_cost_2 = cluster_df$total_cost[cluster_df$scenario==scenario_2])

  # Join dataframes together
  bound_df <- rbind(cluster_rows, global_row)

  # Calculate differences
  bound_df$deaths_diff <- bound_df$total_deaths_2-bound_df$total_deaths_1
  bound_df$cost_diff <- bound_df$total_cost_2-bound_df$total_cost_1

  # Calculate Incremental cost
  bound_df$ICER_deaths = bound_df$cost_diff/bound_df$deaths_diff

  # If a 3rd scenario is used, calculate and cbind results to df
  if(!is.null(scenario_3)){
    # Create global row
    global_row <- data.frame(Level="all countries",
                             total_deaths_3 = global_df$total_deaths[global_df$scenario==scenario_3],
                             total_cost_3 = global_df$total_cost[global_df$scenario==scenario_3])

    # Create cluster row
    cluster_rows <- data.frame(Level=unique(cluster_df$cluster),
                               total_deaths_3 = cluster_df$total_deaths[cluster_df$scenario==scenario_3],
                               total_cost_3 = cluster_df$total_cost[cluster_df$scenario==scenario_3])

    # Join dataframes together
    extra_df <- rbind(cluster_rows, global_row)
    bound_df <- cbind(bound_df, extra_df[,-1])

    # Calculate differences
    bound_df$deaths_diff_3 <- bound_df$total_deaths_3-bound_df$total_deaths_1
    bound_df$cost_diff_3 <- bound_df$total_cost_3-bound_df$total_cost_1

    # Calculate Incremental cost
    bound_df$ICER_deaths_3 = bound_df$cost_diff_3/bound_df$deaths_diff_3
  }

  # Stack dataframes
  ICER_stacked_df <- data.frame(Level=bound_df$Level, deaths_diff=bound_df$deaths_diff,
                              cost_diff=bound_df$cost_diff, ICER_deaths=bound_df$ICER_deaths,
                              Scenario=paste0(scenario_1, " vs. ", scenario_2))

  if(!is.null(scenario_3)){
    stacked_df_p2 <- data.frame(Level=bound_df$Level, deaths_diff=bound_df$deaths_diff_3,
                                cost_diff=bound_df$cost_diff_3, ICER_deaths=bound_df$ICER_deaths_3,
                                Scenario=paste0(scenario_1, " vs. ", scenario_3))
    ICER_stacked_df <- rbind(ICER_stacked_df, stacked_df_p2)
  }

  return(ICER_stacked_df)

}

#################
# fig_4_df_prep #
#################
projected_outcomes_plot_prep <- function(global_df, country_df){

  # Prepare global dataframes
  global_deaths <- data.frame(Level="All countries", year=global_df$year, scenario=global_df$scenario,
                              variable="Deaths", mean=global_df$total_deaths,
                              lci=global_df$total_deaths_lci, uci=global_df$total_deaths_uci, stringsAsFactors=FALSE)
  global_vaccinated <- data.frame(Level="All countries", year=global_df$year, scenario=global_df$scenario,
                              variable="Persons vaccinated", mean=global_df$vaccinated,
                              lci=global_df$vaccinated_lci, uci=global_df$vaccinated_uci, stringsAsFactors=FALSE)
  global_vials <- data.frame(Level="All countries", year=global_df$year, scenario=global_df$scenario,
                              variable="Vaccine vials", mean=global_df$total_vials,
                              lci=global_df$total_vials_lci, uci=global_df$total_vials_uci, stringsAsFactors=FALSE)

  # Prepare country dataframes
  country_deaths <- data.frame(Level=country_df$country, year=country_df$year, scenario=country_df$scenario,
                              variable="Deaths", mean=country_df$total_deaths,
                              lci=country_df$total_deaths_lci, uci=country_df$total_deaths_uci, stringsAsFactors=FALSE)
  country_vaccinated <- data.frame(Level=country_df$country, year=country_df$year, scenario=country_df$scenario,
                                  variable="Persons vaccinated", mean=country_df$vaccinated,
                                  lci=country_df$vaccinated_lci, uci=country_df$vaccinated_uci, stringsAsFactors=FALSE)
  country_vials <- data.frame(Level=country_df$country, year=country_df$year, scenario=country_df$scenario,
                             variable="Vaccine vials", mean=country_df$total_vials,
                             lci=country_df$total_vials_lci, uci=country_df$total_vials_uci, stringsAsFactors=FALSE)

  # Bind dataframes together
  plot_df <- rbind(global_deaths, global_vaccinated, global_vials, country_deaths, country_vaccinated, country_vials)

  # Set level as factor
  plot_df$Level = factor(plot_df$Level, levels=c("All countries", "Bangladesh", "Ethiopia", "India", "Kenya", "Myanmar"))

  # Add text to act as text for plot panels
  plot_df$text <- NA
  for(i in 1:nrow(plot_df)){
    plot_df$text[i] <- paste0(plot_df$Level[i], "-", plot_df$variable[i])
  }

  return(plot_df)
}

###################
# summarise_costs #
###################
summarise_costs <- function(dataframe, scenario){

  cost_df <- dataframe[which(dataframe$scenario == scenario),]

  # Summarise by cluster
  cluster_summary <- cost_df %>%
    group_by(cluster) %>%
    summarise(min_total_cost=mean(total_cost.y),
              default_total_cost=mean(total_cost.x),
              max_total_cost=mean(total_cost),
              #min_DALY=mean(total_YLL.y),
              #default_DALY=mean(total_YLL.x),
              #max_DALY=mean(total_YLL),
              min_cost_per_death_averted=mean(cost_per_death_averted.y),
              default_cost_per_death_averted=mean(cost_per_death_averted.x),
              max_cost_per_death_averted=mean(cost_per_death_averted),
              min_cost_per_YLL_averted=mean(cost_per_YLL_averted.y),
              default_cost_per_YLL_averted=mean(cost_per_YLL_averted.x),
              max_cost_per_YLL_averted=mean(cost_per_YLL_averted))

  # Sumarise globally
  global_summary <- cluster_summary %>%
      summarise(cluster="global",
                min_total_cost=sum(min_total_cost),
                default_total_cost=sum(default_total_cost),
                max_total_cost=sum(max_total_cost),
                #min_DALY=mean(total_YLL.y),
                #default_DALY=mean(total_YLL.x),
                #max_DALY=mean(total_YLL),
                min_cost_per_death_averted=sum(min_cost_per_death_averted),
                default_cost_per_death_averted=sum(default_cost_per_death_averted),
                max_cost_per_death_averted=sum(max_cost_per_death_averted),
                min_cost_per_YLL_averted=sum(min_cost_per_YLL_averted),
                default_cost_per_YLL_averted=sum(default_cost_per_YLL_averted),
                max_cost_per_YLL_averted=sum(max_cost_per_YLL_averted))

  # Bind together summaries
  final_cost_df <- rbind(global_summary, cluster_summary)
  final_cost_df <- as.data.frame(final_cost_df)

  # Round number nearest 1000
  final_cost_df[,2:4] <- format(round(final_cost_df[,2:4], -3), nsmall=0, big.mark=",", trim=TRUE)

  # Round costs to 0 dp
  final_cost_df[,5:7] <- format(round(final_cost_df[,5:7], 0), nsmall=0, big.mark=",", trim=TRUE)
  final_cost_df[,8:10] <- format(round(final_cost_df[,8:10], 1), nsmall=1, big.mark=",", trim=TRUE)

  return(final_cost_df)

}