################################################################################
#                     7.5 Run Multivar decision tree model                     #
#                          Country-specific LTs; discounting                   #
# Run discountd minimmum costs  - improved PEP and RIG (for ICERs)
# save to folder countryLTs_discount_mincosts
# Run discountd maximum costs  - improved PEP and RIG (for ICERs)
# save to folder countryLTs_discount_maxcosts
################################################################################

#' * Life Tables - Country specific *
#' * Discounting - 3% *
#' * PEP cost - $2.5 (low) *
#' * RIG cost - $20 (low) *
#' * Intro grant - $100k *
#' * Scenarios - a3_1, a4 *
#' * Run count - 500 *

rm(list=ls())

# Load in packages
library(gdata)
library(rlang)
library(reshape2)
library(ggplot2)
library(tools)
library(triangle)
library(plyr)
library(dplyr)
library(Hmisc)

# Load in functions
source("R/YLL.R") # Calculate YLL given life tables and rabies age distribution
source("R/PEP.R") # Vial use under different regimens and throughput
source("R/prob_rabies.R") # Probability of developing rabies - sensitivity analysis
source("R/decision_tree_sensitivity_by_year.R") # Sensitivity analysis
source("R/decision_tree_multivariate_analysis_by_year_v2.R") # Multivariate sensitivity analysis
source("R/multivar_output_summary_V3.R")
source("R/scenario_params.R") # Parameters and functions for gavi support and phasing

# Set folder name for output
folder_name <- "countryLTs_discount_mincosts"

######################
# 1. Setup variables #
######################

# These variables are setup/loaded in the order they are used in the
# multivariate_analysis() function arguments

# DEFAULTS
rabies = read.csv("data/baseline_incidence_Gavi.csv") # incidence from fitted model - with NO vaccination
data <- read.csv("output/gavi_output_data.csv") # Load gavi-prepared data
params <- read.csv("output/bio_data.csv") # parameters i.e. rabies transmission, prevention given incomplete PEP
vacc <- read.csv("data/vaccine_use.csv") # PEP scenarios - clinic throughput, regimen, completeness, vials, clinic visits:
dogs <- read.csv(file="output/dogs_pop_traj.csv", stringsAsFactors = FALSE) # dog pop 2018-2070 created in 6.elimination_traj.R
elimination_traj <- read.csv(file="output/rabies_traj.csv") # by year of global business plan
y1 = "2020"; yN = "2035"
pop = data[,grep(y1, names(data)):grep(yN, names(data))] # needs this format to combine with elimination trajectories!

# Set time horizon: from 2020 to 2035
hrz=length(2020:2035)

# Load in DALYs - disability weightings and lifetables
DALYrabies_input <- read.csv("data/DALY_params_rabies.csv") # from Knobel et al. 2005

# SPECIFIC PARAMETERS
# Life table
country_LE <- read.csv("data/lifetables_bycountry.csv")
country_LE <- country_LE[-which(country_LE$age_from == 100),]
LE2020 <- country_LE[which(country_LE$year == 2020),] # Use 2020 age distributions throughout!

# Set discounting rate
discount = 0.03

# Set prices (USD) - MINIMUM
gavi_intro_grant <- 100000 # Intro grant
gavi_vaccine_price <- 2.5 # vaccine cost per vial
gavi_RIG_price <- 20 # ERIG cost per vial - CHANGE TO $20!


################
# 2. Run model #
################

# Set number of runs
n = 5 #
# n = 500 # ~1 hr per scenario, so just 2 hours

# scenario_a1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                      DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a1, paste("output/", folder_name, "/scenario_a1_gavi.csv", sep="")) # # a1 works ok

# scenario_a2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                      DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a2, paste("output/", folder_name, "/scenario_a2_gavi.csv", sep="")) # # a2 works ok

scenario_a3_1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                       DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_1, paste("output/", folder_name, "/scenario_a3_1_gavi.csv", sep="")) # # a3_1 works ok

# scenario_a3_2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="low", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_2, paste("output/", folder_name, "/scenario_a3_2_gavi.csv", sep="")) # # a3_2 works ok

# scenario_a3_3 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="high", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_3, paste("output/", folder_name, "/scenario_a3_3_gavi.csv", sep="")) # # a3_3 works ok

scenario_a4 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="high risk", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a4, paste("output/", folder_name, "/scenario_a4_gavi.csv", sep="")) # # a4 works ok
#
# scenario_a5_1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# # write.csv(scenario_a5_1, paste("output/", folder_name, "/scenario_a5_1_gavi.csv", sep="")) # # a5_1 works ok
#
# scenario_a5_2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=TRUE)
# write.csv(scenario_a5_2, paste("output/", folder_name, "/scenario_a5_2_gavi.csv", sep="")) # # a5_2 works ok

###########################################
# 3. Bind outputs into a single dataframe #
###########################################

# Append all results into a dataframe
out <- rbind.data.frame(
  #cbind.data.frame(scenario_a1, scenario="a1"),
  #cbind.data.frame(scenario_a2, scenario="a2"),
  cbind.data.frame(scenario_a3_1, scenario="a3_1"),
  #cbind.data.frame(scenario_a3_2, scenario="a3_2"),
  #cbind.data.frame(scenario_a3_3, scenario="a3_3"),
  cbind.data.frame(scenario_a4, scenario="a4") # ,
  # cbind.data.frame(scenario_a5_1, scenario="a5_1"),
  # cbind.data.frame(scenario_a5_2, scenario="a5_2")
  )
dim(out)
table(out$scenario)

countries <- unique(out$country)
scenarios <- unique(out$scenario)
yrs <- unique(out$year)

# INCLUDE GAVI ELIGIBILITY
gavi_info <- read.csv("output/gavi_output_data.csv", stringsAsFactors=FALSE)
out <- merge(out, data.frame(country=gavi_info$country, gavi_2018=gavi_info$gavi_2018), by="country", all.x=TRUE)

# CE outputs
out$cost_per_death_averted <-  out$total_cost/out$total_deaths_averted
out$cost_per_YLL_averted <-  out$total_cost/out$total_YLL_averted
out$deaths_averted_per_100k_vaccinated <-  out$total_deaths_averted/out$vaccinated/100000

# summarize by iteration over time horizon
out_horizon = country_horizon_iter(out)

######################################
# 4a. Create summary outputs         #
######################################

# Country, cluster, & global by year
country_summary_yr = multivar_country_summary(out, year = TRUE)
cluster_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting ="cluster")
global_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting="global")
gavi2018_summary_yr = multivar_summary(country_summary_yr[which(country_summary_yr$gavi_2018==TRUE),], year=TRUE, setting="global")

write.csv(country_summary_yr, paste("output/", folder_name, "/country_stats.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_yr, paste("output/", folder_name, "/cluster_stats.csv", sep=""), row.names=FALSE)
write.csv(global_summary_yr, paste("output/", folder_name, "/global_stats.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_yr, paste("output/", folder_name, "/gavi2018_stats.csv", sep=""), row.names=FALSE)

################################################
# 4b. Create summary outputs over time horizon #
################################################

# Country, cluster, & global over time horizon
country_summary_horizon = multivar_country_summary(out_horizon, year = FALSE)
cluster_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting ="cluster")
global_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting="global")
gavi2018_summary_horizon = multivar_summary(country_summary_horizon[which(country_summary_horizon$gavi_2018==TRUE),], year=FALSE, setting="global")

write.csv(country_summary_horizon, paste("output/", folder_name, "/country_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_horizon, paste("output/", folder_name, "/cluster_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(global_summary_horizon, paste("output/", folder_name, "/global_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_horizon, paste("output/", folder_name, "/gavi2018_stats_horizon.csv", sep=""), row.names=FALSE)


################################################################################
#                     Run Multivar decision tree model                         #
################################################################################

#' * Life Tables - Country specific *
#' * Discounting - 3% *
#' * PEP cost - $10 (max) *
#' * RIG cost - $45 (default) *
#' * Intro grant - $500k *
#' * Scenarios - a3_1, a4 *
#' * Run count - 500 *

# Set folder name for output
folder_name <- "countryLTs_discount_maxcosts"

# Set prices (USD) - MAXIMUM
gavi_intro_grant <- 500000 # Intro grant
gavi_vaccine_price <- 10 # vaccine cost per vial
gavi_RIG_price <- 45 # ERIG cost per vial


################
# 2. Run model #
################

# scenario_a1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                      DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a1, paste("output/", folder_name, "/scenario_a1_gavi.csv", sep="")) # # a1 works ok

# scenario_a2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                      DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a2, paste("output/", folder_name, "/scenario_a2_gavi.csv", sep="")) # # a2 works ok

scenario_a3_1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                       DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_1, paste("output/", folder_name, "/scenario_a3_1_gavi.csv", sep="")) # # a3_1 works ok

# scenario_a3_2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="low", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_2, paste("output/", folder_name, "/scenario_a3_2_gavi.csv", sep="")) # # a3_2 works ok

# scenario_a3_3 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="high", DogVax_TF=F, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a3_3, paste("output/", folder_name, "/scenario_a3_3_gavi.csv", sep="")) # # a3_3 works ok

scenario_a4 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="high risk", discount=discount, breaks="5yr", IBCM=FALSE)
# write.csv(scenario_a4, paste("output/", folder_name, "/scenario_a4_gavi.csv", sep="")) # # a4 works ok

# scenario_a5_1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# # write.csv(scenario_a5_1, paste("output/", folder_name, "/scenario_a5_1_gavi.csv", sep="")) # # a5_1 works ok
#
# scenario_a5_2 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=T, VaxRegimen="Updated TRC",
#                                        DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=TRUE)
# write.csv(scenario_a5_2, paste("output/", folder_name, "/scenario_a5_2_gavi.csv", sep="")) # # a5_2 works ok

###########################################
# 3. Bind outputs into a single dataframe #
###########################################

# Append all results into a dataframe
out <- rbind.data.frame(
  #cbind.data.frame(scenario_a1, scenario="a1"),
  #cbind.data.frame(scenario_a2, scenario="a2"),
  cbind.data.frame(scenario_a3_1, scenario="a3_1"),
  #cbind.data.frame(scenario_a3_2, scenario="a3_2"),
  #cbind.data.frame(scenario_a3_3, scenario="a3_3"),
  cbind.data.frame(scenario_a4, scenario="a4") # ,
  # cbind.data.frame(scenario_a5_1, scenario="a5_1"),
  # cbind.data.frame(scenario_a5_2, scenario="a5_2")
)
dim(out)
table(out$scenario)

# INCLUDE GAVI ELIGIBILITY
out <- merge(out, data.frame(country=gavi_info$country, gavi_2018=gavi_info$gavi_2018), by="country", all.x=TRUE)

# CE outputs
out$cost_per_death_averted <-  out$total_cost/out$total_deaths_averted
out$cost_per_YLL_averted <-  out$total_cost/out$total_YLL_averted
out$deaths_averted_per_100k_vaccinated <-  out$total_deaths_averted/out$vaccinated/100000

# summarize by iteration over time horizon
out_horizon = country_horizon_iter(out)

######################################
# 4a. Create summary outputs         #
######################################

# Country, cluster, & global by year
country_summary_yr = multivar_country_summary(out, year = TRUE)
cluster_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting ="cluster")
global_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting="global")
gavi2018_summary_yr = multivar_summary(country_summary_yr[which(country_summary_yr$gavi_2018==TRUE),], year=TRUE, setting="global")

write.csv(country_summary_yr, paste("output/", folder_name, "/country_stats.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_yr, paste("output/", folder_name, "/cluster_stats.csv", sep=""), row.names=FALSE)
write.csv(global_summary_yr, paste("output/", folder_name, "/global_stats.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_yr, paste("output/", folder_name, "/gavi2018_stats.csv", sep=""), row.names=FALSE)

################################################
# 4b. Create summary outputs over time horizon #
################################################

# Country, cluster, & global over time horizon
country_summary_horizon = multivar_country_summary(out_horizon, year = FALSE)
cluster_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting ="cluster")
global_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting="global")
gavi2018_summary_horizon = multivar_summary(country_summary_horizon[which(country_summary_horizon$gavi_2018==TRUE),], year=FALSE, setting="global")

write.csv(country_summary_horizon, paste("output/", folder_name, "/country_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_horizon, paste("output/", folder_name, "/cluster_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(global_summary_horizon, paste("output/", folder_name, "/global_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_horizon, paste("output/", folder_name, "/gavi2018_stats_horizon.csv", sep=""), row.names=FALSE)
