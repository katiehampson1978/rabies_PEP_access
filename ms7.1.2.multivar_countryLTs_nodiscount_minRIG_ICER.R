################################################################################
#                   7.1.2 Run Multivar decision tree model                     #
#             OUTPUTS USED BY GAVI AND IN PAPER FOR BURDEN ESTIMATES           #
# run ICER scenarios (no discount) for MIN prices i.e. improved PEP & RIG
# save outputs to folder: countryLTs_nodiscount
################################################################################

#' * Life Tables - Country specific *
#' * Discounting - 0 *
#' * PEP cost - $5 (default) *
#' * RIG cost - $20 (low) *
#' * Intro grant - $100k *
#' * Scenarios - a1, a3_1, a4 *
#' * Run count - 1000 *

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
library(tidyr)

# Load in functions
source("R/YLL.R") # Calculate YLL given life tables and rabies age distribution
source("R/PEP.R") # Vial use under different regimens and throughput
source("R/prob_rabies.R") # Probability of developing rabies - sensitivity analysis
source("R/decision_tree_sensitivity_by_year.R") # Sensitivity analysis
source("R/decision_tree_multivariate_analysis_by_year_v2.R") # Multivariate sensitivity analysis
source("R/scenario_params.R") # Parameters and functions for gavi support and phasing
source("R/multivar_output_summary_Github.R") # MEAN
source("R/multivariate_plot_summarise_data_Github.R")

# Set folder name for output
folder_name <- "countryLTs_nodiscount"

######################
# 1. Setup variables #
######################

rabies = read.csv("data/baseline_incidence_Gavi_final.csv")
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
discount = 0

# Set prices (USD)
gavi_intro_grant <- 100000 # Intro grant
gavi_vaccine_price <- 5 # vaccine cost per vial
gavi_RIG_price <- 20 # ERIG cost per vial

################
# 2. Run model #
################

# Set number of runs
n=1000

# SQ - Paper SC1
scenario_a1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)

# Improved PEP access - Paper SC2 (base)
scenario_a3_1 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                       DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)
# Improved PEP access + RIG - Paper SC3
scenario_a4 <- multivariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="high risk", discount=discount, breaks="5yr", IBCM=FALSE)

###########################################
# 3. Bind outputs into a single dataframe #
###########################################

# Append all results into a dataframe
out <- rbind.data.frame(
  cbind.data.frame(scenario_a1, scenario="a1"),
  cbind.data.frame(scenario_a3_1, scenario="a3_1"),
  cbind.data.frame(scenario_a4, scenario="a4"))
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

# Summarize by iteration over time horizon
out_horizon = country_horizon_iter(out)

######################################
# 4a. Create summary outputs         #
######################################

# Country, cluster, & global by year
country_summary_yr = multivar_country_summary(out, year = TRUE)
cluster_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting ="cluster")
global_summary_yr = multivar_summary(country_summary_yr, year=TRUE, setting="global")
gavi2018_summary_yr = multivar_summary(country_summary_yr[which(country_summary_yr$gavi_2018==TRUE),], year=TRUE, setting="global")

write.csv(country_summary_yr, paste("output/", folder_name, "/country_stats_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_yr, paste("output/", folder_name, "/cluster_stats_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(global_summary_yr, paste("output/", folder_name, "/global_stats_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_yr, paste("output/", folder_name, "/gavi2018_stats_minRIG_ICER.csv", sep=""), row.names=FALSE)

################################################
# 4b. Create summary outputs over time horizon #
################################################

# Country, cluster, & global over time horizon
country_summary_horizon = multivar_country_summary(out_horizon, year = FALSE)
cluster_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting ="cluster")
global_summary_horizon = multivar_summary(country_summary_horizon, year=FALSE, setting="global")
gavi2018_summary_horizon = multivar_summary(country_summary_horizon[which(country_summary_horizon$gavi_2018==TRUE),], year=FALSE, setting="global")

write.csv(country_summary_horizon, paste("output/", folder_name, "/country_stats_horizon_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_horizon, paste("output/", folder_name, "/cluster_stats_horizon_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(global_summary_horizon, paste("output/", folder_name, "/global_stats_horizon_minRIG_ICER.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_horizon, paste("output/", folder_name, "/gavi2018_stats_horizon_minRIG_ICER.csv", sep=""), row.names=FALSE)
