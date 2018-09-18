################################################################################
#                    7.6 Run decision-tree model                               #
# Run (discounted, default costs) univariate PSA - SQ, improved PEP, dog vax + PEP
# save to folder: univariate
################################################################################

#' * Life Tables - Country specific *
#' * Discounting - 3% *
#' * PEP cost - $5 (default) *
#' * RIG cost - $45 (default) *
#' * Intro grant - $100k *
#' * Scenarios - a1 *
#' * Run count - 500 *
#' * VARY: suspect_bite_incidence, non_suspect_bite_incidence, p_rabid, p_transmission, p_prevent *

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

# LOAD IN R FUNCTIONS
source("R/YLL.R") # Calculate YLL given life tables and rabies age distribution
source("R/PEP.R") # Vial use under different regimens and throughput
source("R/prob_rabies.R") # Probability of developing rabies - sensitivity analysis
source("R/decision_tree_univ_draw.R") # Univariable draws
source("R/decision_tree_univariate_analysis.R") # Univariate sensitivity analysis
source("R/decision_tree_sensitivity_by_year.R") # Sensitivity analysis
source("R/decision_tree_multivariate_analysis_by_year_v2.R") # Multivariate sensitivity analysis
source("R/scenario_params.R") # parameters and functions for gavi support and phasing
source("R/univar_output_summary.R")
source("R/univariate_plot_summarise_data_Github.R")

# Set folder name for output
folder_name <- "univariate"

######################
# 1. Setup variables #
######################

# DEFAULTS
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
discount = 0.03

# Set prices (USD)
gavi_intro_grant <- 100000 # Intro grant
gavi_vaccine_price <- 5 # vaccine cost per vial
gavi_RIG_price <- 45 # ERIG cost per vial

################################################################################
#                  RUN UNIVARIATE MODELS FOR GAVI COUNTRIES                  #
################################################################################

n = 500 # ~2 hrs per scenario, so 6 hours total

# Examine baseline scenario
scenario_a1 <- univariate_analysis(ndraw=n, horizon=hrz, GAVI_status="none", DogVax_TF=F, VaxRegimen="Updated TRC",
                                   DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)

# Examine improved PEP
scenario_a3_1 <- univariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=F, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)

# Examine dog vaccination plus improved PEP
scenario_a5_1 <- univariate_analysis(ndraw=n, horizon=hrz, GAVI_status="base", DogVax_TF=T, VaxRegimen="Updated TRC",
                                     DALYrabies=DALYrabies_input, LE=LE2020, RIG_status="none", discount=discount, breaks="5yr", IBCM=FALSE)

###########################################
# 3. Bind outputs into a single dataframe #
###########################################

# Append all results into a dataframe
out <- rbind.data.frame(
  cbind.data.frame(scenario_a1, scenario="a1"),
  cbind.data.frame(scenario_a3_1, scenario="a3_1"),
  cbind.data.frame(scenario_a5_1, scenario="a5_1"))
dim(out)
table(out$scenario)

countries <- unique(out$country)
scenarios <- unique(out$scenario)
yrs <- unique(out$year)

gavi_info <- read.csv("output/gavi_output_data.csv", stringsAsFactors=FALSE)
out <- merge(out, data.frame(country=gavi_info$country, cluster=gavi_info$cluster, gavi_2018=gavi_info$gavi_2018), by="country", all.x=TRUE)

# CE outputs
out$cost_per_death_averted <-  out$total_cost/out$total_deaths_averted
out$cost_per_YLL_averted <-  out$total_cost/out$total_YLL_averted
out$deaths_averted_per_100k_vaccinated <-  out$total_deaths_averted/out$vaccinated/100000

# Summarize by iteration over time horizon
out_horizon = country_horizon_iter_univar(out)
out_horizon$iter_order <- rank(out_horizon$total_deaths)
mean(out_horizon$total_deaths)

######################################
# 4a. Create summary outputs         #
######################################

# Country, cluster, & global by year
country_summary_yr = univar_country_summary(out, year = TRUE)
cluster_summary_yr = univar_summary(country_summary_yr, year=TRUE, setting ="cluster")
global_summary_yr = univar_summary(country_summary_yr, year=TRUE, setting="global")
gavi2018_summary_yr = univar_summary(country_summary_yr[which(country_summary_yr$gavi_2018==TRUE),], year=TRUE, setting="global")

write.csv(country_summary_yr, paste("output/", folder_name, "/country_stats.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_yr, paste("output/", folder_name, "/cluster_stats.csv", sep=""), row.names=FALSE)
write.csv(global_summary_yr, paste("output/", folder_name, "/global_stats.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_yr, paste("output/", folder_name, "/gavi2018_stats.csv", sep=""), row.names=FALSE)

################################################
# 4b. Create summary outputs over time horizon #
################################################

# Country, cluster, & global over time horizon
country_summary_horizon = univar_country_summary(out_horizon, year = FALSE)
cluster_summary_horizon = univar_summary(country_summary_horizon, year=FALSE, setting ="cluster")
global_summary_horizon = univar_summary(country_summary_horizon, year=FALSE, setting="global")
gavi2018_summary_horizon = univar_summary(country_summary_horizon[which(country_summary_horizon$gavi_2018==TRUE),], year=FALSE, setting="global")

write.csv(country_summary_horizon, paste("output/", folder_name, "/country_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(cluster_summary_horizon, paste("output/", folder_name, "/cluster_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(global_summary_horizon, paste("output/", folder_name, "/global_stats_horizon.csv", sep=""), row.names=FALSE)
write.csv(gavi2018_summary_horizon, paste("output/", folder_name, "/gavi2018_stats_horizon.csv", sep=""), row.names=FALSE)
