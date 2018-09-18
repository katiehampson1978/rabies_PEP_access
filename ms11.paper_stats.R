################################################################################
#                     Report results in the paper                              #
################################################################################

rm(list=ls())

source("R/figures_for_paper_functions_Github.R")
source("R/prob_rabies.R")
options(scipen = 999)

# Undiscounted burden by year
out_countries <- read.csv("output/countryLTs_nodiscount/country_stats.csv", stringsAsFactors=FALSE)
out_cluster <- read.csv("output/countryLTs_nodiscount/cluster_stats.csv", stringsAsFactors=FALSE)
out_global <- read.csv("output/countryLTs_nodiscount/global_stats.csv", stringsAsFactors=FALSE)
out_gavi <- read.csv("output/countryLTs_nodiscount/gavi2018_stats.csv", stringsAsFactors=FALSE)

# Undiscounted burden over time horizon
out_coun <- read.csv("output/countryLTs_nodiscount/country_stats_horizon.csv", stringsAsFactors=FALSE)
out_clus <- read.csv("output/countryLTs_nodiscount/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
out_glo <- read.csv("output/countryLTs_nodiscount/global_stats_horizon.csv", stringsAsFactors=FALSE)
out_gav <- read.csv("output/countryLTs_nodiscount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Discounted burden by year
disc_countries <- read.csv("output/countryLTs_discount/country_stats.csv", stringsAsFactors=FALSE)
disc_cluster <- read.csv("output/countryLTs_discount/cluster_stats.csv", stringsAsFactors=FALSE)
disc_global <- read.csv("output/countryLTs_discount/global_stats.csv", stringsAsFactors=FALSE)
disc_gavi <- read.csv("output/countryLTs_discount/gavi2018_stats.csv", stringsAsFactors=FALSE)

# Discounted burden over time horizon
disc_coun <- read.csv("output/countryLTs_discount/country_stats_horizon.csv", stringsAsFactors=FALSE)
disc_clus <- read.csv("output/countryLTs_discount/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
disc_glo <- read.csv("output/countryLTs_discount/global_stats_horizon.csv", stringsAsFactors=FALSE)
disc_gav <- read.csv("output/countryLTs_discount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Undiscounted burden over time horizon with WHO LE
WHO_coun <- read.csv("output/WHO_LT_nodiscount/country_stats_horizon.csv", stringsAsFactors=FALSE)
WHO_clus <- read.csv("output/WHO_LT_nodiscount/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
WHO_glo <- read.csv("output/WHO_LT_nodiscount/global_stats_horizon.csv", stringsAsFactors=FALSE)
WHO_gav <- read.csv("output/WHO_LT_nodiscount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Discounted burden over time horizon with WHO LE
discWHO_coun <- read.csv("output/WHO_LT_discount/country_stats_horizon.csv", stringsAsFactors=FALSE)
discWHO_clus <- read.csv("output/WHO_LT_discount/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
discWHO_glo <- read.csv("output/WHO_LT_discount/global_stats_horizon.csv", stringsAsFactors=FALSE)
discWHO_gav <- read.csv("output/WHO_LT_discount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Discounted burden over time horizon for max costs
maxdisc_coun <- read.csv("output/countryLTs_discount_maxcosts/country_stats_horizon.csv", stringsAsFactors=FALSE)
maxdisc_clus <- read.csv("output/countryLTs_discount_maxcosts/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
maxdisc_glo <- read.csv("output/countryLTs_discount_maxcosts/global_stats_horizon.csv", stringsAsFactors=FALSE)
maxdisc_gav <- read.csv("output/countryLTs_discount_maxcosts/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Discounted burden over time horizon for MIN costs
mindisc_coun <- read.csv("output/countryLTs_discount_mincosts/country_stats_horizon.csv", stringsAsFactors=FALSE)
mindisc_clus <- read.csv("output/countryLTs_discount_mincosts/cluster_stats_horizon.csv", stringsAsFactors=FALSE)
mindisc_glo <- read.csv("output/countryLTs_discount_mincosts/global_stats_horizon.csv", stringsAsFactors=FALSE)
mindisc_gav <- read.csv("output/countryLTs_discount_mincosts/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)

# Discounted ICER run
ICER_glo <- read.csv("output/countryLTs_discount/global_stats_horizon_ICER.csv", stringsAsFactors=FALSE)

# Undiscounted ICER run with minium RIG cost
minRIG_clus <- read.csv("output/countryLTs_nodiscount/cluster_stats_horizon_minRIG_ICER.csv", stringsAsFactors=FALSE)
minRIG_glo <- read.csv("output/countryLTs_nodiscount/global_stats_horizon_minRIG_ICER.csv", stringsAsFactors=FALSE)
out_det <- read.csv("output/countryLTs_nodiscount/global_stats_horizon_det.csv", stringsAsFactors=FALSE)

gavi_info <- read.csv("output/gavi_output_data.csv", stringsAsFactors=FALSE)
length(which(gavi_info$gavi_2018=="TRUE")) # 46 Gavi-eligible countries
yr = 2020:2035

bio_params <- read.csv("output/bio_data.csv", stringsAsFactors = FALSE)
cost_info <- read.csv("data/gavi_PEP_policy.csv", stringsAsFactors = FALSE)

##########################
# 1. Research in context #
##########################

paste0("Additional deaths averted (SC2-SC1): ", format(round(sum(out_countries$total_deaths_averted[which(out_countries$scenario=="a3_1")])-sum(out_countries$total_deaths_averted[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","))

###############
# 2. Abstract #
###############

paste0("Total deaths for 67 countries: ", format(round(sum(out_countries$total_deaths[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","))
paste0("Number of countries: ", length(unique(out_countries$country)))
paste0("Total deaths averted annually (SC1): ", format(round(sum(out_countries$total_deaths_averted[which(out_countries$scenario=="a1")])/16, digits=-3), big.mark=","))
paste0("Additional deaths averted (SC2-SC1): ", format(round(sum(out_countries$total_deaths_averted[which(out_countries$scenario=="a3_1")])-sum(out_countries$total_deaths_averted[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","))

paste0("Number of vials pre-switch to efficient ID: ", format(round(out_glo$total_vials[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Number of vials post-switch to efficient ID: ", format(round(out_glo$total_vials[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))
paste0("Number of additional persons treated: ", format(round(out_glo$vaccinated[which(out_glo$scenario=="a3_1")]-out_glo$vaccinated[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))

paste0("Cost per death averted (SC2): $", round(disc_glo$cost_per_death_averted[which(disc_glo$scenario=="a3_1")], digits=0))
paste0("Cost per DALY averted (SC2): $", round(disc_glo$cost_per_YLL_averted[which(disc_glo$scenario=="a3_1")], digits=0))

min(out_global$total_deaths); min(out_global$total_deaths_lci);
yrs <- out_global$year[which(out_global$scenario=="a2" & out_global$total_deaths==0)]
yrs <- out_global$year[which(out_global$scenario=="a2" & out_global$total_deaths_lci==0)]
paste("First year in which human Rabies could be eliminated by scaling up dog vaccinations (SC4a): ", yrs[1])

yrs <- out_global$year[which(out_global$scenario=="a5_1" & out_global$total_deaths==0)]
yrs <- out_global$year[which(out_global$scenario=="a2" & out_global$total_deaths_lci==0)]
paste("First year in which human Rabies could be eliminated by scaling up dog vaccinations & improving PEP access (SC4b): ", yrs[1])

##############
# 3. Methods #
##############

# Model parameters:
paste0("P_infect: ", round(bio_params$p_rabies_transmission, digits=3))
quantile(mixture_model(1000), c(0.025, 0.5, 0.975))
Hmisc::binconf(bio_params$p_prevent_given_complete_n, n=bio_params$p_prevent_given_complete_n)

paste0("Number of countries: ", length(unique(out_countries$country)))
paste0("Number of countries Gavi-eligible in 2018: ", length(unique(out_countries$country[which(out_countries$gavi_2018==TRUE)])))

##############
# 4. Results #
##############

#########################################
# Paragraph 1: Estimates of rabies burden
paste0("Total deaths for 67 countries: ", format(round(sum(out_countries$total_deaths[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","))
paste0("95% Prediction Intervals (PrI): ", format(round(out_glo$total_deaths_lci[which(out_glo$scenario=="a1")], digits=-3), big.mark=","),
      "-", format(round(out_glo$total_deaths_uci[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Total deaths for 46 gavi eligible 2018 countries: ", format(round(sum(out_gavi$total_deaths[which(out_gavi$scenario=="a1")]), digits=-3), big.mark=","))
paste0("Rabies deaths per year: ", format(round(sum(out_countries$total_deaths[which(out_countries$scenario=="a1")])/length(yr), digits=-3), big.mark=","))
paste0("U5 deaths for 67 countries: ", round((sum(out_countries$U5_deaths[which(out_countries$scenario=="a1")])/sum(out_countries$total_deaths[which(out_countries$scenario=="a1")]))*100, digits=0), "%, ",
       format(round(sum(out_countries$U5_deaths[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","), "/",
       format(round(sum(out_countries$total_deaths[which(out_countries$scenario=="a1")]), digits=-3), big.mark=","))
paste0("95% PrI: ", format(round(out_glo$U5_deaths_lci[which(out_glo$scenario=="a1")], digits=-2), big.mark=","),
       "-", format(round(out_glo$U5_deaths_uci[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Total deaths in E Africa: ", format(round(sum(out_cluster$total_deaths[which(out_cluster$scenario=="a1" & out_cluster$cluster=="east africa")]), digits=-3), big.mark=","))
paste0("Total deaths in W Africa: ", format(round(sum(out_cluster$total_deaths[which(out_cluster$scenario=="a1" & out_cluster$cluster=="west africa")]), digits=-3), big.mark=","))
paste0("Total deaths in Asia: ", format(round(sum(out_cluster$total_deaths[which(out_cluster$scenario=="a1" & out_cluster$cluster=="asia")]), digits=-3), big.mark=","))
paste0("Total deaths in Americas: ", format(round(sum(out_cluster$total_deaths[which(out_cluster$scenario=="a1" & out_cluster$cluster=="americas")]), digits=-3), big.mark=","))
paste0("Deaths averted annually (SC1): ", format(round(out_glo$total_deaths_averted[which(out_glo$scenario=="a1")]/length(yr), digits=-3), big.mark=","))
paste0("DALYs averted annually (SC1): ", format(round(out_glo$total_YLL_averted[which(out_glo$scenario=="a1")]/length(yr), digits=-3), big.mark=","))

#######################################
# Paragraph 2: Deaths and DALYs averted
paste0("Deaths in 2035 under SC1 (", format(round(out_global$total_deaths[which(out_global$scenario=="a1" & out_global$year==2035)], digits=-3), big.mark=","),
       ") are reduced to ", format(round(out_global$total_deaths[which(out_global$scenario=="a3_1" & out_global$year==2035)], digits=-3), big.mark=","), " under SC2")

paste0("DALYs in 2035 under SC1 (", format(round(out_global$total_YLL[which(out_global$scenario=="a1" & out_global$year==2035)], digits=-3), big.mark=","),
       ") are reduced to ", format(round(out_global$total_YLL[which(out_global$scenario=="a3_1" & out_global$year==2035)], digits=-3), big.mark=","), " under SC2")

paste0("Total Deaths averted (SC2): ", format(round(out_glo$total_deaths_averted[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))
paste0("Total DALYs averted (SC2): ", format(round(out_glo$total_YLL_averted[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))

###################################
# Paragraph 3: Vaccine requirements
paste0("Persons initiating PEP 2020-2035 (SC1): ", format(round(out_glo$vaccinated[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Persons initiating PEP 2020-2035 (SC2): ", format(round(out_glo$vaccinated[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))
paste0("Total vaccine vials required 2020-2035 (SC1): ", format(round(out_glo$total_vials[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Total vaccine vials required 2020-2035 (SC2): ", format(round(out_glo$total_vials[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))

paste0("Total cost of PEP 2020-2035 (SC1): $", format(round(out_glo$total_cost[which(out_glo$scenario=="a1")], digits=-5), big.mark=","))
paste0("Total cost of PEP 2020-2035 (SC2): $", format(round(out_glo$total_cost[which(out_glo$scenario=="a3_1")], digits=-5), big.mark=","))
paste0("Annual costs of PEP for 67 countries (SC2): $", format(round(out_glo$total_cost[which(out_glo$scenario=="a3_1")]/16, digits=-3), big.mark=","))
paste0("Annual costs of PEP for 2018 Gavi-eligible countries (SC2): $", format(round(out_gav$total_cost[which(out_gav$scenario=="a3_1")]/16, digits=-3), big.mark=","))

# Patients pay for vaccine:
gavi_67 <- sort(unique(out_coun$country))
cost_info <- as.data.frame(lapply(cost_info,function(x) gsub("C?te d'Ivoire","Ivory Coast", x)))
paste0("Countries where patients pay for vaccine: ", length(which(cost_info$Country %in% gavi_67)) - length(which(cost_info$PEP=="free")))

# Costs with Gavi support
sc1_costs = sum(out_countries$total_cost[which(out_countries$scenario=="a1")])
sc2_costs = sum(out_countries$total_cost[which(out_countries$scenario=="a3_1")])

# GAVI support = $500,000 x all countries supported, plus vaccines from year of interest
sc2_gavi =sum(out_countries$total_cost[which(out_countries$scenario=="a3_1" & out_countries$gavi_support=="2")]) # with Gavi support
sc2_pre_gavi = sum(out_countries$total_cost[which(out_countries$scenario=="a3_1" & out_countries$gavi_support=="1")]) # without Gavi support
sc2_gavi + sc2_pre_gavi; sc2_costs # Check: - should equal sc2_gavi
sc2_gavi_2018 =sum(out_countries$total_cost[which(out_countries$scenario=="a3_1" & out_countries$gavi_support=="2" & out_countries$gavi_2018==TRUE)]) # with Gavi support for th 2018 Gavi-eligible counntries

paste0("Gavi Investment for all 67 countries: $", format(round(sc2_gavi, digits=-5), big.mark=","))
paste0("Gavi Investment for 46 countries: $", format(round(sc2_gavi_2018, digits=-5), big.mark=","))

###########################################
# Paragraph 4: Cost per death/ DALY averted
paste0("Cost per death averted (SC1): $", round(disc_glo$cost_per_death_averted[which(disc_glo$scenario=="a1")], digits=0))
paste0("Cost per DALY averted (SC1): $", round(disc_glo$cost_per_YLL_averted[which(disc_glo$scenario=="a1")], digits=0))
paste0("Cost per death averted for 67 countries (SC2): $", round(disc_glo$cost_per_death_averted[which(disc_glo$scenario=="a3_1")], digits=0))
paste0("Cost per DALY averted for 67 countries (SC2): $", round(disc_glo$cost_per_YLL_averted[which(disc_glo$scenario=="a3_1")], digits=0))
paste0("Cost per death averted for gavi 2018 countries (SC2): $", round(disc_gav$cost_per_death_averted[which(disc_gav$scenario=="a3_1")], digits=0))
paste0("Cost per DALY averted for gavi 2018 countries (SC2): $", round(disc_gav$cost_per_YLL_averted[which(disc_gav$scenario=="a3_1")], digits=0))
paste0("Cost per death averted for E Africa (SC2): $", round(disc_clus$cost_per_death_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="east africa")], digits=0))
paste0("Cost per DALY averted for E Africa (SC2): $", round(disc_clus$cost_per_YLL_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="east africa")], digits=0))
paste0("Cost per death averted for W Africa (SC2): $", round(disc_clus$cost_per_death_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="west africa")], digits=0))
paste0("Cost per DALY averted for W Africa (SC2): $", round(disc_clus$cost_per_YLL_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="west africa")], digits=0))
paste0("Cost per death averted for Asia (SC2): $", round(disc_clus$cost_per_death_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="asia")], digits=0))
paste0("Cost per DALY averted for Asia (SC2): $", round(disc_clus$cost_per_YLL_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="asia")], digits=0))
paste0("Cost per death averted for Americas (SC2): $", round(disc_clus$cost_per_death_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="americas")], digits=0))
paste0("Cost per DALY averted for Americas (SC2): $", round(disc_clus$cost_per_YLL_averted[which(disc_clus$scenario=="a3_1" & disc_clus$cluster=="americas")], digits=0))

# CALCULATE ICER OF SC1 VS. SC2 WITH 1000 RUNS (DISCOUNTED)
PEP_cost = round(ICER_glo$total_cost[which(ICER_glo$scenario=="a3_1")]-ICER_glo$total_cost[which(ICER_glo$scenario=="a1")], digits=-3)
PEP_cost_lci = round(ICER_glo$total_cost_lci[which(ICER_glo$scenario=="a3_1")]-ICER_glo$total_cost_lci[which(ICER_glo$scenario=="a1")], digits=-3)
PEP_cost_uci = round(ICER_glo$total_cost_uci[which(ICER_glo$scenario=="a3_1")]-ICER_glo$total_cost_uci[which(ICER_glo$scenario=="a1")], digits=-3)
PEP_deaths_diff = ICER_glo$total_deaths[which(ICER_glo$scenario=="a3_1")] - ICER_glo$total_deaths[which(ICER_glo$scenario=="a1")]
PEP_deaths_averted_diff = ICER_glo$total_deaths_averted[which(ICER_glo$scenario=="a3_1")] - ICER_glo$total_deaths_averted[which(ICER_glo$scenario=="a1")]
paste0("Additional deaths averted (SC2-SC1): ", format(round(PEP_deaths_averted_diff, digits=0), big.mark=","))
paste0("Additional cost of PEP (SC2-SC1): ", format(round(PEP_cost, digits=-5), big.mark=","))
paste0("ICER of PEP (SC2-SC1): $-", format(round(PEP_cost/PEP_deaths_diff, digits=0), big.mark=","), "/death averted")

PEP_DALYs_diff = ICER_glo$total_YLL[which(ICER_glo$scenario=="a3_1")] - ICER_glo$total_YLL_lci[which(ICER_glo$scenario=="a1")]
paste0("ICER of PEP (SC2-SC1): $-", format(round(PEP_cost/PEP_DALYs_diff, digits=0), big.mark=","), "/DALY averted")

# CALCULATING ICER OF RIG: SC2 VS. SC3 WITH 1000 RUNS (DISCOUNTED)
# ICER_glo - discounted
RIG_cost = round(ICER_glo$total_cost[which(ICER_glo$scenario=="a4")]-ICER_glo$total_cost[which(ICER_glo$scenario=="a3_1")], digits=-3); RIG_cost
RIG_cost_lci = round(ICER_glo$total_cost_lci[which(ICER_glo$scenario=="a4")]-ICER_glo$total_cost_lci[which(ICER_glo$scenario=="a3_1")], digits=-3)
RIG_cost_uci = round(ICER_glo$total_cost_uci[which(ICER_glo$scenario=="a4")]-ICER_glo$total_cost_uci[which(ICER_glo$scenario=="a3_1")], digits=-3)
paste0("Additional cost of RIG (SC3-SC2): ", format(round(RIG_cost, digits=-5), big.mark=","))
paste0("RIG costs 95% PrIs (SC3-SC2): ", format(round(RIG_cost_lci, digits=-5), big.mark=","), "-", format(round(RIG_cost_uci, digits=-5), big.mark=","))

# Compare difference in deaths and deaths averted for RIG v Vaccine only - shoudl be the same!
out_det$total_deaths[which(out_det$scenario=="a1")] # Status Quo deaths
RIG_deaths_diff_det = out_det$total_deaths[which(out_det$scenario=="a3_1")] - out_det$total_deaths[which(out_det$scenario=="a4")]
RIG_deaths_diff_det*100/out_det$total_deaths[which(out_det$scenario=="a3_1")] # vaccine
RIG_deaths_diff = ICER_glo$total_deaths[which(ICER_glo$scenario=="a3_1")] - ICER_glo$total_deaths[which(ICER_glo$scenario=="a4")]
RIG_deaths_averted_diff_det = out_det$total_deaths_averted[which(out_det$scenario=="a4")] - out_det$total_deaths_averted[which(out_det$scenario=="a3_1")]
RIG_deaths_averted_diff = ICER_glo$total_deaths_averted[which(ICER_glo$scenario=="a4")] - ICER_glo$total_deaths_averted[which(ICER_glo$scenario=="a3_1")]

paste0("Fewer deaths with RIG (SC3-SC2): ", format(round(RIG_deaths_diff, digits=0), big.mark=","))
paste0("Fewer deaths with RIG (SC3-SC2) DETERMINISTIC: ", format(round(RIG_deaths_diff_det, digits=0), big.mark=","))
paste0("Additional deaths averted (SC3-SC2): ", format(round(RIG_deaths_averted_diff, digits=0), big.mark=","))
paste0("Additional deaths averted (SC3-SC2) DETERMINISTIC: ", format(round(RIG_deaths_averted_diff_det, digits=0), big.mark=","))
paste0("ICER of RIG (deaths; SC3-SC2): ", format(round(RIG_cost/RIG_deaths_diff, digits=-3), big.mark=","))
paste0("ICER of RIG (deaths; SC3-SC2) DETERMINISTIC: ", format(round(RIG_cost/RIG_deaths_diff_det, digits=-3), big.mark=","))

############################################
# Paragraph 5: Incorporating dog vaccination
paste0("Total deaths for 67 countries (SC4a): ", format(round(out_glo$total_deaths[which(out_glo$scenario=="a2")], digits=-3), big.mark=","))
paste0("Total DALYs for 67 countries (SC4a): ", format(round(out_glo$total_YLL[which(out_glo$scenario=="a2")], digits=-3), big.mark=","))
paste0("Total deaths for 67 countries (SC4b): ", format(round(out_glo$total_deaths[which(out_glo$scenario=="a5_1")], digits=-3), big.mark=","))
paste0("Total DALYs for 67 countries (SC4b): ", format(round(out_glo$total_YLL[which(out_glo$scenario=="a5_1")], digits=-3), big.mark=","))
paste0("Demand for vaccine (SC4b): ", format(round(out_glo$total_vials[which(out_glo$scenario=="a5_1")], digits=-5), big.mark=","))

paste0("Cost per death averted (SC4bc): $", format(round(out_glo$cost_per_death_averted[which(out_glo$scenario=="a5_1")], digits=0), big.mark=","))
paste0("Cost per DALY averted (SC4bc): $", format(round(out_glo$cost_per_YLL_averted[which(out_glo$scenario=="a5_1")], digits=0), big.mark=","))

paste0("Demand for vaccine (SC4c): ", format(round(out_glo$total_vials[which(out_glo$scenario=="a5_2")], digits=-5), big.mark=","))

###################################
# Paragraph 6: Sensitivity Analyses
paste0("Disc cost per DALY averted (SC2): $", format(round(disc_glo$cost_per_YLL_averted[which(disc_glo$scenario=="a3_1")], digits=0), big.mark=","))
paste0("Disc cost per DALY averted (SC2) with WHO life expectancy: $", format(round(discWHO_glo$cost_per_YLL_averted[which(discWHO_glo$scenario=="a3_1")], digits=0), big.mark=","))

paste0("Disc cost per death averted (SC2) max vacc: $", format(round(maxdisc_glo$cost_per_death_averted[which(maxdisc_glo$scenario=="a3_1")], digits=0), big.mark=","))

# ICER of min RIG
minRIG_cost = round(mindisc_glo$total_cost[which(mindisc_glo$scenario=="a4")]-mindisc_glo$total_cost[which(mindisc_glo$scenario=="a3_1")], digits=-3)
paste0("Additional deaths averted with RIG at 20$ (SC3-SC2) DETERMINISTIC: ", format(round(RIG_deaths_averted_diff_det, digits=-1), big.mark=","))
paste0("ICER of RIG (SC3-SC2) DETERMINISTIC: ", format(round(minRIG_cost/RIG_deaths_averted_diff_det, digits=-1), big.mark=","))

##############
# 4. Discussion #
##############

paste0("Total deaths under SQ over TH: ", format(round(out_glo$total_deaths[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Total Deaths averted (SC2): ", format(round(out_glo$total_deaths_averted[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))
paste0("ADDITIONAL Deaths averted (SC2): ", format(round(out_glo$total_deaths_averted[which(out_glo$scenario=="a3_1")]-out_glo$total_deaths_averted[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
paste0("Total DALYs averted (SC2): ", format(round(out_glo$total_YLL_averted[which(out_glo$scenario=="a3_1")], digits=-3), big.mark=","))
paste0("Cost per death averted (SC2): $", round(disc_glo$cost_per_death_averted[which(disc_glo$scenario=="a3_1")], digits=0))
paste0("Cost per DALY averted (SC2): $", round(disc_glo$cost_per_YLL_averted[which(disc_glo$scenario=="a3_1")], digits=0))

# Use IM vs ID
table(gavi_info$ID, gavi_info$ID_2015) # 55/67

paste0("ADDITIONAL Deaths averted (SC2): ", format(round(out_glo$total_deaths_averted[which(out_glo$scenario=="a3_1")]-out_glo$total_deaths_averted[which(out_glo$scenario=="a1")], digits=-3), big.mark=","))
