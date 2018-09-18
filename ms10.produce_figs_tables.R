################################################################################
#                                                                              #
#                 CREATE FIGURES AND TABLES FOR THE MANUSCRIPT                 #
#                                                                              #
################################################################################

# This script produces all figures and tables in the manuscript, in the
# order they appear.

rm(list=ls())
options(scipen = 999)

# Load packages - ensure plyr is not used
detach(package:plyr)
library(tools)
library(dplyr)
library(ggplot2)
library(scales)
library(Hmisc)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(shapefiles)
library(raster)
library(SDMTools)
library(adehabitatHR)
library(gridExtra)
library(maps)
library(reshape2)

# Source functions
source("R/multivariate_plot_summarise_data_Github.R")
source("R/univariate_plot_summarise_data_Github.R")
source("R/figures_for_paper_functions_Github.R")
source("R/prob_rabies.R")

################################################################################
#                         1. MAIN FIGURES/TABLES                               #
################################################################################

# Load in data
undisc_global_gavi67 <- read.csv("output/countryLTs_nodiscount/global_stats_horizon.csv", stringsAsFactors=FALSE)
undisc_global_gavi46 <- read.csv("output/countryLTs_nodiscount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)
disc_global_gavi67 <- read.csv("output/countryLTs_discount/global_stats_horizon.csv", stringsAsFactors=FALSE)
disc_global_gavi46 <- read.csv("output/countryLTs_discount/gavi2018_stats_horizon.csv", stringsAsFactors=FALSE)
undisc_global <- read.csv("output/countryLTs_nodiscount/global_stats.csv", stringsAsFactors=FALSE)
undisc_country <- read.csv("output/countryLTs_nodiscount/country_stats.csv", stringsAsFactors=FALSE)
ICER_global_df <- read.csv("output/countryLTs_discount/global_stats_horizon_ICER.csv", stringsAsFactors=FALSE)
ICER_cluster_df <- read.csv("output/countryLTs_discount/cluster_stats_horizon_ICER.csv", stringsAsFactors=FALSE)

##########################
# Figure 2: multivariate #
##########################

# Set CI limits
lci_p = 0.05
uci_p = 0.95

# Run sourced function to stack data
undisc_gavi67 <- summarise_multivariate_data(undisc_global_gavi67, setting="global")
undisc_gavi46 <- summarise_multivariate_data(undisc_global_gavi46, setting="global")
disc_gavi67 <- summarise_multivariate_data(disc_global_gavi67, setting="global")
disc_gavi46 <- summarise_multivariate_data(disc_global_gavi46, setting="global")

# Set colour palette across all scenarios
all_palette <- c("firebrick2", "#E69F00", "#009E73", "deepskyblue1", "deepskyblue3", "deepskyblue4")

# Set vector of chosen metrics - TOTAL DEATHS/DALYS ARE UNDISCOUNTED, COSTS PER xx ARE DISCOUNTED.
undisc_metrics <- c("total_deaths", "total_YLL")
disc_metrics <- c("cost_per_death_averted", "cost_per_YLL_averted")

# GAVI-67 - no discount for deaths/DALYs, discount for cost per death/DALY
nodisc_gavi67 <- undisc_gavi67[which(undisc_gavi67$metric %in% undisc_metrics),]
disc_gavi67 <- disc_gavi67[which(disc_gavi67$metric %in% disc_metrics),]
combined_gavi67 <- rbind(nodisc_gavi67, disc_gavi67)

# GAVI-46 - no discount for deaths/DALYs, discount for cost per death/DALY
undisc_gavi46 <- undisc_gavi46[which(undisc_gavi46$metric %in% undisc_metrics),]
disc_gavi46 <- disc_gavi46[which(disc_gavi46$metric %in% disc_metrics),]
combined_gavi46 <- rbind(undisc_gavi46, disc_gavi46)

# Set factor levels in correct order
combined_gavi67 <- arrange_factor_levels(combined_gavi67, type="multi")
combined_gavi46 <- arrange_factor_levels(combined_gavi46, type="multi")

# Create combined plot for paper
combined_gavi67$Gavi_group <- "Gavi-67"
combined_gavi46$Gavi_group <- "Gavi-46"
combined_gavi <- rbind(combined_gavi67, combined_gavi46)

# Set factor levels of Gavi group
combined_gavi$Gavi_group <- as.factor(combined_gavi$Gavi_group)
combined_gavi <- arrange(transform(combined_gavi,
                                   Gavi_group=factor(Gavi_group, levels=c("Gavi-67", "Gavi-46"))), Gavi_group)

# Transform DALYs
combined_gavi$m[combined_gavi$metric=="A) Total DALYs (x1000)"] <- combined_gavi$m[combined_gavi$metric=="A) Total DALYs (x1000)"]/1000
combined_gavi$lci[combined_gavi$metric=="A) Total DALYs (x1000)"] <- combined_gavi$lci[combined_gavi$metric=="A) Total DALYs (x1000)"]/1000
combined_gavi$uci[combined_gavi$metric=="A) Total DALYs (x1000)"] <- combined_gavi$uci[combined_gavi$metric=="A) Total DALYs (x1000)"]/1000
combined_gavi$group_var <- 1:nrow(combined_gavi)

# Create plot
pdf("figs/paper/multivariate.pdf", width=12, height=11)
ggplot(data=combined_gavi, aes(x=scenario, y=m, group=group_var)) +
  theme_classic() +
  geom_point(aes(colour=scenario), position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=lci, ymax=uci, colour=scenario, lty=Gavi_group, width=0.2), position=position_dodge(0.3)) +
  facet_wrap( ~ metric, scales="free_y") +
  labs(colour="Scenario",y="Value",x="") +
  theme(plot.title = element_text(size=14,face="bold",hjust = 0.5)) +
  theme(legend.title=element_blank()) + theme_classic(base_size = 18) +
  scale_colour_manual(values=all_palette, breaks=c("1", "2", "3", "4a", "4b", "4c"),
                      labels=c("Status Quo", "Vaccine", "Vaccine + RIG", "Canine Vacc Status Quo", "Vaccine + Canine Vacc", "Vaccine + Canine Vacc + IBCM")) +
  scale_linetype_manual(name="", breaks=c("Gavi-67", "Gavi-46"), values=c("solid", "dotted")) +
  scale_y_continuous(labels=comma, limits=c(0,NA)) +
  scale_x_discrete(breaks=c("1","2","3","4a","4b","4c"),
                   labels=c("Scenario 1 \n Status Quo", "Scenario 2 \n Improved PEP access", "Scenario 3 \n S2 + Provision of RIG",
                            "Scenario 4a Dog vax \n Status Quo", "Scenario 4b Dog vax \n Improved PEP access", "Scenario 4c Dog vax \n S4b + IBCM")) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, size=8), legend.position="top") + guides(color=FALSE)
dev.off()

################################
# Table 2: Summary by scenario #
################################

# Transform scenario names
global_hor <- scenario_prep(undisc_global_gavi67)

# Reduce numbers to millions
global_hor[,-c(1, 38:46)] <- global_hor[,-c(1, 38:46)]/1000000

# Round all numbers to 2 dp
global_hor[,-1] <- format(signif(global_hor[,-1], 3), nsmall=0, big.mark=",", trim=TRUE)

# Build dataframe
global_sum_df <- data.frame("Scenario"=global_hor$scenario,
                            "Rabies deaths"=paste0(global_hor$total_deaths, " (", global_hor$total_deaths_lci, "-", global_hor$total_deaths_uci, ")"),
                            "Rabies deaths averted"=paste0(global_hor$total_deaths_averted, " (", global_hor$total_deaths_averted_lci, "-", global_hor$total_deaths_averted_uci, ")"),
                            "DALYs"=paste0(global_hor$total_YLL, " (", global_hor$total_YLL_lci, "-", global_hor$total_YLL_uci, ")"),
                            "DALYs averted"=paste0(global_hor$total_YLL_averted, " (", global_hor$total_YLL_averted_lci, "-", global_hor$total_YLL_averted_uci, ")"),
                            "Vaccine vials used"=paste0(global_hor$total_vials, " (", global_hor$total_vials_lci, "-", global_hor$total_vials_uci, ")"),
                            "RIG vials used"=paste0(global_hor$RIG, " (", global_hor$RIG_lci, "-", global_hor$RIG_uci, ")"),
                            "PEP courses initiated"=paste0(global_hor$vaccinated, " (", global_hor$vaccinated_lci, "-", global_hor$vaccinated_uci, ")"),
                            "PEP courses completed"=paste0(global_hor$fully_vaccinated, " (", global_hor$fully_vaccinated_lci, "-", global_hor$fully_vaccinated_uci, ")"),
                            "Total Cost (USD)"=paste0(global_hor$total_cost, " (", global_hor$total_cost_lci, "-", global_hor$total_cost_uci, ")"))
colnames(global_sum_df) <- c("Scenario", "Rabies deaths", "Rabies deaths averted", "DALYs",
                             "DALYs averted", "Vaccine vials used", "RIG vials used",
                             "PEP courses initiated", "PEP courses completed", "Total Cost  (USD)")

final_global_sum_df <- as.data.frame(t(global_sum_df))
colnames(final_global_sum_df) <- global_sum_df$Scenario

# Order columns by colname
final_global_sum_df <- final_global_sum_df[c("1", "2", "a3_2", "a3_3", "3", "4a", "4b", "4c")]
final_global_sum_df <- final_global_sum_df[-1,]

colnames(final_global_sum_df) <- c("Scenario 1", "Scenario 2 base", "Scenario 2 low", "Scenario 2 high", "Scenario 3", "Scenario 4a", "Scenario 4b", "Scenario 4c")

# Create main table for manuscript
model_summary_df <- final_global_sum_df[-6,-c(5:8)]

# Save to .csv
write.csv(model_summary_df, "output/paper/model_summary_scenario.csv")

################################
# Figure 3: Projected Outcomes #
################################

# Select countries for highlight
chosen_countries <- c("Bangladesh", "Ethiopia", "Kenya", "Myanmar")
country_df <- undisc_country[which(undisc_country$country %in% chosen_countries),]

# Create dataframe for plot
plot_df <- projected_outcomes_plot_prep(global_df=undisc_global, country_df=country_df)

# Transform all variables
plot_df$adj_mean <- plot_df$mean/1000
plot_df$adj_lci <- plot_df$lci/1000
plot_df$adj_uci <- plot_df$uci/1000

# Only keep necessary scenarios
scenarios_to_keep <- c("1", "2", "4b")
plot_df <- scenario_prep(plot_df)
plot_df <- plot_df[which(plot_df$scenario %in% scenarios_to_keep),]

# Set the palette colours
palette <- c("firebrick2", "#E69F00", "#0072B2")

# Set labels for plot
facet_labels <- c(`All countries-Deaths`="", `All countries-Persons vaccinated`="",
                  `All countries-Vaccine vials`="All countries",
                  `Bangladesh-Deaths`="", `Bangladesh-Persons vaccinated`="",
                  `Bangladesh-Vaccine vials`="Bangladesh",
                  `Ethiopia-Deaths`="", `Ethiopia-Persons vaccinated`="",
                  `Ethiopia-Vaccine vials`="Ethiopia",
                  `India-Deaths`="", `India-Persons vaccinated`="",
                  `India-Vaccine vials`="India", `Kenya-Deaths`="",
                  `Kenya-Persons vaccinated`="", `Kenya-Vaccine vials`="Kenya",
                  `Myanmar-Deaths`="", `Myanmar-Persons vaccinated`="", `Myanmar-Vaccine vials`="Myanmar")

pdf("figs/paper/deaths_vaccinated_vials.pdf", width=10, height=13)
# L-R: Deaths, Persons vaccinated, Vials; text added above columns in PDF document.
ggplot(data=plot_df) + geom_line(aes(x=year, y=adj_mean, col=scenario), lwd=1) +
  facet_wrap(~text, ncol=3, scales="free_y", strip.position ="right", labeller = as_labeller(facet_labels)) +
  theme_bw(base_size = 14) +
  labs(y="Values (x1000)", x="Year") +
  scale_y_continuous(limits = c(0,NA)) +
  geom_ribbon(aes(x=year, ymin=adj_lci, ymax=adj_uci, fill=scenario), alpha=0.15) +
  scale_colour_manual(values=palette, breaks=c("1", "2", "4b"),
                      labels=c("S1 Status Quo", "S2 Improved PEP access", "S4b Dog vax + Improved PEP access")) +
  scale_fill_manual(values=palette, breaks=c("1", "2", "4b"),
                    labels=c("S1 Status Quo", "S2 Improved PEP access", "S4b Dog vax + Improved PEP access")) +
  theme(legend.title=element_blank(),
        legend.position="top",
        strip.background = element_blank(),
        strip.text.y = element_text(size=13),
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))
dev.off()

###############################################
# Figure 4: Incremental analysis of scenarios #
###############################################

# Transform scenario names
ICER_global_df <- scenario_prep(ICER_global_df)
ICER_cluster_df <- scenario_prep(ICER_cluster_df)

# SCenario 4b vs Scenario 4a
SC4a_SC4b_SC4c <- stacked_incremental_cost_analysis(ICER_global_df, ICER_cluster_df, scenario_1="4a",
                                                    scenario_2="4b", scenario_3="4c")

# Change cost to be in millions (USD)
SC4a_SC4b_SC4c$cost_diff <- SC4a_SC4b_SC4c$cost_diff/1000000

# Store the min and max values
min(SC4a_SC4b_SC4c$cost_diff); max(SC4a_SC4b_SC4c$cost_diff)
yaxis=-500
min(SC4a_SC4b_SC4c$deaths_diff); max(SC4a_SC4b_SC4c$deaths_diff)
xaxis=-90000

palette <- c("black", "firebrick2", "#E69F00", "#009E73", "#0072B2")
SC4a_SC4b_SC4c$Level <- factor(SC4a_SC4b_SC4c$Level, levels=c("all countries", "americas", "asia", "east africa", "west africa"))

# Create plot of ICER analysis
pdf("figs/paper/ICER.pdf", width=10, height=5)
ggplot(data=SC4a_SC4b_SC4c, aes(x=deaths_diff, y=cost_diff)) +
  geom_vline(xintercept = 0, col="gray70") + geom_hline(yintercept = 0, col="gray70") +
  geom_point(aes(col=Level, shape=Scenario), size=3) +
  theme_classic() +
  scale_x_reverse(labels=comma, limits=c(-xaxis, xaxis),
                  breaks=c(90000,45000,0,-45000,-90000)) +
  scale_y_continuous(limits=c(yaxis, -yaxis),
                     breaks=c(-500,-250,-100,0,100,250,500)) +
  labs(x="Difference in number of deaths", y="Difference in cost (USD in millions)") +
  scale_shape_manual(values=c(17,19), breaks=c("4a vs. 4b", "4a vs. 4c"),
                     labels=c("\n 4a SQ + dog vax vs. \n 4b Improved PEP access + dog vax \n",
                              "\n 4a SQ + dog vax vs. \n 4c Improved PEP access + dog vax + IBCM \n")) +
  scale_colour_manual(values=palette, name="Cluster",
                      breaks=c("all countries", "americas", "asia", "east africa", "west africa"),
                      labels=c("All Countries", "Americas", "Asia", "East Africa", "West Africa")) +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))
dev.off()

################################################################################
#                           2. SUPPLEMENTARY FIGURES                           #
################################################################################

# Load data
map.world <- map_data("world")
country_data <- read.csv("output/country_data_GBP.csv", stringsAsFactors=FALSE)
s1_table <- read.csv("data/S1_table.csv", stringsAsFactors = FALSE)
intervention_data <- read.csv("output/country_data_GBP_Intervention.csv", stringsAsFactors=FALSE)
undisc_country <- read.csv("output/countryLTs_nodiscount/country_stats.csv", stringsAsFactors = FALSE)
univar_gavi67 <- read.csv("output/univariate/global_stats_horizon.csv")

############
# SF2: Map #
############

map.world <- map.world[-which(map.world$region=="Antarctica"),]
country_data <- data.frame(country=country_data$country, cluster=country_data$cluster, endemic=country_data$endemic, gavi_2018=country_data$gavi_2018, gavi=country_data$gavi)
map.world$region <- as.character(map.world$region)

# Correct countries are present in the country_data but missing in the world map
country_data$country[which(!country_data$country %in% map.world$region)]
map.world$region[which(map.world$region=="Antigua")] <- "Antigua and Barbuda"
map.world$region[which(map.world$region=="Barbuda")] <- "Antigua and Barbuda"
map.world$region[which(map.world$region=="Brunei")] <- "Brunei Darussalam"
map.world$region[which(map.world$region=="Cape Verde")] <- "Cabo Verde"
map.world$region[which(map.world$region=="Republic of Congo")] <- "Congo"
map.world$region[which(map.world$region=="Czech Republic")] <- "Czechia"
map.world$region[which(map.world$region=="North Korea")] <- "Dem. People's Republic of Korea"
map.world$region[which(map.world$region=="Guinea-Bissau")] <- "Guinea Bissau"
map.world$region[which(map.world$region=="Iran")] <- "Iran (Islamic Republic of)"
map.world$region[which(map.world$region=="Laos")] <- "Lao People's Democratic Republic"
map.world$region[which(map.world$region=="Micronesia")] <- "Micronesia (Fed. States of)"
map.world$region[which(map.world$region=="South Korea")] <- "Republic of Korea"
map.world$region[which(map.world$region=="Moldova")] <- "Republic of Moldova"
map.world$region[which(map.world$region=="Russia")] <- "Russian Federation"
map.world$region[which(map.world$region=="Saint Kitts")] <- "Saint Kitts and Nevis"
map.world$region[which(map.world$region=="Nevis")] <- "Saint Kitts and Nevis"
map.world$region[which(map.world$region=="Saint Vincent")] <- "Saint Vincent and the Grenadines"
map.world$region[which(map.world$region=="Grenadines")] <- "Saint Vincent and the Grenadines"
map.world$region[which(map.world$region=="Palestine")] <- "State of Palestine"
map.world$region[which(map.world$region=="Syria")] <- "Syrian Arab Republic"
map.world$region[which(map.world$region=="Macedonia")] <- "TFYR Macedonia"
map.world$region[which(map.world$region=="Trinidad")] <- "Trinidad and Tobago"
map.world$region[which(map.world$region=="Tobago")] <- "Trinidad and Tobago"
map.world$region[which(map.world$region=="UK")] <- "United Kingdom"
map.world$region[which(map.world$region=="USA")] <- "United States of America"
map.world$region[which(map.world$region=="Venezuela")] <- "Venezuela (Bolivarian Republic of)"

# Merge country_data info in to map world dataframe
map.world.df <- dplyr::left_join(map.world, country_data, by= c("region"="country"))

gavi_elig_df <- map.world.df
gavi_elig_df$alpha <- NA

# Retain countries that are gavi eligible
gavi_2018_eligible <- which(gavi_elig_df$gavi_2018==TRUE & gavi_elig_df$endemic==1)
gavi_elig_df$alpha[gavi_2018_eligible] <- "gavi_2018_eligible"

# Add layer storing the endemic countries that were previously Gavi eligible
previously_eligible <- which(gavi_elig_df$gavi_2018 == FALSE & gavi_elig_df$gavi==TRUE & gavi_elig_df$endemic==1)
gavi_elig_df$alpha[previously_eligible] <- "previously_eligible"

# Remove "endemic" and NA in the cluster name column
table(gavi_elig_df$cluster)
gavi_elig_df$cluster[gavi_elig_df$cluster=="endemic"] <- NA
gavi_elig_df$cluster[gavi_elig_df$cluster=="free"] <- NA
unique(gavi_elig_df$cluster)
gavi_elig_df$cluster <- as.character(gavi_elig_df$cluster)
gavi_elig_df$cluster <- as.factor(gavi_elig_df$cluster)

# Remove NAs in cluster
gavi_elig_df <- gavi_elig_df[which(!is.na(gavi_elig_df$cluster)),]

# Remove NAs in gavi eligiblity
data_map <- gavi_elig_df[which(!is.na(gavi_elig_df$alpha)),];nrow(data_map)
data_map$alpha <- as.factor(data_map$alpha); levels(data_map$alpha)
table(data_map$alpha, useNA="always")

# Check only 67 countries:
length(unique(data_map$region))

# Plot A: countries that are modelled in the study, shaded by gavi eligibility
eligible_palette <- c("firebrick2", "#E69F00", "#009E73", "#0072B2")
pdf("figs/paper/map_gavieligible.pdf", width = 10, height = 5)
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group),
               colour="black", fill="grey95", lwd=0.04) +
  geom_polygon(data = data_map,
               aes(x = long, y = lat, group = group, fill=cluster, colour=cluster, alpha=alpha), lwd=0.1) +
  scale_fill_manual(name = "Cluster", values = eligible_palette,
                    labels=c("Americas", "Asia", "East Africa", "West Africa")) +
  scale_colour_manual(name = "Cluster", values = eligible_palette,
                      labels=c("Americas", "Asia", "East Africa", "West Africa")) +
  scale_alpha_manual(values=c(0.6, 0.3), guide=FALSE) +
  coord_fixed(ratio = 1) +
  geom_text(label="A)", aes(x=-Inf, y=Inf, hjust=-1, vjust=1), size=8) +
  guides(colour = guide_legend(override.aes = list(alpha=0.5))) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour="black"),
        legend.key.size =  unit(0.2, "in"),
        legend.text = element_text(size=8),
        legend.title = element_text(colour="black", size=8, face="bold"),
        plot.title = element_text(hjust = 0.5))
dev.off()

# Find which countries contributed data to the clusters
contributing_countries <- c("Bangladesh", "Bhutan", "Cambodia", "Cameroon", "Central African Republic",
                            "Chad", "Ivory Coast", "Democratic Republic of the Congo", "Ethiopia",
                            "Ghana", "Guinea", "Haiti", "India", "Kenya", "Lao People's Democratic Republic",
                            "Liberia", "Madagascar", "Mali", "Mozambique", "Nepal", "Pakistan", "Philippines",
                            "Senegal", "Sri Lanka", "Tajikistan", "Tanzania", "Thailand", "Uganda", "Vietnam",
                            "Yemen", "Zambia")
contrib_data <- gavi_elig_df[which(gavi_elig_df$region %in% contributing_countries),]
contrib_data$cluster <- as.character(contrib_data$cluster)
contrib_data$cluster <- as.factor(contrib_data$cluster)
levels(contrib_data$cluster)

# Plot B: All countries that contributed data to the study
contrib_palette <- c("firebrick2", "#E69F00", "#009E73", "#0072B2")
pdf("figs/paper/map_datacontributed.pdf", width = 10, height = 5)
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group),
               colour="black", fill="grey95", lwd=0.04) +
  geom_polygon(data = contrib_data,
               aes(x = long, y = lat, group = group, fill=cluster, colour=cluster), alpha=0.6, lwd=0.1) +
  scale_fill_manual(name = "Cluster", values = contrib_palette,
                    labels=c("Americas", "Asia", "East Africa", "West Africa"), guide=FALSE) +
  scale_colour_manual(name = "Cluster", values = contrib_palette,
                      labels=c("Americas", "Asia", "East Africa", "West Africa"), guide=FALSE) +
  coord_fixed(ratio = 1) +
  geom_text(label="B)", aes(x=-Inf, y=Inf, hjust=-1, vjust=1), size=8) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour="black"),
        legend.key.size =  unit(0.2, "in"),
        legend.text = element_text(size=8),
        legend.title = element_text(colour="black", size=8, face="bold"),
        plot.title = element_text(hjust = 0.5))
dev.off()

##################################
# SF3: Comparison of death rates #
##################################

# Subset table
s1_table <- data.frame(CODE = s1_table$CODE, pop_2010 = s1_table$Pop_2010, deaths_2010 = s1_table$Deaths,
                       deaths_lower_2010 = s1_table$deaths.Lower, deaths_upper_2010 = s1_table$deaths.Upper,
                       death_rate_2010 = s1_table$Per.capita.death.rate.from.rabies...100.000.,
                       stringsAsFactors = FALSE)

# Clean up table
s1_table$death_rate_2010[s1_table$death_rate_2010=="#VALUE!"] <- NA
s1_table$deaths_2010 <- as.numeric(gsub(",", "", s1_table$deaths_2010))
s1_table$deaths_lower_2010 <- as.numeric(gsub(",", "", s1_table$deaths_lower_2010))
s1_table$deaths_upper_2010 <- as.numeric(gsub(",", "", s1_table$deaths_upper_2010))
s1_table$death_rate_2010 <- as.numeric(s1_table$death_rate_2010)

# Calculate 2010 lower and upper death rates
s1_table$death_rate_lower_2010 <- (100000/s1_table$pop_2010)*s1_table$deaths_lower_2010
s1_table$death_rate_upper_2010 <- (100000/s1_table$pop_2010)*s1_table$deaths_upper_2010

# Find which rows have no deaths but a death rate
indx <- which(is.na(s1_table$deaths_2010) & !is.na(s1_table$death_rate_2010))
s1_table$deaths_2010[indx] <- (s1_table$death_rate_2010[indx]/100000)*s1_table$pop_2010[indx]

# Merge tables together
s1_table <- merge(s1_table, data.frame(country=country_data$country, CODE = country_data$CODE,
                                       pop_2020 = country_data$pop2020, endemic=country_data$endemic,
                                       gavi = country_data$gavi, gavi_2018 = country_data$gavi_2018),
                  by = "CODE", all.y = TRUE)

# Remove countries without endemic Rabies
s1_table <- s1_table[which(s1_table$endemic==1),]

# Calculate deaths in 2020
s1_table$deaths_2020 <- NA
s1_table$deaths_2020 <- (s1_table$death_rate_2010/100000)*s1_table$pop_2020
s1_table$deaths_lower_2020 <- (s1_table$death_rate_lower_2010/100000)*s1_table$pop_2020
s1_table$deaths_upper_2020 <- (s1_table$death_rate_upper_2010/100000)*s1_table$pop_2020

# Replace Ivory Coast with Cote d'Ivoire
s1_table$country <- as.character(s1_table$country)
s1_table$country[which(s1_table$country=="Ivory Coast")] <- "Cote d'Ivoire"

# Read in model results
country_stats <- undisc_country[which(undisc_country$year==2020 & undisc_country$scenario=="a1"),]
country_stats$country[which(country_stats$country=="Ivory Coast")] <- "Cote d'Ivoire"

# Rearrange table so it is stacked
s1_table_plot_predicted <- data.frame(country=s1_table$country, gavi_2018=s1_table$gavi_2018,
                                      deaths=s1_table$deaths_2020, deaths_lower=s1_table$deaths_lower_2020,
                                      deaths_upper=s1_table$deaths_upper_2020, Type="2010 extrapolation (Hampson et al. 2015)")
s1_table_plot_modelled <- data.frame(country=country_stats$country, gavi_2018=country_stats$gavi_2018,
                                     deaths=country_stats$total_deaths, deaths_lower=country_stats$total_deaths_lci,
                                     deaths_upper=country_stats$total_deaths_uci, Type="Current model")
s1_table_plot <- rbind(s1_table_plot_predicted, s1_table_plot_modelled)

s1_table_plot$group_var <- 1:nrow(s1_table_plot)

# Use only gavi-67 countries
gavi_countries <- s1_table$country[which(s1_table$gavi==TRUE)]
s1_gavi_eligible <- s1_table_plot[which(s1_table_plot$country %in% gavi_countries),]; length(unique(s1_gavi_eligible$country))

# Plot
palette <- c("firebrick2", "#0072B2")
pdf("figs/paper/deaths_comparison.pdf", width=12, height=9)
ggplot(data=s1_gavi_eligible, aes(x=country, group=group_var)) +
  geom_point(aes(y=deaths, colour=Type), position=position_dodge(0.5)) +
  theme_classic() + labs(x="Country", y="Total deaths") +
  geom_errorbar(aes(ymin=deaths_lower, ymax=deaths_upper, colour=Type, width=0.3),
                position=position_dodge(0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.1, size=8),
        panel.grid.major.x = element_line(colour="grey90"),
        axis.text.y  = element_text(angle=90, hjust=0.5, size=10),
        legend.position="top",
        legend.title=element_blank()) +
  scale_y_log10(limits=c(NA,100000), labels=scales::comma) + scale_color_manual(values=palette)
dev.off()

#######################################################
# SF4: Comparison of P|seek, P|receive and P|complete #
#######################################################

# Interventions:
# PEP made free: Chad, Kenya, Tanzania
# Long-term free PEP: Bhutan, Philippines, Cambodia (subsidized), Madagascar (subsidized)
# Haiti was PEP sought/completed after IBCM counselling

# Select only certain columns
intervention_data <- dplyr::select(intervention_data, country, Intervention_Type, p_seek, p_receive, p_complete)

# Change Madagascar intervention status to FALSE
intervention_data$Intervention_Type[which(intervention_data$country=="Madagascar")] <- FALSE

# Subset for only countries with recent interventions
ri <- c("Chad", "Ethiopia", "Haiti", "Kenya", "Tanzania")
rec_interventions <- intervention_data[which(intervention_data$country %in% ri),]
rec_interventions <- melt(rec_interventions, id.vars = c("country", "Intervention_Type"))
rec_interventions$Free <- rec_interventions$Intervention_Type

# Plot A: Countries with interventions to improve PEP access
p <- ggplot() +
  geom_point(data = rec_interventions, aes(y=value, x=variable, shape = country, color=Free))  +
  ylim(0,1) + theme_classic() + labs(x="", y="Value") +
  scale_x_discrete(labels=c(expression(P["seek"]), expression(P["recieve"]), expression(P["complete"]))) +
  scale_shape_manual(name="Country", values=c(19, 17, 15, 3, 21)) +
  geom_text(label="A)", aes(x=-Inf, y=Inf, hjust=-1, vjust=1), size=5)

# Subset for only countries that provide Long term free PEP
lt <- c("Bhutan", "Cambodia", "Madagascar", "Philippines")
lt_free <- intervention_data[which(intervention_data$country %in% lt),]
lt_free$Intervention_Type <- NULL
lt_free = melt(lt_free, id.vars = c("country"))
lt_free$Free = TRUE
lt_free$shape <- lt_free$country

# Subset for only counties where PEP is paid for by patients
patients_pay <- intervention_data[-which(intervention_data$country %in% lt),]
patients_pay <- patients_pay[-which(patients_pay$Intervention_Type==TRUE),]
patients_pay$Intervention_Type <- NULL
patients_pay = melt(patients_pay, id.vars = c("country"))
patients_pay$Free = FALSE
patients_pay = patients_pay[which(!is.na(patients_pay$value)),]
patients_pay$shape <- "Other countries"

# Combine tables
p2_df <- rbind(lt_free, patients_pay)

# Set order of "shape" factor
p2_df$shape <- factor(p2_df$shape, levels=c("Bhutan", "Cambodia", "Madagascar", "Philippines", "Other countries"))

# Plot of countries under status quo - either patients pay or PEP available for free (or heavily subsidized)
p2 <- ggplot() +
  #geom_point(data = patients_pay, aes(y=value, x=variable, color=Free), shape = 21) +
  geom_point(data = p2_df, aes(y=value, x=variable, color=Free, shape = shape)) +
  ylim(0,1) + theme_classic() + labs(x="", y="Value") +
  scale_x_discrete(labels=c(expression(P["seek"]), expression(P["recieve"]), expression(P["complete"]))) +
  scale_shape_manual(name="Country", values=c(19, 17, 15, 3, 21)) +
  geom_text(label="B)", aes(x=-Inf, y=Inf, hjust=-1, vjust=1), size=5)

pdf("figs/paper/health_seeking_assumptions.pdf", width=8, height=4)
grid.arrange(p, p2, nrow = 1)
dev.off()

#############################################################
# SF5: Predictors of incidence of bite patient presentation #
#############################################################

# Created in script ms5.1

############################
# SF6: Univariate analysis #
############################

# Set CI limits
lci_p = 0.05
uci_p = 0.95

# Run function to stack data
univar_gavi67 <- summarise_univar_data(univar_gavi67)

# Adjust appearence of variable names
levels(univar_gavi67$variable)<-c("Non-suspect bite incidence", "P(prevent)", "P(rabid)", "P(transmission)", "Suspect bite incidence")
univar_gavi67$variable <- factor(univar_gavi67$variable, levels = c("Suspect bite incidence","Non-suspect bite incidence","P(rabid)", "P(transmission)", "P(prevent)"))

# Subset to only include chosen metrics
set_metrics <- c("total_deaths", "total_YLL", "cost_per_death_averted", "cost_per_YLL_averted")
univar_gavi67 <- univar_gavi67[which(univar_gavi67$metric %in% set_metrics),]

# Rename metrics and Set order of factor (contained in sourced function)
univar_gavi67 <- arrange_factor_levels(univar_gavi67, type="uni")

# Transform DALYs and Deaths
univar_gavi67$m[univar_gavi67$metric=="A) Total DALYs (x1000)"] <- univar_gavi67$m[univar_gavi67$metric=="A) Total DALYs (x1000)"]/1000
univar_gavi67$lci[univar_gavi67$metric=="A) Total DALYs (x1000)"] <- univar_gavi67$lci[univar_gavi67$metric=="A) Total DALYs (x1000)"]/1000
univar_gavi67$uci[univar_gavi67$metric=="A) Total DALYs (x1000)"] <- univar_gavi67$uci[univar_gavi67$metric=="A) Total DALYs (x1000)"]/1000
univar_gavi67$m[univar_gavi67$metric=="B) Total deaths (x1000)"] <- univar_gavi67$m[univar_gavi67$metric=="B) Total deaths (x1000)"]/1000
univar_gavi67$lci[univar_gavi67$metric=="B) Total deaths (x1000)"] <- univar_gavi67$lci[univar_gavi67$metric=="B) Total deaths (x1000)"]/1000
univar_gavi67$uci[univar_gavi67$metric=="B) Total deaths (x1000)"] <- univar_gavi67$uci[univar_gavi67$metric=="B) Total deaths (x1000)"]/1000

# Set the palette colours
palette <- c("firebrick2", "#E69F00", "#0072B2")

# Save plot to pdf
pdf("figs/paper/univariate.pdf", width=12,height=9)
ggplot(univar_gavi67) +
  geom_point(aes(x=m, y=variable, color=scenario)) +
  geom_errorbarh(aes(x=m, xmin=lci, xmax=uci, y=variable, color=scenario)) +
  facet_grid(scenario~metric, scales="free_x") +
  labs(x="Metric", y="Variable") + theme(legend.position="none") +
  theme(plot.title = element_text(size=14,face="bold",hjust = 0.5)) +
  scale_y_discrete(limits=rev(levels(univar_gavi67$variable)),
                   labels=c(expression(P["prevent"]), expression(P["infect"]),
                            expression(P["rabid"]), "Non-rabid bite incidence",
                            "Rabid bite incidence")) +
  scale_x_continuous(labels=comma) +
  theme_classic(base_size = 14) +
  theme(legend.position="none",
        axis.text.x  = element_text(size=10),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_colour_manual(values=palette, breaks=c("1", "2", "4b"),
                      labels=c("Status Quo", "Vaccine", "Vaccine + Canine Vacc")) +
  scale_fill_manual(values=palette, breaks=c("1", "2", "4b"),
                    labels=c("Status Quo", "Vaccine", "Vaccine + Canine Vacc"))
dev.off()

############################################
# SF7: Time series of dog rabies incidence #
############################################

# Created in script ms5.1

################################################################################
#                           3. SUPPLEMENTARY TABLES                            #
################################################################################

# Load data
country_data <- read.csv("output/country_data_GBP.csv", stringsAsFactors=FALSE)
gavi_policy <- read.csv("data/gavi_PEP_policy.csv", stringsAsFactors=FALSE)
bite_incidence <- read.csv("output/bite_incidence.csv", stringsAsFactors = FALSE)
prepped_country_data <- read.csv("output/prepped_data_final.csv", stringsAsFactors=FALSE)
bio_data <- read.csv("output/bio_data.csv", stringsAsFactors=FALSE)
vaccine_data <- read.csv("data/vaccine_use.csv")
undisc_country_hor <- read.csv("output/countryLTs_nodiscount/country_stats_horizon.csv", stringsAsFactors=FALSE)
disc_country_hor <- read.csv("output/countryLTs_discount/country_stats_horizon.csv", stringsAsFactors=FALSE)
undisc_global_gavi67 <- read.csv("output/countryLTs_nodiscount/global_stats_horizon.csv", stringsAsFactors=FALSE)

##########################################
# ST1: Rabies endemic countries in study #
##########################################

# Merge Gavi information into country_data
joined_df <- merge(country_data, gavi_policy, by.x="country", by.y="Country", all=TRUE)

# Build dataframe
study_countries <- data.frame(Country=joined_df$country,
                            Cluster=joined_df$cluster,
                            Rabies.endemic=joined_df$endemic,
                            Previously.Gavi.eligible=joined_df$gavi,
                            Gavi.eligible.in.2018=joined_df$gavi_2018,
                            Gavi.phase=joined_df$Eligibility.in.2018,
                            GSP.phase=joined_df$Business.Plan.Phase,
                            PEP.policy=NA, stringsAsFactors=FALSE)

# Change appearence of table
study_countries$Rabies.endemic <- ifelse(study_countries$Rabies.endemic==1, "Yes", "")
study_countries$Previously.Gavi.eligible <- ifelse(study_countries$Previously.Gavi.eligible==TRUE, "Yes", "No")
study_countries$Gavi.eligible.in.2018 <- ifelse(study_countries$Gavi.eligible.in.2018==TRUE, "Yes", "No")
cols <- grepl("phase|policy",names(study_countries))
study_countries[cols] <- lapply(study_countries[cols], function(x) replace(x,x %in% NA, "") )

# Remove non-endemic countries
study_countries <- study_countries[which(study_countries$Rabies.endemic=="Yes"),]

# Keep only countries that are 2018 Gavi eligible OR have been Gavi eligible
gavi_eligible <- study_countries$Country[which(study_countries$Gavi.eligible.in.2018=="Yes" |
                                               study_countries$Previously.Gavi.eligible=="Yes")]
final_study_countries <- study_countries[which(study_countries$Country %in% gavi_eligible),]
final_study_countries$Rabies.endemic <- NULL

# Add PEP policy information
free_pep <- prepped_country_data$country[which(prepped_country_data$vaccine_paid_by_patient==FALSE)]
free_pep = c(free_pep, "Bangladesh", "Bhutan", "Madagascar", "Nepal", "Pakistan", "Sri Lanka")
final_study_countries$PEP.policy[which(final_study_countries$Country %in% free_pep)] <- "Free"
patient_pays <- prepped_country_data$country[which(prepped_country_data$vaccine_paid_by_patient==TRUE)]
final_study_countries$PEP.policy[which(final_study_countries$Country %in% patient_pays)] <- "Patient pays"

# Add specific info on subsidized countries
final_study_countries$PEP.policy[which(final_study_countries$Country %in% c("Chad", "Ethiopia"))] <- "Subsidized"
final_study_countries$PEP.policy[which(final_study_countries$Country %in% c("Cambodia", "Madagascar"))] <- "Subsidized*"
final_study_countries$PEP.policy[which(final_study_countries$Country =="India")] <- "subsidized (75%)19"

# Alter Ivory Coast to read as Cote D'Ivoire
final_study_countries$Country[which(final_study_countries$Country=="Ivory Coast")] <- "Cote d'Ivoire"

# Order by country name
final_study_countries <- final_study_countries[order(final_study_countries$Country), ]

# Remove Unecessary columns
final_study_countries$Previously.Gavi.eligible <- NULL

# Save to .csv
write.csv(final_study_countries, "output/paper/countries_in_study.csv", row.names=FALSE)

########################################################
# ST2: Summary of country specific parameters in study #
########################################################

# Only include rows from table which are currently/previously Gavi eligible, or
# contributed data to the study
contributing_countries <- c("Philippines", "Thailand")
countries_to_include <- unique(c(contributing_countries, gavi_eligible)); length(countries_to_include)
params_summary_df <- country_data[which(country_data$country %in% countries_to_include),]

# Select parameter columns
paramscols <- c("bite_inc", "p_rabid", "p_seek", "p_receive", "p_complete",
                "tot_dog_pop", "price_per_vial")

# Set parameter names as they appear in data frame
paramsnames <- c("Bite incidence", "P rabid", "P seek|rabid", "P receive", "P complete",
                 "Total dog population", "Vaccine price per vial")

# params meaning (from datatemplates)
paramsmeaning <- c("Incidence of dog bites per population per year in health facilities",
                   "Probability bite is from a rabid animal",
                   "Probability bite victim will seek health care (PEP)",
                   "Probability bite victim seeking treatment will obtain PEP",
                   "Probability bite victim will complete full PEP course",
                   "Total dog population (country-specific estimates)",
                   "Vaccine vial cost ($)")

ssize <- c(rep(NA, length(paramscols)))
countries_available <- as.character(c(rep(NA, length(paramscols))))

for(i in 1:(length(paramscols)-1)){
  # Set i as i+1 to remove processing of Bite incidence
  j = i+1
  index <- which(complete.cases(params_summary_df[[paramscols[j]]]))
  ssize[j] <- length(index)
  params_countries <- c(params_summary_df$country[index])
  params_countries <- paste(params_countries, collapse = "; ")
  countries_available[j] <- params_countries
}

params_table <- data.frame(paramsnames, paramsmeaning, ssize,countries_available, stringsAsFactors=FALSE)
colnames(params_table) <- c("Parameter", "Parameter Meaning", "Number of countries contributing", "Names of countries contributing")

b_i_index <- which(params_table$Parameter=="Bite incidence")
params_table$`Number of countries contributing`[b_i_index] <- length(unique(bite_incidence$country[which(!is.na(bite_incidence$bite_incidence))]))
params_table$`Names of countries contributing`[b_i_index] <- paste(unique(bite_incidence$country[which(!is.na(bite_incidence$bite_incidence))]), collapse = "; ")

# Save to .csv
write.csv(params_table, "output/paper/parameters_summary.csv", row.names=FALSE)

##########################################################
# ST3: Country specific parameters used in decision tree #
##########################################################

# Select columns
country_params <- data.frame(country_data$country, country_data$Country.code,
                             country_data$Continent, data_source=NA,
                             country_data$endemic, country_data$HDI,
                             format(country_data$total_dogs_Knoble, digits=0, big.mark=","),
                             country_data$cluster, country_data$gavi,
                             country_data$gavi_2018,
                             round(country_data$prop_urban, digits=3),
                             round(prepped_country_data$bite_inc, digits=3),
                             round(country_data$p_rabid, digits=3), country_data$p_rabid_n,
                             round(country_data$p_seek, digits=3), country_data$p_seek_n,
                             round(country_data$p_receive, digits=3), country_data$p_receive_n,
                             round(country_data$p_complete, digits=3), country_data$p_complete_n,
                             country_data$vaccine_paid_by_patient, country_data$RIG_usage,
                             country_data$RIG_paid_by_patient, country_data$ID,
                             round(country_data$cost_first_visit, digits=3),
                             round(country_data$cost_followup_visit, digits=3),
                             round(country_data$price_per_vial, digits=3),
                             round(country_data$price_per_vial_RIG, digits=3), stringsAsFactors=FALSE)

# Fill in Data source column
country_params$data_source[which(country_params$country_data.country=="Bangladesh")] <- "Hossain et al (2012); Hossain et al (2013); CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Bhutan")] <- "Tenzin Tenzin"
country_params$data_source[which(country_params$country_data.country=="Cambodia")] <- "Arnaud Tarantola; Ly et al (2009); CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Cameroon")] <- "Galileu Barbosa Costa; Sofeu et al (in review); CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Central African Republic")] <- "Tricou et al (2016)"
country_params$data_source[which(country_params$country_data.country=="Chad")] <- "Jakob Zinsstag; Ronelngar Moyengar; Assandi Oussiguéré; Anyiam et al (2016); Frey et al (2013); Mbilo et al (2016); CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Ivory Coast")] <- "Jakob Zinsstag; Bassirou Bonfoh; CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Democratic Republic of the Congo")] <- "Twabela et al (2016)"
country_params$data_source[which(country_params$country_data.country=="Ethiopia")] <- "Beyenne et al (in review); CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Ghana")] <- "CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Guinea")] <- "Youla et al (2014)"
country_params$data_source[which(country_params$country_data.country=="Haiti")] <- "Ryan Wallace; Ertheart et al (in review); Schildecker et al (2016); CDC PEP Survey; PAHO RF"
country_params$data_source[which(country_params$country_data.country=="India")] <- "DH Ashwath Narayana; Mysore Kalappa Sudarshan; Fitzpatrick et al (2016); Abbas et al (2011); Sudarshan (2006); Salve (2014); HC data"
country_params$data_source[which(country_params$country_data.country=="Kenya")] <- "Samuel M. Thumbi"
country_params$data_source[which(country_params$country_data.country=="Lao People's Democratic Republic")] <- "Ahmed et al (2015)"
country_params$data_source[which(country_params$country_data.country=="Liberia")] <- "Olarinmoye et al (2017)"
country_params$data_source[which(country_params$country_data.country=="Madagascar")] <- "Malavika Rajeev"
country_params$data_source[which(country_params$country_data.country=="Mali")] <- "Jakob Zinsstag; Abdallah Traoré; CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Mozambique")] <- "Salomão et al (2014)"
country_params$data_source[which(country_params$country_data.country=="Nepal")] <- "Devleesschauwer et al (2016); Buchy CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Pakistan")] <- "Zaidi et al (2013); Salahuddin (2016) "
country_params$data_source[which(country_params$country_data.country=="Philippines")] <- "Sarah Jayme; Valenzuela et al (2017)"
country_params$data_source[which(country_params$country_data.country=="Senegal")] <- "Diop et al (2007)"
country_params$data_source[which(country_params$country_data.country=="Sri Lanka")] <- "Amila Gunasekera; CDC CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Tajikistan")] <- "Picot et al (2017)"
country_params$data_source[which(country_params$country_data.country=="Tanzania")] <- "Katie Hampson; CDC PEP Survey"
country_params$data_source[which(country_params$country_data.country=="Thailand")] <- "Onpirun Yurachai"
country_params$data_source[which(country_params$country_data.country=="Uganda")] <- "Ryan Wallace"
country_params$data_source[which(country_params$country_data.country=="Vietnam")] <- "Huong Nguyen"
country_params$data_source[which(country_params$country_data.country=="Yemen")] <- "Al-Shamahy et al (2013)"
country_params$data_source[which(country_params$country_data.country=="Zambia")] <- "Babaniyi et al (2016)"
country_params$data_source[which(is.na(country_params$data_source))] <- ""

# Set column names
colnames(country_params) <- c("Country", "CODE", "Continent", "Data source", "Rabies endemic",
                              "HDI", "Dog population", "Cluster", "Gavi eligible", "Gavi eligible in 2018",
                              "Proportion of human population living in urban area", "Bite incidence",
                              "P|rabid", "P|rabid sample size", "P|seek", "P|seek sample size",
                              "P|receive", "P|receive sample size", "P|complete", "P|complete sample size",
                              "Is vaccine paid for by patients?", "Is RIG used?", "Is RIG paid for by patients",
                              "Intra-dermal regimen used", "Cost of initial visit", "Cost of follow-up visit",
                              "Price of vaccine per vial", "Price of RIG per vial")

# Alter Ivory Coast to read as Cote D'Ivoire
country_params$Country[which(country_params$Country=="Ivory Coast")] <- "Cote d'Ivoire"

# Order by country name
country_params <- country_params[order(country_params$Country), ]

# Save to .csv
write.csv(country_params, "output/paper/decision_tree_params.csv", row.names=FALSE)

#########################
# ST4: Cluster averages #
#########################

# Summarise parameters by cluster
cluster_data <- country_data %>%
  group_by(cluster) %>%
  summarise(p_seek = round(mean(p_seek, na.rm=T), digits=3),
            p_seek_n = sum(p_seek_n, na.rm=TRUE),
            p_receive = round(mean(p_receive, na.rm=T), digits=3),
            p_receive_n = sum(p_receive_n, na.rm=TRUE),
            p_complete = round(mean(p_complete, na.rm=T), digits=3),
            p_complete_n = sum(p_complete_n, na.rm=TRUE))

# Calculate prices without ethiopia (NTV)
cluster_data_prices <- country_data %>%
  filter(country != "Ethiopia") %>%
  group_by(cluster) %>%
  summarise(price_per_vial = round(mean(price_per_vial, na.rm=T), digits=0),
            price_per_vial_RIG = round(mean(price_per_vial_RIG, na.rm=T), digits=0))

# Merge the 2 datasets
cluster_data <- dplyr::left_join(cluster_data, cluster_data_prices)
cluster_data[is.na(cluster_data)] <- NA # Replace "NaN" with "NA"

# Keep only main clusters
keep_clusters <- c("americas", "asia", "east africa", "west africa")
cluster_data <- cluster_data[which(cluster_data$cluster %in% keep_clusters),]

# Save to .csv
write.csv(cluster_data, "output/paper/cluster_data.csv", row.names=FALSE)

#############################
# ST5: Biological constants #
#############################

# Build dataframe
parameter <- c("P|infect", "P|prevent1", "P|prevent2", "P|prevent3", "P|prevent4")
prob <- c("Developing infection in the absence of PEP", "Complete and timely vaccination prevents rabies*",
          "Incomplete/ late vaccination prevents rabies**", "Complete NTV prevents rabies",
          "Incomplete NTV prevents rabies***")

p_infect_values <- round(quantile(mixture_model(1000), c(0.025, 0.5, 0.975)), digits=3)
p_prevent1 <- round(binconf(bio_data$p_prevent_given_complete_s, n=bio_data$p_prevent_given_complete_n), digits=3)
p_prevent2 <- round(binconf(bio_data$p_prevent_given_imperfect_s, n=bio_data$p_prevent_given_imperfect_n), digits=3)
p_prevent3 <- round(binconf(bio_data$p_prevent_given_complete_NTV_s, n=bio_data$p_prevent_given_complete_NTV_n), digits=3)
p_prevent4 <- round(binconf(bio_data$p_prevent_given_insufficient_NTV_s, n=bio_data$p_prevent_given_insufficient_NTV_n), digits=3)
values <- c(paste0(p_infect_values[2], " (", p_infect_values[1], "-", p_infect_values[3],")"),
            paste0(round(bio_data$p_prevent_given_complete, digits=3), " (", p_prevent1[2], "-", p_prevent1[3], ")"),
            paste0(round(bio_data$p_prevent_given_imperfect, digits=3), " (", p_prevent2[2], "-", p_prevent2[3], ")"),
            paste0(round(bio_data$p_prevent_given_complete_NTV, digits=3), " (", p_prevent3[2], "-", p_prevent3[3], ")"),
            paste0(round(bio_data$p_prevent_given_insufficient_NTV, digits=3), " (", p_prevent4[2], "-", p_prevent4[3], ")"))

sample_size <- c(transmission$n_bite.vics, bio_data$p_prevent_given_complete_n, bio_data$p_prevent_given_imperfect_n,
                 bio_data$p_prevent_given_complete_NTV_n, bio_data$p_prevent_given_insufficient_NTV_n)
rationale <- c(rep("Observational studies [13]", 3), rep("Observational studies [14]", 2))

bio_df <- data.frame(parameter, prob, values, sample_size, rationale)
colnames(bio_df) <- c("Parameter","Probability", "Value (95% CIs)", "n", "Rationale")

# Save to .csv
write.csv(bio_df, "output/paper/bio_constants.csv", row.names=FALSE)

#############################
# ST6: Vial use assumptions #
#############################

# Take only high/low throughput
vaccine_data <- vaccine_data[which(vaccine_data$throughput != "Med"),]

# Take only selected columns
vaccine_data <- dplyr::select(vaccine_data, Setting, regimen, completeness, vial, RIG_wound)

# Alter rural RIG to read as NA
vaccine_data$RIG_wound[which(vaccine_data$RIG_wound==0.92)] <- NA

# Alter setting text appearence
vaccine_data$Setting <- ifelse(vaccine_data$Setting=="rural", "Rural", "Urban")

# Save to .csv
write.csv(vaccine_data, "output/paper/vial_use.csv", row.names=FALSE)

#################################
# ST7: Rabies burden by country #
#################################

# Fix scenario names
undisc_country_hor <- scenario_prep(undisc_country_hor)
disc_country_hor <- scenario_prep(disc_country_hor)

# Subset for only S1 (Status Quo) and S2 (Improved PEP access)
sc1_ud <- undisc_country_hor[which(undisc_country_hor$scenario =="1"),]
sc2_ud <- undisc_country_hor[which(undisc_country_hor$scenario == "2"),]
sc1_di <- disc_country_hor[which(disc_country_hor$scenario == "1"),]
sc2_di <- disc_country_hor[which(disc_country_hor$scenario == "2"),]

# Round all number to 0 dp
sc1_ud[,-c(1:3, 40:41, 48:50)] <- format(round(sc1_ud[,-c(1:3, 40:41, 48:50)], 0), nsmall=0, big.mark=",", trim=TRUE)
sc2_ud[,-c(1:3, 40:41, 48:50)] <- format(round(sc2_ud[,-c(1:3, 40:41, 48:50)], 0), nsmall=0, big.mark=",", trim=TRUE)
sc1_di[,-c(1:3, 40:41, 48:50)] <- format(round(sc1_di[,-c(1:3, 40:41, 48:50)], 0), nsmall=0, big.mark=",", trim=TRUE)
sc2_di[,-c(1:3, 40:41, 48:50)] <- format(round(sc2_di[,-c(1:3, 40:41, 48:50)], 0), nsmall=0, big.mark=",", trim=TRUE)

# Build dataframe
country_sum_df <- data.frame("Country" = unique(undisc_country_hor$country),
                             "SC_1_Deaths" = paste0(sc1_ud$total_deaths, " (", sc1_ud$total_deaths_lci, "-", sc1_ud$total_deaths_uci, ")"),
                             "SC_1_DALYs" = paste0(sc1_ud$total_YLL, " (", sc1_ud$total_YLL_lci, "-", sc1_ud$total_YLL_uci, ")"),
                             "SC_1_Deaths averted" = paste0(sc1_ud$total_deaths_averted, " (", sc1_ud$total_deaths_averted_lci, "-", sc1_ud$total_deaths_averted_uci, ")"),
                             "SC_1_DALYs averted" = paste0(sc1_ud$total_YLL_averted, " (", sc1_ud$total_YLL_averted_lci, "-", sc1_ud$total_YLL_averted_uci, ")"),
                             "SC_1_Vials" = paste0(sc1_ud$total_vials, " (", sc1_ud$total_vials_lci, "-", sc1_ud$total_vials_uci, ")"),
                             "SC_1_Vaccinated" = paste0(sc1_ud$vaccinated, " (", sc1_ud$vaccinated_lci, "-", sc1_ud$vaccinated_uci, ")"),
                             "SC_1_Fully vaccinated" = paste0(sc1_ud$fully_vaccinated, " (", sc1_ud$fully_vaccinated_lci, "-", sc1_ud$fully_vaccinated_uci, ")"),
                             "SC_1_Cost" = paste0(sc1_ud$total_cost, " (", sc1_ud$total_cost_lci, "-", sc1_ud$total_cost_uci, ")"),
                             "SC_1_Cost per death averted" = paste0(sc1_di$cost_per_death_averted, " (", sc1_di$cost_per_death_averted_lci, "-", sc1_di$cost_per_death_averted_uci, ")"),
                             "SC_1_Cost per DALY averted" = paste0(sc1_di$cost_per_YLL_averted, " (", sc1_di$cost_per_YLL_averted_lci, "-", sc1_di$cost_per_YLL_averted_uci, ")"),

                             "SC_2_Deaths" = paste0(sc2_ud$total_deaths, " (", sc2_ud$total_deaths_lci, "-", sc2_ud$total_deaths_uci, ")"),
                             "SC_2_DALYs" = paste0(sc2_ud$total_YLL, " (", sc2_ud$total_YLL_lci, "-", sc2_ud$total_YLL_uci, ")"),
                             "SC_2_Deaths averted" = paste0(sc2_ud$total_deaths_averted, " (", sc2_ud$total_deaths_averted_lci, "-", sc2_ud$total_deaths_averted_uci, ")"),
                             "SC_2_DALYs averted" = paste0(sc2_ud$total_YLL_averted, " (", sc2_ud$total_YLL_averted_lci, "-", sc2_ud$total_YLL_averted_uci, ")"),
                             "SC_2_Vials" = paste0(sc2_ud$total_vials, " (", sc2_ud$total_vials_lci, "-", sc2_ud$total_vials_uci, ")"),
                             "SC_2_Vaccinated" = paste0(sc2_ud$vaccinated, " (", sc2_ud$vaccinated_lci, "-", sc2_ud$vaccinated_uci, ")"),
                             "SC_2_Fully vaccinated" = paste0(sc2_ud$fully_vaccinated, " (", sc2_ud$fully_vaccinated_lci, "-", sc2_ud$fully_vaccinated_uci, ")"),
                             "SC_2_Cost" = paste0(sc2_ud$total_cost, " (", sc2_ud$total_cost_lci, "-", sc2_ud$total_cost_uci, ")"),
                             "SC_2_Cost per death averted" = paste0(sc2_di$cost_per_death_averted, " (", sc2_di$cost_per_death_averted_lci, "-", sc2_di$cost_per_death_averted_uci, ")"),
                             "SC_2_Cost per DALY averted" = paste0(sc2_di$cost_per_YLL_averted, " (", sc2_di$cost_per_YLL_averted_lci, "-", sc2_di$cost_per_YLL_averted_uci, ")"),
                             stringsAsFactors=FALSE)

# Replace Ivory Coast with Cote d'Ivoire
country_sum_df$Country <- as.character(country_sum_df$Country)
country_sum_df$Country[which(country_sum_df$Country=="Ivory Coast")] <- "Cote d'Ivoire"

# Order by country name
country_sum_df <- country_sum_df[order(country_sum_df$Country), ]

# Save to .csv
write.csv(country_sum_df, "output/paper/model_summary_country.csv", row.names=FALSE)

##############################################################
# ST8: Summary of model results across all Gavi-67 countries #
##############################################################

# Transform scenario names
global_hor <- scenario_prep(undisc_global_gavi67)

# Reduce numbers to millions
global_hor[,-c(1, 38:46)] <- global_hor[,-c(1, 38:46)]/1000000

# Round all numbers to 2 dp
global_hor[,-1] <- format(signif(global_hor[,-1], 3), nsmall=0, big.mark=",", trim=TRUE)

# Build dataframe
global_sum_df <- data.frame("Scenario"=global_hor$scenario,
                            "Rabies deaths"=paste0(global_hor$total_deaths, " (", global_hor$total_deaths_lci, "-", global_hor$total_deaths_uci, ")"),
                            "Rabies deaths averted"=paste0(global_hor$total_deaths_averted, " (", global_hor$total_deaths_averted_lci, "-", global_hor$total_deaths_averted_uci, ")"),
                            "DALYs"=paste0(global_hor$total_YLL, " (", global_hor$total_YLL_lci, "-", global_hor$total_YLL_uci, ")"),
                            "DALYs averted"=paste0(global_hor$total_YLL_averted, " (", global_hor$total_YLL_averted_lci, "-", global_hor$total_YLL_averted_uci, ")"),
                            "Vaccine vials used"=paste0(global_hor$total_vials, " (", global_hor$total_vials_lci, "-", global_hor$total_vials_uci, ")"),
                            "RIG vials used"=paste0(global_hor$RIG, " (", global_hor$RIG_lci, "-", global_hor$RIG_uci, ")"),
                            "PEP courses initiated"=paste0(global_hor$vaccinated, " (", global_hor$vaccinated_lci, "-", global_hor$vaccinated_uci, ")"),
                            "PEP courses completed"=paste0(global_hor$fully_vaccinated, " (", global_hor$fully_vaccinated_lci, "-", global_hor$fully_vaccinated_uci, ")"),
                            "Total Cost (USD)"=paste0(global_hor$total_cost, " (", global_hor$total_cost_lci, "-", global_hor$total_cost_uci, ")"))
colnames(global_sum_df) <- c("Scenario", "Rabies deaths", "Rabies deaths averted", "DALYs",
                             "DALYs averted", "Vaccine vials used", "RIG vials used",
                             "PEP courses initiated", "PEP courses completed", "Total Cost  (USD)")

model_summary_df <- as.data.frame(t(global_sum_df))
colnames(model_summary_df) <- global_sum_df$Scenario

# Order columns by colname
model_summary_df <- model_summary_df[c("1", "2", "a3_2", "a3_3", "3", "4a", "4b", "4c")]
model_summary_df <- model_summary_df[-1,]

# Remove SC2 low and high cases
model_summary_df <- model_summary_df[,-c(3:4)]

# Rename columns
colnames(model_summary_df) <- c("Scenario 1 \n Status Quo (95% CIs)", "Scenario 2 \n base case - improved PEP access (95% CIs)",
                                   "Scenario 3 \n S2 + Provision of RIG (95% CIs)", "Scenario 4a \n Status Quo dog vax (95% CIs)",
                                   "Scenario 4b \n Dog vax + improved PEP access (95% CIs)", "Scenario 4c \n S4b + IBCM (95% CIs) ")

# Save to .csv
write.csv(model_summary_df, "output/paper/model_summary_scenario_expanded.csv", row.names=FALSE)
