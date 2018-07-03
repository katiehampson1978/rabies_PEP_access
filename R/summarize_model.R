################################################################################
###                   Display decision tree outputs for review                 #
################################################################################
require(dplyr)
# out <- multivariate
# out <- read.csv("output/multivariate_decision_tree_analysis_GAVI_med_thruput.csv")
out <- read.csv("output/multivariate_decision_tree_analysis_GAVI.csv")

countries <- unique(out$country)
scenarios <- unique(out$scenario)
yrs <- unique(out$year)

# Summarize each country outputs - average across iterations
function(model_runs){}

country_sums <- out %>%
  dplyr::group_by(country, year, scenario, Gavi_support) %>%
  dplyr::summarise(cluster = unique(cluster),
                   total_deaths = mean(total_deaths),
                   total_vials = mean(total_vials),
                   total_cost = mean(total_cost),
                   total_deaths_averted = mean(total_deaths_averted),
                   total_YLL = mean(total_YLL),
                   total_YLL_averted = mean(total_YLL_averted),
                   vaccinated = mean(vaccinated),
                   fully_vaccinated = mean(fully_vaccinated),
                   p_seek_rabid = mean(p_seek_rabid),
                   p_seek_healthy = mean(p_seek_healthy),
                   p_receive = mean(p_receive),
                   p_receive_healthy = mean(p_receive_IBCM),
                   p_receive_RIG = mean(p_receive_RIG),
                   p_complete = mean(p_complete),
                   TargetPopulation_rabid = mean(TargetPopulation_rabid),
                   TargetPopulation_healthy = mean(TargetPopulation_healthy),
                   RIG = mean(RIG)
  )

country_sums$cost_per_death_averted <-  country_sums$total_cost/country_sums$total_deaths_averted
country_sums$cost_per_YLL_averted <-  country_sums$total_cost/country_sums$total_YLL_averted
country_sums$deaths_averted_per_100k_vaccinated <-  country_sums$total_deaths_averted/country_sums$vaccinated/100000

vaccine = (model_output$vaccinated * model_output$prop_urban * vacc$vial[which(vacc$throughput == "High")]) +
  (model_output$vaccinated * (1-model_output$prop_urban) * vacc$vial[which(vacc$throughput == "Low")])
model_output$demand = (vaccine*buffer)/dose # add 1.25 buffer!


# Label data.frame names properly
write.csv(country_sums, "output/country_stats.csv")
models = read.csv("output/country_stats_med_throughput.csv")
models
which(models$country=="Bhutan")

# Summarize each cluster outputs - sum metrics from all countries belonging to same cluster
cluster_sums<- country_sums %>%
  dplyr::group_by(cluster, year, scenario) %>%
  dplyr::summarise(country = unique(cluster),
                   total_deaths = sum(total_deaths),
                   total_vials = sum(total_vials),
                   total_cost = sum(total_cost),
                   total_deaths_averted = sum(total_deaths_averted),
                   total_YLL = sum(total_YLL),
                   total_YLL_averted = sum(total_YLL_averted),
                   vaccinated = sum(vaccinated),
                   fully_vaccinated = sum(fully_vaccinated),
                   p_seek_rabid = mean(p_seek_rabid),
                   p_seek_healthy = mean(p_seek_healthy),
                   p_receive = mean(p_receive),
                   p_receive_healthy = mean(p_receive_healthy),
                   p_receive_RIG = mean(p_receive_RIG),
                   p_complete = mean(p_complete),
                   TargetPopulation_rabid = sum(TargetPopulation_rabid),
                   TargetPopulation_healthy = sum(TargetPopulation_healthy),
                   RIG = sum(RIG))
cluster_sums$cost_per_death_averted <-  cluster_sums$total_cost/cluster_sums$total_deaths_averted
cluster_sums$cost_per_YLL_averted <-  cluster_sums$total_cost/cluster_sums$total_YLL_averted
cluster_sums$deaths_averted_per_100k_vaccinated <-  cluster_sums$total_deaths_averted/cluster_sums$vaccinated/100000

write.csv(cluster_sums, "output/cluster_stats.csv")
# write.csv(cluster_sums, "output/cluster_stats_med_throughput.csv")


# # Summarize global estimates - sum metrics from all countries
global_sums<- cluster_sums %>%
  dplyr::group_by(year, scenario) %>%
  dplyr::summarise(country = "All",
                   cluster = "All",
                   total_deaths = sum(total_deaths),
                   total_vials = sum(total_vials),
                   total_cost = sum(total_cost),
                   total_deaths_averted = sum(total_deaths_averted),
                   total_YLL = sum(total_YLL),
                   total_YLL_averted = sum(total_YLL_averted),
                   vaccinated = sum(vaccinated),
                   fully_vaccinated = sum(fully_vaccinated),
                   p_seek_rabid = mean(p_seek_rabid),
                   p_seek_healthy = mean(p_seek_healthy),
                   p_receive = mean(p_receive),
                   p_receive_healthy = mean(p_receive_healthy),
                   p_receive_RIG = mean(p_receive_RIG),
                   p_complete = mean(p_complete),
                   TargetPopulation_rabid = sum(TargetPopulation_rabid),
                   TargetPopulation_healthy = sum(TargetPopulation_healthy),
                   RIG = sum(RIG))

global_sums$cost_per_death_averted <-  global_sums$total_cost/global_sums$total_deaths_averted
global_sums$cost_per_YLL_averted <-  global_sums$total_cost/global_sums$total_YLL_averted
global_sums$deaths_averted_per_100k_vaccinated <-  global_sums$total_deaths_averted/global_sums$vaccinated/100000

write.csv(global_sums, "output/global_stats.csv")
# write.csv(global_sums, "output/global_stats_med_throughput.csv")

model_summary <- out %>%
  dplyr::group_by(country, year, scenario) %>%
  dplyr::summarise(deaths_lci = quantile(total_deaths, 0.025),
                   deaths_med = quantile(total_deaths, 0.5),
                   deaths_uci = quantile(total_deaths, 0.975),

                   vacc_lci = quantile(vaccinated, 0.025),
                   vacc_med = quantile(vaccinated, 0.5),
                   vacc_uci = quantile(vaccinated, 0.975),

                   vials_lci = quantile(total_vials, 0.025),
                   vials_med = quantile(total_vials, 0.5),
                   vials_uci = quantile(total_vials, 0.975)
  )

names(model_summary)

# Plots for each country
egs = c("Kenya", "Madagascar", "India", "Bangladesh")

pdf("figs/rabies_examples.pdf", height=4, width=12)
par(mfrow=c(1,3), mar=c(2,4,2,1), mgp=c(1.5,0.5,0), tck=-0.005)
for(i in egs){
    m = model_summary[which(model_summary$country==i),]

  # DEATHS
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$deaths_med[which(m$scenario=="a1")],
       main = i, ylab = "Human rabies deaths", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$deaths_uci)))
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a1")],rev(m$deaths_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$deaths_med[which(m$scenario=="a1")])
  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a3_1")],rev(m$deaths_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$deaths_med[which(m$scenario=="a3_1")], col="red")
  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a5_1")],rev(m$deaths_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$deaths_med[which(m$scenario=="a5_1")], col="blue")

  # VACCINATED
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$vacc_med[which(m$scenario=="a1")],
       main = i, ylab = "PVP", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$vacc_uci)))
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a1")],rev(m$vacc_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$vacc_med[which(m$scenario=="a1")])
  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a3_1")],rev(m$vacc_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$vacc_med[which(m$scenario=="a3_1")], col="red")
  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a5_1")],rev(m$vacc_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$vacc_med[which(m$scenario=="a5_1")], col="blue")

  # VIALS
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")],
       main = i, ylab = "Vials used (no buffer)", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$vials_uci)))
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a1")],rev(m$vials_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")])
  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a3_1")],rev(m$vials_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$vials_med[which(m$scenario=="a3_1")], col="red")
  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a5_1")],rev(m$vials_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$vials_med[which(m$scenario=="a5_1")], col="blue")
  }
dev.off()


# Plots for each country
pdf("figs/country_deaths_vacc_vials.pdf", height=4, width=12)
# pdf("figs/country_deaths_vacc_vials_med_throughput.pdf", height=4, width=12)
par(mfrow=c(1,3), mar=c(2,4,2,1), mgp=c(1.5,0.5,0), tck=-0.005)
for(i in 1:length(countries)){
  m = model_summary[which(model_summary$country==countries[i]),]

  # DEATHS
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$deaths_med[which(m$scenario=="a1")],
       main = countries[i], ylab = "Deaths", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$deaths_uci)))
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a1")],rev(m$deaths_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$deaths_med[which(m$scenario=="a1")])

  # scenario a2  - with dog vacc
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a2")],rev(m$deaths_uci[which(m$scenario=="a2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a2")], m$deaths_med[which(m$scenario=="a2")], col="blue", lwd=2)

  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a3_1")],rev(m$deaths_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$deaths_med[which(m$scenario=="a3_1")], col="red")

  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a5_1")],rev(m$deaths_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$deaths_med[which(m$scenario=="a5_1")], col="blue")

  # scenario a5_1  - with gavi and dog vacc and IBCM
  polygon(c(yrs, rev(yrs)), c(m$deaths_lci[which(m$scenario=="a5_2")],rev(m$deaths_uci[which(m$scenario=="a5_2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_2")], m$deaths_med[which(m$scenario=="a5_2")], col="blue", lty=2)


  # VACCINATED
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$vacc_med[which(m$scenario=="a1")],
       main = countries[i], ylab = "Vaccinated", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$vacc_uci)))
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a1")],rev(m$vacc_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$vacc_med[which(m$scenario=="a1")])

  # scenario a2  - with dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a2")],rev(m$vacc_uci[which(m$scenario=="a2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a2")], m$vacc_med[which(m$scenario=="a2")], col="blue", lwd=2)

  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a3_1")],rev(m$vacc_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$vacc_med[which(m$scenario=="a3_1")], col="red")

  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a5_1")],rev(m$vacc_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$vacc_med[which(m$scenario=="a5_1")], col="blue")

  # scenario a5_2  - with gavi and dog vacc and IBCM
  polygon(c(yrs, rev(yrs)), c(m$vacc_lci[which(m$scenario=="a5_2")],rev(m$vacc_uci[which(m$scenario=="a5_2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_2")], m$vacc_med[which(m$scenario=="a5_2")], col="blue", lty=2)


  # VIALS
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")],
       main = countries[i], ylab = "Vials", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$vials_uci)))
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a1")],rev(m$vials_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")])

  # scenario a2  - with dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a2")],rev(m$vials_uci[which(m$scenario=="a2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a2")], m$vials_med[which(m$scenario=="a2")], col="blue", lwd=2)

  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a3_1")],rev(m$vials_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$vials_med[which(m$scenario=="a3_1")], col="red")

  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a5_1")],rev(m$vials_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$vials_med[which(m$scenario=="a5_1")], col="blue")

  # scenario a5_2  - with gavi and dog vacc and IBCM
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a5_2")],rev(m$vials_uci[which(m$scenario=="a5_2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_2")], m$vials_med[which(m$scenario=="a5_2")], col="blue", lty=2)
}
dev.off()


pdf("figs/country_demand.pdf", height=4, width=12)
par(mfrow=c(1,3), mar=c(2,4,2,1), mgp=c(1.5,0.5,0), tck=-0.005)
for(i in 1:length(countries)){
  m = model_summary[which(model_summary$country==countries[i]),]

  # VIALS
  # scenario a1
  plot(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")],
       main = countries[i], ylab = "Vials", xlab = "",
       type="l", xlim = c(2020,2035), ylim=c(0, max(m$vials_uci)))
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a1")],rev(m$vials_uci[which(m$scenario=="a1")])), col="grey", border=NA)
  lines(m$year[which(m$scenario=="a1")], m$vials_med[which(m$scenario=="a1")])

  # scenario a2  - with dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a2")],rev(m$vials_uci[which(m$scenario=="a2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a2")], m$vials_med[which(m$scenario=="a2")], col="blue", lwd=2)

  # scenario a3  - with gavi
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a3_1")],rev(m$vials_uci[which(m$scenario=="a3_1")])), col="lightsalmon", border=NA)
  lines(m$year[which(m$scenario=="a3_1")], m$vials_med[which(m$scenario=="a3_1")], col="red")

  # scenario a5_1  - with gavi and dog vacc
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a5_1")],rev(m$vials_uci[which(m$scenario=="a5_1")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_1")], m$vials_med[which(m$scenario=="a5_1")], col="blue")

  # scenario a5_2  - with gavi and dog vacc and IBCM
  polygon(c(yrs, rev(yrs)), c(m$vials_lci[which(m$scenario=="a5_2")],rev(m$vials_uci[which(m$scenario=="a5_2")])), col="light blue", border=NA)
  lines(m$year[which(m$scenario=="a5_2")], m$vials_med[which(m$scenario=="a5_2")], col="blue", lty=2)
}
dev.off()



#
#
# ## filter to just retain 4 metrics
# multivariate_for_plot <- multivariate
# unique(multivariate_for_plot$metric)
# metric_plot <- c("Total Deaths", "Total YLL", "Cost per Death Averted", "Cost per YLL Averted")
# multivariate_for_plot <- dplyr::filter(multivariate_for_plot, metric %in% metric_plot)
# unique(multivariate_for_plot$metric)
# multivariate_for_plot$metric <- as.character(multivariate_for_plot$metric)
# multivariate_for_plot$metric <- as.factor(multivariate_for_plot$metric)
# unique(multivariate_for_plot$metric)
#
# out_countries <- as.character(unique(multivariate_for_plot$country))
#
# # Examine outcomes across all scenarios
# cbPalette <- c("#D55E00", "#56B4E9", "darkblue", "lightgreen", "#009E73", "grey", "black")
#
# # export plots
# pdf("figs/multivariate_test_endemic.pdf", width=12, height=9)
# for(i in 1:length(unique(multivariate$country)))
# {
#   pi <- ggplot(multivariate_for_plot[which(multivariate_for_plot$country==out_countries[i]),]) +
#     geom_point(aes(x=scenario, y=med,colour=scenario)) +
#     theme_classic()+
#     geom_errorbar(aes(x=scenario,ymin=lci,ymax=uci,colour=scenario, width=0.3)) +
#     scale_colour_manual(values=cbPalette) + facet_wrap(~metric,scales="free_y") +
#     labs(colour="Scenario",y="Value",x="") +
#     ggtitle(out_countries[i]) +
#     theme(plot.title = element_text(size=14,face="bold",hjust = 0.5))
#
#
#   print(pi)
# }
#
# dev.off()
#
#


