#######################################################################
# # TEST ANALYSIS
# ndraw=1000
# horizon=10
# GAVI_status="none"
# DogVax_TF=F
# VaxRegimen="Updated TRC"
# ClinicThroughput="High"
# DALYrabies=DALYrabies_input
# LE=GBD2010$LE
# RIG_status="none"
# discount=0
# breaks="5yr"
# country="Tanzania"

multivariate_analysis<-function(ndraw, horizon, GAVI_status, DogVax_TF, VaxRegimen, DALYrabies, LE, RIG_status, discount, breaks){
  # create list of countries to analyse
  list<-vector("list", nrow(data))
  names(list)<-data$country

  # For all countries - fill list with iterations
  for(i in 1:nrow(data)){
    list[[i]] <- cbind.data.frame(decision_tree_ndraw(ndraw=ndraw,
                                                      country=data$country[i],
                                                      horizon=horizon,
                                                      GAVI_status=GAVI_status,
                                                      DogVax_TF=DogVax_TF,
                                                      VaxRegimen=VaxRegimen, RIG_status=RIG_status,
                                                      DALYrabies=DALYrabies, LE=LE, discount=discount, breaks=breaks),
                                  country=data$country[i])
    # Print % completion to console
    print(paste(round(100/nrow(data)*i, digits=1), "%", sep=" "))
    }

  out<-do.call(rbind.data.frame, list)
  # Label data.frame names properly
  colnames(out)<-c("year","total_deaths","total_vials","total_cost","total_deaths_averted",
                   "total_YLL","total_YLL_averted","vaccinated","fully_vaccinated","iter","country")

  m_out <- melt(out, id=c("year", "iter", "country"), variable.name="metric")

  # Summarize each country outputs
  country_sums<-ddply(m_out,c("iter","country","metric"), function(m_out)sum(m_out$value))
  country_sums<-merge(country_sums, data[,c("country","cluster")], all.x=T)

  # Summarize each cluster outputs
  cluster_sums<-ddply(country_sums,c("iter","cluster","metric"), function(country_sums)sum(country_sums$V1))
  cluster_sums$country<-cluster_sums$cluster

  # Summarize global estimates
  global_sums<-ddply(cluster_sums,c("iter","metric"), function(cluster_sums)sum(cluster_sums$V1))
  global_sums$country<-"All"
  global_sums$cluster<-"All"

  summed_draws<-rbind.data.frame(country_sums,
                                 cluster_sums[ ,c("country","iter","metric","V1","cluster")],
                                 global_sums[ ,c("country","iter","metric","V1","cluster")])
  summed_draws<-reshape(summed_draws, idvar = c("country","iter","cluster"), timevar = "metric", direction = "wide")
  colnames(summed_draws)<-gsub("V1.","", colnames(summed_draws))

  summed_draws$cost_per_death_averted<-summed_draws$total_cost/summed_draws$total_deaths_averted
  summed_draws$cost_per_YLL_averted<-summed_draws$total_cost/summed_draws$total_YLL_averted
  summed_draws$deaths_averted_per_1k_vaccinated<-summed_draws$total_deaths_averted/(summed_draws$vaccinated/1000)
  summed_draws <- melt(summed_draws, id=c("iter","country","cluster"), variable.name="metric")

  quantiles<-ddply(summed_draws, c("country","metric"), function(summed_draws)quantile(summed_draws$value,c(0.025,0.5,0.975)))

  colnames(quantiles)<-c("country","metric","lci","med","uci")
  quantiles$lci<-as.numeric(as.character(quantiles$lci))
  quantiles$med<-as.numeric(as.character(quantiles$med))
  quantiles$uci<-as.numeric(as.character(quantiles$uci))
  return(quantiles)}

