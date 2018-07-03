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
library(dplyr)

multivariate_analysis<-function(ndraw, horizon, GAVI_status, DogVax_TF, VaxRegimen, DALYrabies, LE, RIG_status, discount, breaks, IBCM){
  # create list of countries to analyse
  list<-vector("list", nrow(data))
  names(list)<-data$country

  # For all countries - fill list with iterations
  for(i in 1:nrow(data)){
    # print(data$country[i])
    list[[i]] <- cbind.data.frame(decision_tree_ndraw(ndraw=ndraw,
                                                      country=data$country[i],
                                                      horizon=horizon,
                                                      GAVI_status=GAVI_status,
                                                      DogVax_TF=DogVax_TF,
                                                      VaxRegimen=VaxRegimen, RIG_status=RIG_status,
                                                      DALYrabies=DALYrabies, LE=LE, discount=discount, breaks=breaks,
                                                      IBCM=IBCM),
                                  country=data$country[i])

    list[[i]]$cluster <- data$cluster[i]

    # Print % completion to console
    print(paste(round(100/nrow(data)*i, digits=1), "%", sep=" "))
  }
  out<-do.call(rbind.data.frame, list)

  # Label data.frame names properly
  colnames(out) <- c("year","total_deaths","U5_deaths","total_vials","total_cost","total_deaths_averted","U5_deaths_averted",
                     "total_YLL","total_YLL_averted","YLL_averted_U5","vaccinated","fully_vaccinated",
                     "p_seek_rabid", "p_seek_healthy", "p_receive", "p_receive_IBCM", "p_receive_RIG", "p_complete",
                     "TargetPopulation_rabid","TargetPopulation_healthy","RIG","Gavi_support",
                     "iter","country","cluster")

  return(out)

  # m_out <- melt(out, id=c("year", "iter", "country","cluster"), variable.name="metric")
  # quantiles<-ddply(m_out, c("country","cluster","year","metric"), function(m_out)quantile(m_out$value,c(0.025,0.5,0.975, na.rm=TRUE)))
  # # colnames(quantiles)<-c("country","metric","year","lci","med","uci")
  # # quantiles$lci<-as.numeric(as.character(quantiles$lci))
  # # quantiles$med<-as.numeric(as.character(quantiles$med))
  # # quantiles$uci<-as.numeric(as.character(quantiles$uci))
  # return(quantiles)
  # # return(m_out)
  }







  # Summarize each cluster outputs - sum metrics from all countries belonging to same cluster
  # cluster_sums<- country_sums %>%
  #   dplyr::group_by(cluster, year) %>%
  #   dplyr::summarise(country = unique(cluster),
  #                    total_deaths = sum(total_deaths),
  #                    total_vials = sum(total_vials),
  #                    total_cost = sum(total_cost),
  #                    total_deaths_averted = sum(total_deaths_averted),
  #                    total_YLL = sum(total_YLL),
  #                    total_YLL_averted = sum(total_YLL_averted),
  #                    vaccinated = sum(vaccinated),
  #                    fully_vaccinated = sum(fully_vaccinated),
  #                    p_seek_rabid = mean(p_seek_rabid),
  #                    p_seek_healthy = mean(p_seek_healthy),
  #                    p_receive = mean(p_receive),
  #                    p_complete = mean(p_complete),
  #                    TargetPopulation_rabid = sum(TargetPopulation_rabid),
  #                    TargetPopulation_healthy = sum(TargetPopulation_healthy),
  #                    RIG = sum(RIG))
  #
  #
  # # # Summarize global estimates - sum metrics from all countries
  # global_sums<- cluster_sums %>%
  #   dplyr::group_by(year) %>%
  #   dplyr::summarise(country = "All",
  #                    cluster = "All",
  #                    total_deaths = sum(total_deaths),
  #                    total_vials = sum(total_vials),
  #                    total_cost = sum(total_cost),
  #                    total_deaths_averted = sum(total_deaths_averted),
  #                    total_YLL = sum(total_YLL),
  #                    total_YLL_averted = sum(total_YLL_averted),
  #                    vaccinated = sum(vaccinated),
  #                    fully_vaccinated = sum(fully_vaccinated),
  #                    p_seek_rabid = mean(p_seek_rabid),
  #                    p_seek_healthy = mean(p_seek_healthy),
  #                    p_receive = mean(p_receive),
  #                    p_complete = mean(p_complete),
  #                    TargetPopulation_rabid = sum(TargetPopulation_rabid),
  #                    TargetPopulation_healthy = sum(TargetPopulation_healthy),
  #                    RIG = sum(RIG))

  # rbind data frames
#   alldata <- rbind.data.frame(country_sums, cluster_sums, global_sums)
#
#   return(alldata)
# }

