# Edited on 22/08/17 by Laura C. - p_seek1, calculating incidence from suspect and non-suspect dog bites, population data from 2020 to 2070
# Updated on 11/09/2017 by Katie H. - updated CIs for rabid dog bite incidence, consider non-gavi countries


# Take the raw data from data_curation.R and fill in cluster estimates for appropriate countries and estimates:
# Read in "raw" country data

rm(list=ls())

# Data Curation
#file.name <- paste0("output/Data_curation/country_data_", Sys.Date(), ".csv", sep="")
#country_data <- read.csv(file=file.name, stringsAsFactors=T)

# Data curation with GBP
file.name <- paste0("output/Data_curation/country_data_GBP.csv", sep="")
country_data <- read.csv(file=file.name, stringsAsFactors=T)
country_data <- country_data[which(!is.na(country_data$new.cluster)),] # Only take those with a cluster
colnames(country_data)[colnames(country_data)=="new.cluster"] <- "cluster" # rename column for this process (changed back at the end)

# Read in cluster data
file.name <- paste0("output/Data_curation/cluster_data_", Sys.Date(), ".csv", sep="")
cluster_data <- read.csv(file=file.name,stringsAsFactors=T)

# Produce a prepped data table for input into decision_tree.R
# If fewer than 2 decision tree parameters available for a country, replace with cluster estimates
country_df = c("country","code","continent","cluster","class","data_source","comment","gavi","prop_urban",
               "pop2020","pop2021","pop2022","pop2023","pop2024","pop2025","pop2026","pop2027","pop2028","pop2029",
               "pop2030","pop2031","pop2032","pop2033","pop2034","pop2035","pop2036","pop2037","pop2038","pop2039",
               "pop2040","pop2041","pop2042","pop2043","pop2044","pop2045","pop2046","pop2047","pop2048","pop2049",
               "pop2050","pop2051","pop2052","pop2053","pop2054","pop2055","pop2056","pop2057","pop2058","pop2059",
               "pop2060","pop2061","pop2062","pop2063","pop2064","pop2065","pop2066","pop2067","pop2068","pop2069",
               "pop2070", "cost_first_visit","cost_followup_visit","urban_dogs","rural_dogs","total_dogs","ID", "ID_prop")

# For each "cluster" country, attach the cluster values calculated from "country" averages
# data_prepped_cluster <- merge(country_data[which(country_data$class=="cluster"), country_df],
#                               cluster_data, by="cluster", all.x=T)
#data_prepped_country<-country_data[which(country_data$Country.or.cluster.=="country"),]
#data_prepped_cluster$cluster<-as.character(data_prepped_cluster$cluster)
#data_prepped_country$RIG_usage <- as.character(data_prepped_country$RIG_usage)
cluster_data$cluster<-as.character(cluster_data$cluster)

data_prepped_country <- country_data

# Remove countries with an n/a for cluster (Rabies free!!)
data_prepped_country <- data_prepped_country[which(data_prepped_country$cluster!="n/a"),]

# Loop through all countries - cluster countries now have some cost values read in,
# so ALL countries should go through this process:
for(i in 1:nrow(data_prepped_country)){
  # Bite incidence - perhaps remove these?
  if(is.na(data_prepped_country$bite_inc_mean[i]))
    data_prepped_country$bite_inc_mean[i] <- cluster_data$bite_inc_mean[which(cluster_data$cluster==data_prepped_country$cluster[i])]
  # if(is.na(data_prepped_country$bite_inc_variance[i])) data_prepped_country$bite_inc_variance[i] <-cluster_data$bite_inc_variance[which(cluster_data$cluster==data_prepped_country$cluster[i])]

  # p_rabid
  if(is.na(data_prepped_country$p_rabid[i])|is.na(data_prepped_country$p_rabid_n[i])){
    data_prepped_country$p_rabid[i]<-cluster_data$p_rabid[which(cluster_data$cluster==data_prepped_country$cluster[i])]
    data_prepped_country$p_rabid_n[i]<-cluster_data$p_rabid_n[which(cluster_data$cluster==data_prepped_country$cluster[i])]
    }

  # p_seek1 (rabid)
  if(is.na(data_prepped_country$p_seek1[i])|is.na(data_prepped_country$p_seek_n1[i])){
    data_prepped_country$p_seek1[i]<-cluster_data$p_seek1[which(cluster_data$cluster==data_prepped_country$cluster[i])]
    data_prepped_country$p_seek_n1[i]<-cluster_data$p_seek_n1[which(cluster_data$cluster==data_prepped_country$cluster[i])]
  }

  # p_receive
  if(is.na(data_prepped_country$p_receive[i])|is.na(data_prepped_country$p_receive_n[i])){
    data_prepped_country$p_receive[i]<-cluster_data$p_receive[which(cluster_data$cluster==data_prepped_country$cluster[i])]
    data_prepped_country$p_receive_n[i]<-cluster_data$p_receive_n[which(cluster_data$cluster==data_prepped_country$cluster[i])]
  }

  # p_complete
  if(is.na(data_prepped_country$p_complete[i])|is.na(data_prepped_country$p_complete_n[i])){
    data_prepped_country$p_complete[i]<-cluster_data$p_complete[which(cluster_data$cluster==data_prepped_country$cluster[i])]
    data_prepped_country$p_complete_n[i]<-cluster_data$p_complete_n[which(cluster_data$cluster==data_prepped_country$cluster[i])]
  }

  # Median age - perhaps remove?
  if(is.na(data_prepped_country$median_age[i]))
    data_prepped_country$median_age[i]<-cluster_data$median_age[which(cluster_data$cluster==data_prepped_country$cluster[i])]

  # PEP costs (vaccine & RIG) - IF RIG USAGE IS FALSE OR NA, RIG PRICE IS NOT INSERTED
  if(is.na(data_prepped_country$price_per_vial[i]))
    data_prepped_country$price_per_vial[i]<-cluster_data$price_per_vial[which(cluster_data$cluster==data_prepped_country$cluster[i])]
  if(is.na(data_prepped_country$price_per_vial_RIG[i]))
    data_prepped_country$price_per_vial_RIG[i]<-cluster_data$price_per_vial_RIG[which(cluster_data$cluster==data_prepped_country$cluster[i])]
}

# If country is Gavi eligable and RIG available is NA, replace with FALSE
for(i in 1:nrow(data_prepped_country)){
  if(data_prepped_country$gavi[i]==TRUE & is.na(data_prepped_country$RIG_usage[i]))
    data_prepped_country$RIG_usage[i] <- FALSE
}

data_prepped <- data_prepped_country

# pep_df = c("bite_inc_mean","p_rabid","p_rabid_n","median_age",
#   "p_seek1","p_seek_n1","p_receive","p_receive_n","p_complete","p_complete_n",
#   "price_per_vial","price_per_vial_RIG")
#
# data_prepped<-rbind.data.frame(data_prepped_country[, c(country_df, pep_df)], data_prepped_cluster[,c(country_df, pep_df)])
#
# match(country_df, names(data_prepped_country))


rabies = read.csv("transmission_model/baseline_incidence.csv") # incidence from fitted model - with NO vaccination
y = length(2050:2020) # years
lambda = exp(log(data_prepped$pop2050/data_prepped$pop2020)/y)# population growth
dogs = data_prepped$total_dogs*lambda^5 # to 2020
rabid_dog_bites = 0.38 # Bites per rabid dog - best est, lower, upper
healthy_dog_bites = 1/5 # bites per healthy dog - complete guess for now!
rabid_dogs = dogs * rabies$inc # rabid dogs in each country in 2020
exposures = rabid_dogs * rabid_dog_bites # exposures due to rabid dogs
normal_bites = dogs * healthy_dog_bites # healthy dog bites

# exposure incidence - with confidence intervals
data_prepped$bite_inc_susp = dogs * rabies$inc * rabid_dog_bites /data_prepped$pop2020 # Rabid dog bite incidence
data_prepped$bite_inc_susp_LCI = dogs * rabies$incLCI * rabid_dog_bites /data_prepped$pop2020
data_prepped$bite_inc_susp_UCI = dogs * rabies$incUCI * rabid_dog_bites /data_prepped$pop2020
data_prepped$bite_inc_non_susp = normal_bites / data_prepped$pop2020 # healthy dog bite incidence

colnames(data_prepped)[colnames(data_prepped)=="cluster"] <- "new.cluster" # rename column for this process (changed back at the end)


# Write prepped data
file.name <- paste0("output/Data_curation/prepped_data_", Sys.Date(), ".csv", sep="")
write.csv(data_prepped, file=file.name, row.names=FALSE)
