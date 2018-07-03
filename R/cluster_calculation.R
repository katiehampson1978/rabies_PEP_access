# Calculate cluster averages from countries with high quality studies
# Current clusters: East Africa (including Yemen), West Africa, SE Asia, Asia1 and Eurasia, Americas (Nicaragua and Haiti)
# East Africa = Kenya, TZ, Madagascar
# West Africa = Chad, Mali, Cote d'Ivoire
# SE Asia = Cambodia, +? (Sri Lanka, Philippines)
# Asia1 and Eurasia = Pakistan, Bhutan +? (Bangladesh)
# Americas = Haiti

rm(list=ls())
# Edited on 11/08/17 by Laura C. - 10$ per vial for all missing vial data

# Read in "raw" country data
#file.name <- paste0("output/Data_curation/country_data_", Sys.Date(), ".csv", sep="")
country_data<-read.csv("output/Data_curation/country_data_GBP.csv")
country_data <- country_data[which(!is.na(country_data$new.cluster)),] # Only take those with a cluster
colnames(country_data)[colnames(country_data)=="new.cluster"] <- "cluster" # rename column for this process (changed back at the end)

# Drop all low-quality data
# high_quality<-country_data[which(country_data$country%in%c("Kenya","Tanzania","Madagascar",
#                                                            "Chad","Mali","Ivory Coast",
#                                                            "Cambodia","Pakistan",
#                                                            "Bhutan", "Haiti")),]

# Use all countries that have class=country
high_quality <- country_data[which(country_data$Country.or.cluster.=="country"),]

# Calculate cluster averages and number of countries informing
cluster_mean<-function(vector){tapply(vector,high_quality$cluster,mean,na.rm=T)}
cluster_n <- function(vector){tapply(vector, high_quality$cluster, length)}
cluster_data<-apply(high_quality[,c("bite_inc_mean","p_rabid","p_rabid_n","p_seek1",
                      "p_seek_n1","p_receive","p_receive_n","p_complete","p_complete_n",
                      "median_age","price_per_vial","price_per_vial_RIG")], MARGIN=2, cluster_mean)
cluster_data_n<-apply(high_quality[,c("bite_inc_mean","p_rabid","p_rabid_n","p_seek1",
                                      "p_seek_n1","p_receive","p_receive_n","p_complete","p_complete_n",
                                      "median_age","price_per_vial","price_per_vial_RIG")], MARGIN=2, cluster_n)

cluster_data<-cbind.data.frame(cluster=rownames(cluster_data),
                                n_countries_informing=cluster_data_n[,1], cluster_data)

# Use bite variance from East Africa for all countries
# cluster_data$bite_inc_variance<-cluster_data$bite_inc_variance[which(cluster_data$cluster=="east africa")]

# Use $10 as price per vial where no data
cluster_data$price_per_vial[which(is.na(cluster_data$price_per_vial))]<-10

# Use $25 as price per RIG where no data
cluster_data$price_per_vial_RIG[which(is.na(cluster_data$price_per_vial_RIG))]<-25

# Round sample sizes
cluster_data$p_rabid_n<-round(cluster_data$p_rabid_n)
cluster_data$p_seek_n1<-round(cluster_data$p_seek_n1)
cluster_data$p_receive_n<-round(cluster_data$p_receive_n)
cluster_data$p_complete_n<-round(cluster_data$p_complete_n)

# Drop "n/a" cluster
cluster_data<-cluster_data[-which(cluster_data$cluster=="n/a"),]

# Replace "NaN" with "NA"
#cluster_data[is.na(cluster_data)] <- NA

# Write cluster data
file.name <- paste0("output/Data_curation/cluster_data_", Sys.Date(), ".csv", sep="")
write.csv(cluster_data, file=file.name, row.names=FALSE)
