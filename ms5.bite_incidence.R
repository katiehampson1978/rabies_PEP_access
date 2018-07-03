################################################################################
#                       SUMMARISE BITE INCIDENCE ESTIMATES                     #
# takes bite incidence data from templates and saves output/bite_incidence,csv
# creates the parameters table
################################################################################
rm(list=ls())
library(gdata)
library(triangle)
library(ggplot2)
detach("package:plyr") # , unload=TRUE)
require(dplyr)
library(sjPlot)
library(fitdistrplus)
library(bbmle)
library(grid)
source("R/HighstatLibV6.R")

################################################################################
#              Import incidence data for regression analysis                   #
################################################################################
# All countries with bite incidence data:
countries <-c("Afghanistan", "Bangladesh", "Bhutan", "Cambodia", "Cameroon",
              "CAR", "Chad", "DRC", "Ethiopia", "Gambia", "Ghana", "Guinea",
              "Haiti", "India", "Ivory Coast", "Kenya", "Laos","Liberia",
              "Madagascar", "Malawi", "Mali", "Mozambique", "Nepal", "Nigeria",
              "Pakistan", "Philippines", "Senegal", "Sri Lanka", "Tajikistan",
              "Tanzania", "Thailand", "Togo", "Uganda", "Vietnam", "Yemen",
              "Zambia", "Zimbabwe")
length(countries) # 37 Countries in List

# Create dummy dataframe
data <- data.frame(country=NA, bite_incidence=NA, source=NA, setting=NA, rabies=NA, vaccine_free=NA)
data <- data[0,]

# Import data and add to dataframe
for(i in 1:length(countries)){

  #print(i)
  filename<-paste0("data/DataTemplate/data_template_WHO_", countries[i],".xlsx")
  template<-read.xls(filename, sheet="bite_incidence", stringsAsFactors=FALSE)

  indx <- which(!colnames(template) %in% colnames(data))
  if(length(indx) > 0)
  {
    template <- template[,-indx]
  } else {
    template <- template
  }

  data <- rbind(data, template)
}

## States of India - group under India
in_states <- c("Bihar", "Himachal Pradesh", "Karnataka", "Kerala")
in_states_index <- which(data$country %in% in_states)
data$country[in_states_index] <- "India"
#View(data)

# Read in data from Healthcare for Tanzania, Madagascar, Sri Lanka
hc_data <- read.csv("data/bite_inc_data.csv")
hc_data$bite_inc_variance <- NULL
colnames(hc_data) <- c("country", "bite_incidence")
hc_data$source <- c(rep("healthcare", nrow(hc_data)))
hc_data$setting <- NA
hc_data$rabies <- NA
hc_data$vaccine_free <- c(data$vaccine_free[grep(as.character(hc_data$country[1]), data$country)],
                          data$vaccine_free[grep(as.character(hc_data$country[2]), data$country)],
                          data$vaccine_free[grep(as.character(hc_data$country[3]), data$country)])

# Bind dataframes together
final_data <- rbind(data, hc_data)
final_data <- final_data[order(final_data$country),] # Order by Country
rownames(final_data) <- 1:nrow(final_data) # Reset row numbers

## save output
##   Currently, NO PERMISSION TO USE MADAGASCAR BITE INCIDENCE DATA
final_data <- final_data[-c(which(final_data$country=="Madagascar")),]
rownames(final_data) <- 1:nrow(final_data) # Reset row numbers
write.csv(final_data, "output/bite_incidence.csv", row.names=FALSE)

#########################
# COMPLETE PARAMS TABLE #
#########################

# KH - WHY YOU REMOVE THE DOG POP AND ID DATA?
# FV: I am removing the Knoble dog pop estimate (but retain the dog pop estimates
# from burden study) and ID because it has ~ 190 countries contributing
# import params table produced in script 1
params_table <- read.csv("output/params_table_country_availability.csv")
colnames(params_table)
colnames(params_table)[2] <- "Parameter Meaning"
colnames(params_table)[3] <- "Number of countries contributing"
colnames(params_table)[4] <- "Names of countries contributing"
params_table$Parameter <- as.character(params_table$Parameter)
params_table$`Parameter Meaning` <- as.character(params_table$`Parameter Meaning`)
params_table$`Number of countries contributing` <- as.character(params_table$`Number of countries contributing`)
params_table$`Names of countries contributing` <- as.character(params_table$`Names of countries contributing`)

# params to remove
params_excl <- c("total_dogs_Knoble", "PEP_Intradermal_administration", "RIG usage",
                 "RIG price per vial", "RIG cost for patients")
params_table <- params_table[-which(params_table$Parameter %in% params_excl),]

bites_for_table <- final_data[which(final_data$source=="healthcare"),]

bites_for_table <- bites_for_table %>%
  group_by(country) %>%
  summarise(bite_incidence = mean(bite_incidence))

bite_inc_countries <- c(bites_for_table$country)
bite_inc_countries <- paste(bite_inc_countries, collapse = "; ")
bite_inc_length <- nrow(bites_for_table)

b_i_index <- which(params_table$Parameter=="Bite incidence")
params_table$Parameter[b_i_index] <- "Bite incidence"
params_table$`Number of countries contributing`[b_i_index] <- bite_inc_length
params_table$`Names of countries contributing`[b_i_index] <- bite_inc_countries

write.csv(params_table, "output/Paper/params_table.csv", row.names=FALSE)

