################################################################################
#                 Setup data for multivariate decision-tree model              #
# Prepare data for running decision tree scenarios: output/gavi_output_data.csv
################################################################################
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

#######################################################################
# Model uses data on:
# Population and demography (lifespan) - source: UNWPP
# Route of PEP administration - PEP logistics survey
# Biological parameters on rabies transmission - Tanzania data
# Health seeking and access - country specific studies & literature
# Vaccination scenarios - informed by Gavi & PEP modeling
#######################################################################

#######################################################################
# SCENARIOS
# (1) The status quo, with countries delivering PEP according to methods already used and at current treatment rates
# (2) The status quo, with declining incidence of rabies over time as canine vaccination efforts expand
# (3) GAVI support to increased access to post-exposure vaccination and switch to ID delivery
#### (3.1) base - 10% increase in treatment seeking and receipt, 3% per annum thereafter to cap
#### (3.2) low - 5% increase in treatment seeking and receipt, 3% per annum thereafter to cap
#### (3.3) high - 15% increase in treatment seeking and receipt, 3% per annum thereafter to cap
# (4) As in 3.1 (base), with the addition of RIG according to latest WHO recommendations, with RIG infiltrated only at wound
# (5) With declining incidence of rabies over time as canine vaccination efforts expand
#### (5a) With support for improved access to post-exposure vaccines (3.1)
#### (5b) As (5a) with integrated bite case management (rationalized PEP provision)
#######################################################################

# Load in data from curation process (prepped_data)
country_data <- read.csv("output/prepped_data_final.csv")
country_data$Business.Plan.Phase <- as.character(country_data$Business.Plan.Phase)
country_data$Business.Plan.Phase[which(is.na(country_data$Business.Plan.Phase))] <- 0

# Americas - Bolivia, Cuba, Haiti, Honduras - all in GBP (I or II); Guyana, Nicaragua not in GBP
country_data$Business.Plan.Phase[which(country_data$cluster=="americas" & country_data$Business.Plan.Phase=="II")] <- "I"

# Only include countries endemic for rabies AND GAVI eligibile
unique(country_data$gavi); table(country_data$gavi) # Gavi eligible
unique(country_data$endemic); table(country_data$endemic) # Endemic
data_prepped <- dplyr::filter(country_data, endemic==1) %>% dplyr::filter(gavi=="TRUE")
dim(data_prepped) # 67 rabies endemic AND gavi countries

#######################################################################
#               Arrange data ready to run scenarios                   #
#######################################################################

data_pre_test <- data_prepped
data_pre_test$bite_inc_susp = data_pre_test$exp_inc
data_pre_test$bite_inc_non_susp = data_pre_test$preds_SQ
data_pre_test$bite_inc_susp_LCI <- data_pre_test$LCI_exp_inc
data_pre_test$bite_inc_susp_UCI <- data_pre_test$UCI_exp_inc

# Clusters
unique(data_pre_test$cluster); dim(data_pre_test)

# Columns storing the sample size
ssize_cols <- grep("_n", colnames(data_pre_test), value=TRUE)
ssize_cols <- grep("p_", ssize_cols, value=TRUE)
data <- data_pre_test

# Insert 1000 in the columns where no sample size available
for(k in 1:length(ssize_cols)){
  indx <- which(complete.cases(data[[ssize_cols[k]]])==F)
  if(rlang::is_empty(indx)){ ## if no NAs, do nothing
    indx <- indx
  } else {
    data[[ssize_cols[k]]] <- 1000 ## otherwise, overwrite a dummy ssize
  }
}

# Sort out proportion ID data
data$ID[which(is.na(data$ID))]<-FALSE
data$ID = data$ID*1
data$ID[which(data$country=="India")] = 0.4 # doing some ID administration (extracted from India data)

# Write gavi-prepared data
write.csv(data, "output/gavi_output_data.csv", row.names = FALSE)
