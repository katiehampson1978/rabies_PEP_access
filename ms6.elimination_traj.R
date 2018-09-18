################################################################################
#         2020-2070: Rabies elimination scenarios and dog population           #
# Use country population growth and dog pop data (output/prepped_data.csv from #
# ms5) with dynamic model trajectories (data/baseline_incidence_Gavi.csv &     #
# data/vax_obs_ts_gavi.csv) to get country-specific trajectories               #
# (output/rabies_traj.csv) by GBP phase                                        #
################################################################################

rm(list=ls())

# Full horizon
yrs = 2020:2070; length(yrs) # elimination traj from 2020 to 2070.

# 1. Import population data including business plan phases
country_data <- read.csv(file="output/prepped_data.csv", stringsAsFactors=T)
countries = country_data$country

# Dog rabies incidence - endemic and elimination trajectories
# baseline = read.csv("data/baseline_ts_Gavi.csv") # Transmission baseline, high (+5%), low (-5%)
endemic <- read.csv("data/baseline_incidence_Gavi_final.csv") # endemic <- read.csv("data/baseline_incidence.csv")
vax = read.csv("data/vax_obs_ts_gavi_norm.csv") # Vaccination timeseries

# Recalculate to annual cases
horizon = seq(1, nrow(vax), 12)
case_traj=rep(0, length(2020:2035))
cases_yr = matrix(0, nrow=length(case_traj), ncol=ncol(vax))

# VACCINATION - intermediate
for(j in 1:ncol(vax)){ # each run
  for(i in 1:length(horizon)){case_traj[i] = sum(vax[horizon[i]:(horizon[i]+11),j])}
  cases_yr[,j] <- case_traj
}

pop = 55827 # estimated dogs in 2012
lambda = 1.032173
denom = pop*lambda^(0:(nrow(cases_yr)-1))
inc_traj = cases_yr/denom

# Write to csv
write.csv(inc_traj, "output/incidence_trajectories.csv", row.names=FALSE)

control_traj = inc_traj[c(3:16,16,16),] # only start trajectory from the year of mass dog vax intro
elimination <- c(apply(control_traj, 1, mean), 0, 0)

plot(1:length(denom), inc_traj[,1], type="l", ylim=c(0,0.04))
for(i in 1:100){lines(1:length(denom), inc_traj[,i])}
for(i in 1:100){lines(1:length(denom), control_traj[,i], col="red")}

# Calculate HDR to generate predictions of dog pop from 2018 to 2070
HDR = as.numeric(gsub(",", "", country_data$pop2015))/country_data$total_dogs
y1 = grep("pop2018", names(country_data))
pop = country_data[, y1:(length(yrs)+y1+2-1)]
dogs = pop/HDR

# Replace column names - pop with dogpop
colnames(dogs) <- gsub(x = colnames(dogs), pattern = "\\pop", replacement = "dogpop")

# Append country name
dogs$country <- countries

# Write to csv
write.csv(dogs, file="output/dogs_pop_traj.csv", row.names=FALSE)

# Business plan phases
# Phase I - 2018-2020; Phase II - 2021-2025; Phase III - 2026-2030
yrs = 2018:2035
country_data$Business.Plan.Phase <- as.character(country_data$Business.Plan.Phase)
country_data$Business.Plan.Phase[which(is.na(country_data$Business.Plan.Phase))] <- 0

# Add countries in LAC that have good dog vaccination campaigns to phase 0 i.e. dog rabies incidence trajectories look good
country_data$Business.Plan.Phase[which(country_data$cluster=="americas" & country_data$Business.Plan.Phase=="II")] <- 0

I = which(country_data$Business.Plan.Phase=="I")
II = which(country_data$Business.Plan.Phase=="II")
III = which(country_data$Business.Plan.Phase=="III")

# Simulate incidence of rabid dog bites
# 1. choose the Business plan phase
phaseIa = match(2018:2019, yrs); phaseIb = match(2020:2035, yrs) # Progress starts in 2020
phaseIIa = match(2018:2022, yrs); phaseIIb = match(2023:2035, yrs) # Progress starts in 2023
phaseIIIa = match(2018:2026, yrs); phaseIIIb = match(2027:2035, yrs) # Progress starts in 2027
endgame = match(2018:(2018+10), yrs)

# RABID - create a matrix of incidence
rabid = matrix(endemic$inc, nrow=nrow(country_data), ncol=length(yrs)) # start off with endemic
rabid[which(country_data$endemic==0),]<-0 # set rabies free countries to zero

for(i in 1:length(countries)){
  if(country_data$Business.Plan.Phase[i] == "I"){rabid[i,phaseIb] <- elimination[1:length(phaseIb)]}
  if(country_data$Business.Plan.Phase[i] == "II"){rabid[i,phaseIIb] <- elimination[1:length(phaseIIb)]}
  if(country_data$Business.Plan.Phase[i] == "III"){rabid[i,phaseIIIb] <- elimination[1:length(phaseIIIb)]}
  if(country_data$Business.Plan.Phase[i] =="0" & country_data$endemic[i]==1) {
    rabid[i,] <- 0
    rabid[i,endgame] <- elimination[1:11]
    }
}
rabid <- as.data.frame(rabid)
names(rabid)=2018:2035

plot(yrs, rep(0, length(yrs)), ylim=c(0,2500))
for(i in 1:length(countries)){lines(yrs, rabid[i,1:length(yrs)]*100000)}
rabid$country=country_data$country

# Create a matrix of incidence for different phases
trajI = trajII = trajIII = traj0 = as.data.frame(matrix(NA, nrow=100, ncol=length(yrs))) # start off with endemic
names(trajI) = names(trajII) = names(trajIII) = names(traj0) = yrs
trajI[, phaseIb] = t(control_traj[1:length(phaseIb),])
trajII[, phaseIIb] = t(control_traj[1:length(phaseIIb),])
trajIII[, phaseIIIb] = t(control_traj[1:length(phaseIIIb),])
traj0[,endgame] <- t(control_traj[1:11,]); traj0[,which(is.na(traj0[1,]))] <- 0
trajI$phase = "I"
trajII$phase = "II"
trajIII$phase = "III"
traj0$phase = "0"
traj = rbind(trajI, trajII, trajIII, traj0)

# Write to csv
write.csv(traj, file="output/rabies_traj.csv", row.names=FALSE)

# Plot to check
plot(2020:2035, traj[1,3:18], type="l", ylim=c(0,0.04))
for(i in 1:300){lines(2020:2035, traj[i,3:18])}

# Create rabid dog bites from rabies incidence
y1 = "2020"; yN = "2035"
rabid_dog_bites = 0.38 # Bites per rabid dog
RD = dogs[,grep(y1, names(dogs)):grep(yN, names(dogs))]*rabid[,grep(y1, names(rabid)):grep(yN, names(rabid))]
RDB = RD * rabid_dog_bites
RDB$country=country_data$country

# Write to csv
write.csv(RDB, file="output/rabid_dog_bites_2020_2035.csv", row.names=FALSE)
