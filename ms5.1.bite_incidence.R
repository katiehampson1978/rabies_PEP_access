################################################################################
#                       SUMMARISE BITE INCIDENCE ESTIMATES                     #
# use model generated incidence to get baseline bites by rabid & healthy dogs
# use country-specific patient data for regression to get Pseek (rabid & healthy}
# join pseek estimates and save: output/prepped_data_final.csv
# Generate predictions for plots
# NEED TO FIX VACCINE FREE FOR PLOTS!
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
rabies = read.csv("data/baseline_incidence_Gavi.csv") # new incidence from fitted model - with NO vaccination
y = length(2050:2020) # years
country_data <- read.csv("output/prepped_data.csv") ## import prepped_data
lambda = exp(log(country_data$pop2050/country_data$pop2020)/y); lambda # country specific population growth
dogs = country_data$total_dogs*lambda^5 # assume same lambda for dogs and retrieve the dog pop in 2020
rabid_dog_bites = 0.38 # Bites per rabid dog - mean
healthy_dog_bites = 1/5 # bites per healthy dog - complete guess for now!
rabid_dogs = dogs * rabies$inc # rabid dogs in each country in 2020
exposures = rabid_dogs * rabid_dog_bites # exposures due to rabid dogs
normal_bites = dogs * healthy_dog_bites # healthy dog bites
country_data$exp_inc = exposures/country_data$pop2020 # rabies exposure incidence
country_data$bite_inc = normal_bites/country_data$pop2020 # healthy dog bite incidence

# Upper and lower confidence intervals for exposure incidence & healthy bite incidence
RDB = c(0.38, 0.3, 0.64) # Bites per rabid dog - c(mean * CIs)
n = 1000
RBI = quantile(
  rnorm(n, mean = rabies$inc, sd = rabies$sd) *
    rtriangle(n=n, a=RDB[2], b=RDB[3], c=RDB[1]), probs = c(0.025, 0.5, 0.975))
hist(RBI[2]*100000 * dogs/country_data$pop2020) # check
country_data$LCI_exp_inc = dogs * RBI[1] / country_data$pop2020
country_data$UCI_exp_inc = dogs * RBI[3] / country_data$pop2020
country_data$LCI_bite_inc = dogs * 1/30 / country_data$pop2020
country_data$UCI_bite_inc = dogs * 1/3 / country_data$pop2020

################################################################################
#       Define Pseek when vaccine is free/charged & for healthy dogs           #
################################################################################
bites <- read.csv("output/bite_incidence.csv") # INCLUDES MADAGASCAR
bites$HDI = country_data$HDI[match(bites$country, country_data$country)]
bites$prop_urban = country_data$prop_urban[match(bites$country, country_data$country)] ## add prop urban
bites <- bites[complete.cases(bites$bite_incidence),] # only retain rows with non NA incidence
hdi <- bites[which(bites$source=="healthcare"),]

## for now, extract mean to summarise countries with multiple entries
hdi_nodupl <- hdi %>%
  group_by(country) %>%
  summarise(bite_incidence = mean(bite_incidence),
            vaccine_free=unique(vaccine_free),
            HDI=mean(HDI),
            prop_urban=unique(prop_urban))
full_df <- hdi_nodupl[complete.cases(hdi_nodupl),] ## retain only non NAs
full_df$bites_per_100000 <- round(full_df$bite_incidence*100000)

## use poisson or neg bin distribution?
distrib.pois <- fitdist(full_df$bites_per_100000, dist = "pois")
distrib.nb <- fitdist(full_df$bites_per_100000, dist = "nbinom")
logLik(distrib.pois); logLik(distrib.nb) ## Choose negative binomial

## Check whether the different explanatory variables are collinear.
# Variance Inflation Factors (VIF) are powerful because they detect multicollinearity
# and are generally preferred over Pearson correlation coefficients.
# Calculate the variance inflation factors (VIFs) for all covariates.
# Rule of thumb: a VIF close or > 3 is a sign of collinearity.
names(full_df)
## the explanatory variables to test for collinearity are: "HDI", "prop_urban"
corvif(full_df[,c("HDI", "prop_urban")]) ## no collinearity detected

## stepwise down model selection based on AIC
model0 <- glm.nb(formula = bites_per_100000 ~ HDI + prop_urban + vaccine_free, data=full_df)
model1 <- glm.nb(formula = bites_per_100000 ~  prop_urban + vaccine_free, data=full_df)
model2 <- glm.nb(formula = bites_per_100000 ~ HDI + vaccine_free, data=full_df)
model3 <- glm.nb(formula = bites_per_100000 ~ HDI + prop_urban, data=full_df)

AIC(model0) #  model0 is selected
AIC(model1)
AIC(model2)
AIC(model3)

##  diagnostic satisfactory?
plot(model0)
summary(model0)
results_df <-summary.glm(model0)$coefficients
exp(model0$coefficients)
write.csv(results_df, "output/results_bites_GLM_Mad.csv")

###################################
##      preds for countries      ##
###################################
## Generate predictions for all country in country_data
country_preds <- dplyr::select(country_data, country, CODE, HDI, gavi, prop_urban, vaccine_paid_by_patient)
country_preds$vaccine_free <- NA
country_preds$vaccine_free[which(country_preds$gavi==T)] <- FALSE
country_preds$vaccine_free[which(country_preds$gavi==F)] <- TRUE
country_preds$country[which(country_data$vaccine_paid_by_patient==FALSE)]
free = c("Bangladesh", "Bhutan", "Madagascar", "Nepal", "Pakistan", "Philippines", "Sri Lanka") # "Ghana","Lesotho", "Mongolia"
country_preds$vaccine_free[match(free, country_data$country)] <-TRUE
country_preds$vaccine_gavi <- TRUE # under gavi investment vaccine is free

## The model predictions - Status Quo
preds_status_quo_df <- predict(model0,
                               list(HDI = country_preds$HDI,
                                    prop_urban=country_preds$prop_urban,
                                    vaccine_free=country_preds$vaccine_free),
                               type = "link", se.fit=TRUE)

denom = 100000
country_preds$preds_SQ <- exp(preds_status_quo_df$fit)/denom
country_preds$preds_SQ_LCI <- exp(preds_status_quo_df$fit - 1.96 * preds_status_quo_df$se.fit)/denom
country_preds$preds_SQ_UCI <- exp(preds_status_quo_df$fit + 1.96 * preds_status_quo_df$se.fit)/denom

## The model predictions - assuming all Gavi countries provide PEP for free
preds_gavi_invest_df <- predict(model0,
                                list(HDI = country_preds$HDI,
                                     prop_urban=country_preds$prop_urban,
                                     vaccine_free=country_preds$vaccine_gavi),
                                type = "link", se.fit=TRUE)
country_preds$preds_gavi <- exp(preds_gavi_invest_df$fit)/denom
country_preds$preds_gavi_LCI <- exp(preds_gavi_invest_df$fit - 1.96 * preds_gavi_invest_df$se.fit)/denom
country_preds$preds_gavi_UCI <- exp(preds_gavi_invest_df$fit + 1.96 * preds_gavi_invest_df$se.fit)/denom

##       
sum(country_preds$preds_SQ[which(country_preds$gavi==TRUE)], na.rm=TRUE)
##       

rexp <- dplyr::select(country_data, country, exp_inc, bite_inc, LCI_exp_inc, UCI_exp_inc, LCI_bite_inc, UCI_bite_inc, p_seek)
country_preds <- dplyr::left_join(country_preds, rexp)

# STATUS QUO
country_preds$pseek_norm_SQ = (country_preds$preds_SQ - (country_preds$exp_inc * country_preds$p_seek))/country_preds$bite_inc
range(country_preds$pseek_norm_SQ, na.rm=TRUE)
country_preds$pseek_norm_SQ[which(country_preds$pseek_norm_SQ>1)] <- 1 #which(country_preds$pseek_norm_SQ>1)

#2) GAVI investment
country_preds$pseek_gavi = 0.9
country_preds$pseek_norm_gavi = (country_preds$preds_gavi - (country_preds$exp_inc * country_preds$pseek_gavi))/country_preds$bite_inc
country_preds$pseek_norm_gavi[which(country_preds$pseek_norm_gavi>1)] <- 1 #which(country_preds$pseek_norm_gavi>1)

#        Append results from bite incidence data to prepped country data       #
colstoselect <- c("country","vacc_free","vaccine_free_SQ","vaccine_gavi","preds_SQ",
                  "preds_SQ_LCI","preds_SQ_UCI","preds_gavi","preds_gavi_LCI",
                  "preds_gavi_UCI", "pseek_SQ", "pseek_gavi", "exp_inc",
                  "bite_inc", "LCI_exp_inc", "UCI_exp_inc",
                  "pseek_norm_SQ","pseek_norm_gavi",
                  "endemic") # pseek_norm_SQ_data",
## remove extra columns
colstoselect[which(! colstoselect %in% colnames(country_preds))]
colnames(country_preds)[which(colnames(country_preds) %in% colnames(country_data))]

country_preds <- country_preds[,c("country",
                  "vaccine_free","vaccine_gavi","preds_SQ",
                  "preds_SQ_LCI","preds_SQ_UCI","preds_gavi","preds_gavi_LCI",
                  "preds_gavi_UCI", "p_seek", "pseek_gavi",
                  "pseek_norm_SQ","pseek_norm_gavi")]
colnames(country_preds)[which(colnames(country_preds)=="p_seek")] <-"p_seek_SQ"
colnames(country_preds)[which(colnames(country_preds)=="vaccine_free")] <-"vaccine_free_SQ"

## merge the two datasets
country_data = dplyr::left_join(country_data, country_preds, by = "country")

################################################################################
# Write prepped data
write.csv(country_data, file="output/prepped_data_final.csv", row.names=FALSE)
################################################################################


# Generate predictions for plots:
###################
##      HDI      ##
###################
## Preds for HDI, keeping prop_urban constant (average), considering free/not free pep
HDI <- seq(min(full_df$HDI), max(full_df$HDI), 0.001)
avg_prop_urb <- rep(mean(full_df$prop_urban), length(HDI))

## free pep
preds_df_HDI_free <- predict(model0,
                             list(HDI = HDI, prop_urban=avg_prop_urb, vaccine_free=c(rep(TRUE, length(HDI)))),
                             type = "link", se.fit=TRUE)
preds_df_HDI_free <- as.data.frame(preds_df_HDI_free)

## not free pep
preds_df_HDI_pay <- predict(model0,
                            list(HDI = HDI, prop_urban=avg_prop_urb, vaccine_free=c(rep(FALSE, length(HDI)))),
                            type = "link", se.fit=TRUE)
preds_df_HDI_pay <- as.data.frame(preds_df_HDI_pay)

## bind the 2 predictions dataframes
preds_df_HDI <- rbind(preds_df_HDI_free, preds_df_HDI_pay)

vacc_cost <- c(rep("free", nrow(preds_df_HDI_free)), rep("not free", nrow(preds_df_HDI_pay)))
preds_df_HDI$vacc_cost <- vacc_cost
avg_prop_urb <- rep(mean(full_df$prop_urban), nrow(preds_df_HDI))
preds_df_HDI$prop_urban <- avg_prop_urb

preds_HDI <- exp(preds_df_HDI$fit)
preds_LCI <- exp(preds_df_HDI$fit - 1.96 * preds_df_HDI$se.fit)
preds_UCI <- exp(preds_df_HDI$fit + 1.96 * preds_df_HDI$se.fit)

pred_HDI_df <- data.frame("HDI"=HDI, "predicted_bite_incidence"=preds_HDI,
                          "LCI"=preds_LCI, "UCI"=preds_UCI, vacc_cost=vacc_cost,
                          prop_urban = avg_prop_urb)

##########################
##      prop_urban      ##
##########################
## Preds for prop_urban, keeping HDI constant (average), considering free/not free pep
prop_urb <- seq(min(full_df$prop_urban), max(full_df$prop_urban), 0.001)
avg_HDI <- rep(mean(full_df$HDI), length(prop_urb))

## free pep
preds_df_prop_urb_free <- predict(model0,
                                  list(HDI = avg_HDI, prop_urban=prop_urb, vaccine_free=c(rep(TRUE, length(prop_urb)))),
                                  type = "link", se.fit=TRUE)
preds_df_prop_urb_free <- as.data.frame(preds_df_prop_urb_free)

## not free pep
preds_df_prop_urb_pay <- predict(model0,
                                 list(HDI = avg_HDI, prop_urban=prop_urb, vaccine_free=c(rep(FALSE, length(prop_urb)))),
                                 type = "link", se.fit=TRUE)
preds_df_prop_urb_pay <- as.data.frame(preds_df_prop_urb_pay)

## bind the 2 predictions dataframes
preds_df_prop_urb <- rbind(preds_df_prop_urb_free, preds_df_prop_urb_pay)

vacc_cost <- c(rep("free", nrow(preds_df_prop_urb_free)), rep("not free", nrow(preds_df_prop_urb_pay)))
preds_df_prop_urb$vacc_cost <- vacc_cost
avg_HDI <- rep(mean(full_df$HDI), nrow(preds_df_prop_urb))
preds_df_prop_urb$HDI <- avg_HDI

preds_prop_urb <- exp(preds_df_prop_urb$fit)
preds_prop_urb_LCI <- exp(preds_df_prop_urb$fit - 1.96 * preds_df_prop_urb$se.fit)
preds_prop_urb_UCI <- exp(preds_df_prop_urb$fit + 1.96 * preds_df_prop_urb$se.fit)

preds_prop_urb_df <- data.frame("HDI"=avg_HDI, "predicted_bite_incidence"=preds_prop_urb,
                                "LCI"=preds_prop_urb_LCI, "UCI"=preds_prop_urb_UCI, vacc_cost=vacc_cost,
                                prop_urban = prop_urb)

########################  PLOTS   #########################################
cbPalette <- c("#D55E00", "#0072B2")

pdf("figs/fig4.2.pdf")

ggplot(country_preds, aes(x=HDI, y=pseek_norm_SQ, col=vaccine_free, fill=vaccine_free)) +
  geom_point(aes(fill = vaccine_free, shape=vaccine_free)) +
  labs(x = "HDI", y = "Probability of seeking care given healthy dog bites") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        #axis.title = element_text(color="#666666", face="bold", size=14),
        legend.text=element_text(size=10)) +
  scale_fill_manual(values=cbPalette, labels = c("not free", "free")) +
  scale_colour_manual(values=cbPalette, labels = c("not free", "free")) +
  scale_shape_manual(values=c(16,17), labels = c("not free", "free")) +
  guides(fill=guide_legend(title="Vaccine cost"), col=guide_legend(title="Vaccine cost"),
         shape=guide_legend(title="Vaccine cost"))

dev.off()

cbPalette_inv <- c("#0072B2","#D55E00")

ggplot(country_preds, aes(x=HDI, y=pseek_norm_gavi, col=vaccine_gavi, fill=vaccine_gavi)) +
  geom_point(aes(fill = vaccine_gavi, shape=vaccine_gavi)) +
  labs(x = "HDI", y = "Probability of seeking care given healthy dog bites") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        #axis.title = element_text(color="#666666", face="bold", size=14),
        legend.text=element_text(size=10)) +
  scale_fill_manual(values=cbPalette_inv, labels = c("free", "not free")) +
  scale_colour_manual(values=cbPalette_inv, labels = c("free", "not free")) +
  scale_shape_manual(values=c(17, 16), labels = c("free", "not free")) +

  guides(fill=guide_legend(title="Vaccine cost"), col=guide_legend(title="Vaccine cost"),
         shape=guide_legend(title="Vaccine cost"))


pdf("figs/fig4.3.pdf")
cbPalette <- c("#D55E00", "#0072B2")

ggplot(country_preds, aes(x=prop_urban, y=pseek_norm_SQ, col=vaccine_free, fill=vaccine_free)) +
  geom_point(aes(fill = vaccine_free, shape=vaccine_free)) +
  labs(x = "Proportion urban", y = "Probability of seeking care given healthy dog bites") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        #axis.title = element_text(color="#666666", face="bold", size=14),
        legend.text=element_text(size=10)) +
  scale_fill_manual(values=cbPalette, labels = c("not free", "free")) +
  scale_colour_manual(values=cbPalette, labels = c("not free", "free")) +
  scale_shape_manual(values=c(16,17), labels = c("not free", "free")) +
  guides(fill=guide_legend(title="Vaccine cost"), col=guide_legend(title="Vaccine cost"),
         shape=guide_legend(title="Vaccine cost"))

dev.off()

ggplot(country_preds, aes(x=prop_urban, y=pseek_norm_gavi, col=vaccine_gavi, fill=vaccine_gavi)) +
  geom_point(aes(fill = vaccine_gavi, shape=vaccine_gavi)) +
  labs(x = "Proportion urban", y = "Probability of seeking care given healthy dog bites") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(color="#666666", face="bold", size=14),
        legend.text=element_text(size=10)) +
  scale_fill_manual(values=cbPalette_inv, labels = c("free", "not free")) +
  scale_colour_manual(values=cbPalette_inv, labels = c("free", "not free")) +
  scale_shape_manual(values=c(17, 16), labels = c("free", "not free")) +

  guides(fill=guide_legend(title="Vaccine cost"), col=guide_legend(title="Vaccine cost"),
         shape=guide_legend(title="Vaccine cost"))


