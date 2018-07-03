# Probability of developing rabies depending on where on the body bitten
# Created by Katie Hampson - 31 Jul 17
# transmission <- read.csv("data/tz_p_rabies_transmission.csv")
transmission <- read.csv("data/biological_param_p_infect.csv")
# CHECK
# p_head = transmission$prob_head
# p_arm = transmission$prob_arm
# p_trunk = transmission$prob_trunk
# p_leg = transmission$prob_leg
#
# pdeath_head = transmission$prob_death_given_head
# pdeath_arm = transmission$prob_death_given_arm
# pdeath_trunk = transmission$prob_death_given_trunk
# pdeath_leg = transmission$prob_death_given_leg

#(p_head * pdeath_head) + (p_arm * pdeath_arm) + (p_trunk * pdeath_trunk) + (p_leg * pdeath_leg)

mixture_model<-function(n){ # Cleaveland 2002 mixture model
  N = transmission$n_rabid.bites # N BITES

  # BITES by site
  n_head = transmission$n_head.bites
  n_arm = transmission$n_arm.bites
  n_trunk = transmission$n_trunk.bites
  n_leg = transmission$n_leg.bites

  # DEATHS by site
  death_head = transmission$n_deaths.head
  death_arm = transmission$n_deaths.arm
  death_trunk = transmission$n_deaths.trunk
  death_leg = transmission$n_deaths.leg

  # No PEP by site
  n_no.pep.head = transmission$n_no.pep.head
  n_no.pep.arm = transmission$n_no.pep.arm
  n_no.pep.trunk = transmission$n_no.pep.trunk
  n_no.pep.leg = transmission$n_no.pep.leg

  # random draw of probability of bite site
  prob_head <- rbinom(n=n, size=N, prob=n_head/N)/N
  prob_arm <- rbinom(n=n, size=N, prob=n_arm/N)/N
  prob_trunk <- rbinom(n=n, size=N, prob=n_trunk/N)/N
  prob_leg <- rbinom(n=n, size=N, prob=n_leg/N)/N

  # random draw of probability of death given bite site
  prob_death_given_head <- rbinom(n=n, size=n_no.pep.head, prob=death_head/n_no.pep.head)/n_no.pep.head
  prob_death_given_arm <- rbinom(n=n, size=n_no.pep.arm, prob=death_arm/n_no.pep.arm)/n_no.pep.arm
  prob_death_given_trunk <- rbinom(n=n, size=n_no.pep.trunk, prob=death_trunk/n_no.pep.trunk)/n_no.pep.trunk
  prob_death_given_leg <-  rbinom(n=n, size=n_no.pep.leg, prob=death_leg/n_no.pep.leg)/n_no.pep.leg

  prob_death_given_rabid_bite <- prob_head*prob_death_given_head +
    prob_arm*prob_death_given_arm+
    prob_trunk*prob_death_given_trunk+
    prob_leg*prob_death_given_leg

  return(prob_death_given_rabid_bite)
}



# TEST FOR A TRIANGLE DISTRIBUTION
mixture_model_triangle<-function(n, range = 0.05){ # Cleaveland 2002 mixture model
  p_head = transmission$prob_head
  p_arm = transmission$prob_arm
  p_trunk = transmission$prob_trunk
  p_leg = transmission$prob_leg

  pdeath_head = transmission$prob_death_given_head
  pdeath_arm = transmission$prob_death_given_arm
  pdeath_trunk = transmission$prob_death_given_trunk
  pdeath_leg = transmission$prob_death_given_leg

  prob_head<-rtriangle(n, p_head-range, p_head+range)
  prob_arm<-rtriangle(n, p_arm-range, p_arm+range)
  prob_trunk<-rtriangle(n, p_trunk-range, p_trunk+range)
  prob_leg<-rtriangle(n, p_leg-range, p_leg+range)

  prob_death_given_head<-rtriangle(n, pdeath_head-range, pdeath_head+range)
  prob_death_given_arm<-rtriangle(n, pdeath_arm-range, pdeath_arm+range)
  prob_death_given_trunk<-rtriangle(n, pdeath_trunk-range, pdeath_trunk+range)
  prob_death_given_leg<-rtriangle(n, pdeath_leg-range, pdeath_leg+range)

  prob_death_given_rabid_bite <- prob_head*prob_death_given_head +
    prob_arm*prob_death_given_arm+
    prob_trunk*prob_death_given_trunk+
    prob_leg*prob_death_given_leg

  return(prob_death_given_rabid_bite)
}

# prab = mixture_model(1000)
# quantile(prab, prob=c(0.025, 0.5, 0.975))
