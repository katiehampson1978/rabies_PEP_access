# Details of scenarios for Gavi support:

# high is from high performing countries (LMICs)
# base and low - those that do less well: low-high covers the range of countries that we have data for!

# Vaccine
p_seek_cap  <- data.frame(base = 0.9, low = 0.85, high = 0.95) # Maximum p_seek for rabid bite victims for each scenario
p_receive_cap <- data.frame(base = 0.93, low = 0.88, high = 0.98) # Maximum p_receive for bite victims for each scenario
p_complete_cap <- data.frame(base = 0.8, low = 0.75, high = 0.85) # Maximum p_complete for bite victims for each scenario

# p_step <- 0.1 # 10% increase on introduction of Gavi support
p_step <- data.frame(base = 0.1, low = 0.05, high = 0.15)
p_increment <- 0.03 # 3% per annum as per other Gavi vaccines

# RIG
p_RIG_increment <- 0.025 # improved access to RIG by 2.5% per year
p_RIG_cap <- 0.15 # Maximum of 15% access in urban areas ONLY

# Gavi phasing
gavi_phaseI = 2021
gavi_phaseII = 2022
gavi_phaseIII = 2026
gavi_phase0 = 2036

gavi_phasing = function(phase, p_SQ, step, increment, cap){
  p = rep(p_SQ, length(2020:2035)) # create vector of status quo probabilities
  if(p_SQ > cap){p} # make sure that the gavi investment is NEVER below the status quo!
  else
    if(p_SQ+step > cap){p[which(2020:2035 > phase)] <- cap } # make sure that the gavi investment is NEVER below the status quo!
  else{p[which(2020:2035 == phase)] <- p_SQ + step # step change with year of gavi introduction
  p_traj = c(seq(from = p_SQ + step, to = cap, by = increment), rep(cap, 15)) # subsequent trajectories of probabilities
  yrs = which(2020:2035 > phase) # fit the trajectory into the projection time horizon
  p[yrs] = p_traj[1:length(yrs)]
  }
  p
}
# 172 - Tanzania
# gavi_phasing(phase = gavi_phaseII, p_SQ = data$p_seek[30], step = p_step$base, increment = p_increment, cap = p_seek_cap$base)
# 14 - Bangladesh (has a high p_seek!)
# gavi_phasing(phase = gavi_phaseII, p_SQ = 0.92, step = p_step, increment = p_increment, cap = p_seek_cap$base)

# For p_seek_norm have to rework the cap!
# gavi_phasing(phase = gavi_phaseII, p_SQ = country_data$pseek_norm_SQ[30], step = p_step, increment = 0, cap = country_data$pseek_norm_SQ[30]+p_step)

# For p_receive
# gavi_phasing(phase = gavi_phaseII, p_SQ = country_data$p_receive[30], step = p_step, increment = p_increment, cap = p_receive_cap$base)
# gavi_phasing(phase = gavi_phaseII, p_SQ = data$p_receive[30], step = p_step$base, increment = p_increment, cap = p_receive_cap$base)
# For p_complete
# gavi_phasing(phase = gavi_phaseII, p_SQ = country_data$p_complete[1], step = p_step, increment = p_increment, cap = p_complete_cap$base)
# gavi_phasing(phase = gavi_phaseII, p_SQ = data$p_complete[30], step = p_step$base, increment = p_increment, cap = p_complete_cap$base)

# Introdution of new regimens....
gavi_intro = function(phase){
  support = rep("none", length(2020:2035))
  if(phase < 2036){support[which(2020:2035 >= phase)] <- "support"}
  support
}


# IBCM parameters
IBCM_endemic = 0.5
IBCM_elim = 0.1


#gavi_intro(gavi_phaseI)
# gavi_intro(gavi_phaseII)
# gavi_intro(gavi_phaseIII)



