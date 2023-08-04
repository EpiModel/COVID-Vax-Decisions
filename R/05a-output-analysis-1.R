
# Setup -------------------------------------------------------------------

table2 <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                  "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                  "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5"),
                     Age1Dose1 = NA, Age1Dose2 = NA, Age1Dose3 = NA, Age1Dose4 = 0,
                     Age2Dose1 = NA, Age2Dose2 = NA, Age2Dose3 = NA, Age2Dose4 = NA,
                     Age3Dose1 = NA, Age3Dose2 = NA, Age3Dose3 = NA, Age3Dose4 = NA)

table3 <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                  "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                  "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5"),
                     CInc = NA, IncAverted = NA, PctAverted = NA, AvertedPerDose = NA)

table4 <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                  "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                  "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5"),
                     CDeaths = NA, DeathsAverted = NA, PctAverted = NA, AvertedPerDose = NA)

table2_fill <- function(sim, rownum, table2) {
  end_cov_vax1_18to64 <- round(quantile(as.numeric(sim$epi$cov_vax1_18to64[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax2_18to64 <- round(quantile(as.numeric(sim$epi$cov_vax2_18to64[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax3_18to49 <- round(quantile(as.numeric(sim$epi$cov_vax3_18to49[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  
  end_cov_vax3_50to64 <- round(quantile(as.numeric(sim$epi$cov_vax3_50to64[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax4_50to64 <- round(quantile(as.numeric(sim$epi$cov_vax4_50to64[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  
  end_cov_vax1_65p <- round(quantile(as.numeric(sim$epi$cov_vax1_65p[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax2_65p <- round(quantile(as.numeric(sim$epi$cov_vax2_65p[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax3_65p <- round(quantile(as.numeric(sim$epi$cov_vax3_65p[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  end_cov_vax4_65p <- round(quantile(as.numeric(sim$epi$cov_vax4_65p[608, ]), probs = c(.25, 0.5, .75)), 3) * 100
  
  table2[rownum, 2] <- paste0(end_cov_vax1_18to64[2], " (", end_cov_vax1_18to64[1], ", ", end_cov_vax1_18to64[3], ")")
  table2[rownum, 3] <- paste0(end_cov_vax2_18to64[2], " (", end_cov_vax2_18to64[1], ", ", end_cov_vax2_18to64[3], ")")
  table2[rownum, 4] <- paste0(end_cov_vax3_18to49[2], " (", end_cov_vax3_18to49[1], ", ", end_cov_vax3_18to49[3], ")")
  
  table2[rownum, 6] <- paste0(end_cov_vax1_18to64[2], " (", end_cov_vax1_18to64[1], ", ", end_cov_vax1_18to64[3], ")")
  table2[rownum, 7] <- paste0(end_cov_vax2_18to64[2], " (", end_cov_vax2_18to64[1], ", ", end_cov_vax2_18to64[3], ")")
  table2[rownum, 8] <- paste0(end_cov_vax3_50to64[2], " (", end_cov_vax3_50to64[1], ", ", end_cov_vax3_50to64[3], ")")
  table2[rownum, 9] <- paste0(end_cov_vax4_50to64[2], " (", end_cov_vax4_50to64[1], ", ", end_cov_vax4_50to64[3], ")")
  
  table2[rownum, 10] <- paste0(end_cov_vax1_65p[2], " (", end_cov_vax1_65p[1], ", ", end_cov_vax1_65p[3], ")")
  table2[rownum, 11] <- paste0(end_cov_vax2_65p[2], " (", end_cov_vax2_65p[1], ", ", end_cov_vax2_65p[3], ")")
  table2[rownum, 12] <- paste0(end_cov_vax3_65p[2], " (", end_cov_vax3_65p[1], ", ", end_cov_vax3_65p[3], ")")
  table2[rownum, 13] <- paste0(end_cov_vax4_65p[2], " (", end_cov_vax4_65p[1], ", ", end_cov_vax4_65p[3], ")")
  
  return(table2)
}

table4_fill <- function(sim, rownum, table4, ref_death_rate = NA, ref_death_count = NA, ref_doses = NA){
  deaths <- colSums(sim$epi$d.h.flow)
  pds <- colSums(sim$epi$num)
  death_rates <- deaths / pds * 100000
  death_stats <- quantile(death_rates, probs = c(.25, 0.5, .75))
  table4[rownum, 2] <- paste0(round(death_stats[2], 3), " (", round(death_stats[1], 3), ", ", round(death_stats[3], 3), ")")
  
  if (!is.na(ref_death_rate)){
    deaths_averted.1 <- round(ref_death_rate - death_stats[1], 3)
    deaths_averted.2 <- round(ref_death_rate - death_stats[2], 3)
    deaths_averted.3 <- round(ref_death_rate - death_stats[3], 3)
    
    table4[rownum, 3] <- paste0(deaths_averted.2, " (", deaths_averted.3, ", ", deaths_averted.1, ")")
    
    pct_averted.1 <- round((ref_death_rate - death_stats[1]) / ref_death_rate, 3) * 100
    pct_averted.2 <- round((ref_death_rate - death_stats[2]) / ref_death_rate, 3) * 100
    pct_averted.3 <- round((ref_death_rate - death_stats[3]) / ref_death_rate, 3) * 100
    
    table4[rownum, 4] <- paste0(pct_averted.2, " (", pct_averted.3, ", ", pct_averted.1, ")")
    
    total_doses <- colSums(sim$epi$nVax1) + colSums(sim$epi$nVax2) + colSums(sim$epi$nVax3) + colSums(sim$epi$nVax4) 
    addtl_doses <- total_doses - ref_doses
    total_deaths <- colSums(sim$epi$d.h.flow)
    total_deaths_averted <- ref_death_count - total_deaths 
    averted_per_dose <- total_deaths_averted / addtl_doses * 1000
    
    averted_per_dose <- round(quantile(averted_per_dose, probs = c(.25, 0.5, .75)), 1)
    
    table4[rownum, 5] <- paste0(averted_per_dose[2], " (", averted_per_dose[1], ", ", averted_per_dose[3], ")")
  }
  return(table4)
}

table3_fill <- function(sim, rownum, table3, ref_inf_rate = NA, ref_inf_count = NA, ref_doses = NA){
  inc <- colSums(sim$epi$se.flow)
  pd <- colSums(sim$epi$num)
  inc_rate <- inc / pd * 100000
  inc_stats <- quantile(inc_rate, probs = c(.25, 0.5, .75))
  table3[rownum, 2] <- paste0(round(inc_stats[2], 1), " (", round(inc_stats[1], 1), ", ", round(inc_stats[3], 1), ")")
  
  if (!is.na(ref_inf_rate)){
    inf_averted.1 <- round(ref_inf_rate - inc_stats[1], 1)
    inf_averted.2 <- round(ref_inf_rate - inc_stats[2], 1)
    inf_averted.3 <- round(ref_inf_rate - inc_stats[3], 1)
    
    table3[rownum, 3] <- paste0(inf_averted.2, " (", inf_averted.3, ", ", inf_averted.1, ")")
    
    pct_averted.1 <- round((ref_inf_rate - inc_stats[1]) / ref_inf_rate, 3) * 100
    pct_averted.2 <- round((ref_inf_rate - inc_stats[2]) / ref_inf_rate, 3) * 100
    pct_averted.3 <- round((ref_inf_rate - inc_stats[3]) / ref_inf_rate, 3) * 100
    
    table3[rownum, 4] <- paste0(pct_averted.2, " (", pct_averted.3, ", ", pct_averted.1, ")")
    
    total_doses <- colSums(sim$epi$nVax1) + colSums(sim$epi$nVax2) + colSums(sim$epi$nVax3) + colSums(sim$epi$nVax4) 
    addtl_doses <- total_doses - ref_doses
    total_inf <- colSums(sim$epi$se.flow)
    total_inf_averted <- ref_inf_count - total_inf
    averted_per_dose <- total_inf_averted / addtl_doses * 1000
    
    averted_per_dose <- round(quantile(averted_per_dose, probs = c(.25, 0.5, .75)), 1)
    
    table3[rownum, 5] <- paste0(averted_per_dose[2], " (", averted_per_dose[1], ", ", averted_per_dose[3], ")")
  }
  
  return(table3)
}

# Baseline -------------------------------------------------------------------

# Vaccine coverage by age group and dose
table2 <- table2_fill(baseline_sims, 1, table2)
ref_doses <- median(colSums(baseline_sims$epi$nVax1) + colSums(baseline_sims$epi$nVax2) + colSums(baseline_sims$epi$nVax3) + colSums(baseline_sims$epi$nVax4))

# Infections
table3 <- table3_fill(baseline_sims, 1, table3)
ref_inf_rate <- quantile(colSums(baseline_sims$epi$se.flow) / colSums(baseline_sims$epi$num) * 100000, probs = 0.5)
ref_inf_count <- quantile(colSums(baseline_sims$epi$se.flow), probs = 0.5)

# Deaths
table4 <- table4_fill(baseline_sims, 1, table4)
ref_death_rate <- quantile(colSums(baseline_sims$epi$d.h.flow) / colSums(baseline_sims$epi$num) * 100000, probs = 0.5)
ref_death_count <- quantile(colSums(baseline_sims$epi$d.h.flow), probs = 0.5)

# No Targeting Scenarios --------------------------------------------------

# sim.1010.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at baseline
# load and merge files
sim.1010 <- merge_simfiles(1010, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1010$param$hosp.nudge.prob
sim.1010$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1010, 2, table2)
# cases
table3 <- table3_fill(sim.1010, 2, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1010, 2, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.1055 <- merge_simfiles(1055, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1055$param$hosp.nudge.prob
sim.1055$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1055, 3, table2)
# cases
table3 <- table3_fill(sim.1055, 3, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1055, 3, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.1110 <- merge_simfiles(1110, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1110$param$hosp.nudge.prob
sim.1110$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1110, 4, table2)
# cases
table3 <- table3_fill(sim.1110, 4, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1110, 4, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.1065 <- merge_simfiles(1065, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1065$param$hosp.nudge.prob
sim.1065$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1065, 5, table2)
# cases
table3 <- table3_fill(sim.1065, 5, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1065, 5, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at at 0
# load and merge files
sim.1120 <- merge_simfiles(1120, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1120$param$hosp.nudge.prob
sim.1120$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1120, 6, table2)
# cases
table3 <- table3_fill(sim.1120, 6, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1120, 6, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[5] / 0.125
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses[121, ] <- c(1.0, 1.0, ref_doses)

# No Targeting Scenarios: Misc. NP varying --------------------------------------------------

# sim.1010.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at baseline
# load and merge files
sim.1010.2 <- merge_simfiles(1010, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1010.2$param$hosp.nudge.prob
sim.1010.2$param$bt.nudge.prob
sim.1010.2$param$misc.nudge.prob.1
sim.1010.2$param$misc.nudge.prob.2
sim.1010.2$param$misc.nudge.prob.3
# vaccine coverage
table2 <- table2_fill(sim.1010.2, 7, table2)
# cases
table3 <- table3_fill(sim.1010.2, 7, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1010.2, 7, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1055.X.rds - hosp.nudge prob at baseline, misc.nudge.prob halved
# load and merge files
sim.1055.2 <- merge_simfiles(1055, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1055.2$param$hosp.nudge.prob
sim.1055.2$param$bt.nudge.prob
sim.1055.2$param$misc.nudge.prob.1
sim.1055.2$param$misc.nudge.prob.2
sim.1055.2$param$misc.nudge.prob.3
# vaccine coverage
table2 <- table2_fill(sim.1055.2, 8, table2)
# cases
table3 <- table3_fill(sim.1055.2, 8, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1055.2, 8, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1110.X.rds - hosp.nudge prob at baseline, misc.nudge.prob at 0
# load and merge files
sim.1110.2 <- merge_simfiles(1110, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1110.2$param$hosp.nudge.prob
sim.1110.2$param$bt.nudge.prob
sim.1110.2$param$misc.nudge.prob.1
sim.1110.2$param$misc.nudge.prob.2
sim.1110.2$param$misc.nudge.prob.3
# vaccine coverage
table2 <- table2_fill(sim.1110.2, 9, table2)
# cases
table3 <- table3_fill(sim.1110.2, 9, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1110.2, 9, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1065.X.rds - hosp.nudge.prob doubled, misc.nudge.prob halved
# load and merge files
sim.1065.2 <- merge_simfiles(1065, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1065.2$param$hosp.nudge.prob
sim.1065.2$param$bt.nudge.prob
sim.1065.2$param$misc.nudge.prob.1
sim.1065.2$param$misc.nudge.prob.2
sim.1065.2$param$misc.nudge.prob.3
# vaccine coverage
table2 <- table2_fill(sim.1065.2, 10, table2)
# cases
table3 <- table3_fill(sim.1065.2, 10, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1065.2, 10, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.1120.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at at 0
# load and merge files
sim.1120.2 <- merge_simfiles(1120, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1120.2$param$hosp.nudge.prob
sim.1120.2$param$bt.nudge.prob
sim.1120.2$param$misc.nudge.prob.1
sim.1120.2$param$misc.nudge.prob.2
sim.1120.2$param$misc.nudge.prob.3
# vaccine coverage
table2 <- table2_fill(sim.1120.2, 11, table2)
# cases
table3 <- table3_fill(sim.1120.2, 11, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1120.2, 11, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots: Misc NP Varying -----------------------------------------------------------

cInc.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  temp <- merge_simfiles(i, indir = "data/output/no-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$misc.nudge.prob.1[5] / 0.12
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
  
}

cInc.2[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths.2[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses.2[121, ] <- c(1.0, 1.0, ref_doses)

library("viridis")
library("ggplot2")
library("metR")
library("patchwork")

# Plot cases
minval.i <- floor(min(min(cInc$cInc), min(cInc.2$cInc, na.rm = TRUE)))
maxval.i <- ceiling(max(max(cInc$cInc), max(cInc.2$cInc, na.rm = TRUE)))
g_inc_cont <- ggplot(cInc, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Breakthrough NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.i, maxval.i), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() 

g_inc_cont_2 <- ggplot(cInc.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Misc. NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.i, maxval.i)) + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.justification = "left") 

# Plot deaths
minval.d <- min(min(cDeaths$cDeaths), min(cDeaths.2$cDeaths, na.rm = TRUE))
maxval.d <- max(max(cDeaths$cDeaths), max(cDeaths.2$cDeaths, na.rm = TRUE))
g_death_cont <- ggplot(cDeaths, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Breakthrough NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.d, maxval.d), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() 

g_death_cont_2 <- ggplot(cDeaths.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Misc. NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.d, maxval.d)) + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.justification = "left") 

# Plot doses
minval.v <- min(min(cDoses$cDoses), min(cDoses.2$cDoses, na.rm = TRUE)) - 11
maxval.v <- max(max(cDoses$cDoses), max(cDoses.2$cDoses, na.rm = TRUE))
g_dose_cont <- ggplot(cDoses, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Breakthrough NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.v, maxval.v), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() 

g_dose_cont_2 <- ggplot(cDoses.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Misc. NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.v, maxval.v), labels = c("150'000", "160'000", "170'000", "180'000", "190'000", "200'000")) + 
   theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.justification = "left")

combined1 <- g_dose_cont + g_dose_cont_2 + plot_annotation(tag_levels = list(c("A1", "A2")), title = "Total Vaccine Doses Administered") 
combined2 <- g_inc_cont + g_inc_cont_2 + plot_annotation(tag_levels = list(c("B1", "B2")), title = "Infections per 100'000 Person-Days") 
combined3 <- g_death_cont + g_death_cont_2 + plot_annotation(tag_levels = list(c("C1", "C2")), title = "Deaths per 100'000 Person-Days")

wrap_elements(combined1) / wrap_elements(combined2) / wrap_elements(combined3)

ggsave("Figure3.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')
