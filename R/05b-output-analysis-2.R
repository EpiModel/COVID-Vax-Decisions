# Old Targeting Scenarios --------------------------------------------------

# sim.old.target1010.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at baseline
# load and merge files
sim.old.1010 <- merge_simfiles(1010, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1010$param$hosp.nudge.prob
sim.old.1010$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1010, 7, table2)
# cases
table3 <- table3_fill(sim.old.1010, 7, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1010, 7, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.old.1055 <- merge_simfiles(1055, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1055$param$hosp.nudge.prob
sim.old.1055$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1055, 8, table2)
# cases
table3 <- table3_fill(sim.old.1055, 8, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1055, 8, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.old.1110 <- merge_simfiles(1110, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1110$param$hosp.nudge.prob
sim.old.1110$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1110, 9, table2)
# cases
table3 <- table3_fill(sim.old.1110, 9, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1110, 9, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.taget.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.old.1065 <- merge_simfiles(1065, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1065$param$hosp.nudge.prob
sim.old.1065$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1065, 10, table2)
# cases
table3 <- table3_fill(sim.old.1065, 10, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1065, 10, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at 0
# load and merge files
sim.old.1120 <- merge_simfiles(1120, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1120$param$hosp.nudge.prob
sim.old.1120$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1120, 11, table2)
# cases
table3 <- table3_fill(sim.old.1120, 11, table3, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1120, 11, table4, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc.old <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.old <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.old <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[5] / 0.125
  
  if (is.na(scale.1) | (is.na(scale.2))) {
    stop("Missing parameter")
  }
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.old[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths.old[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses.old[121, ] <- c(1.0, 1.0, ref_doses)

# Plot cases
g_inc_cont_old <- ggplot(cInc.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Reference)", y = "BNP (% of Reference)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PD") + 
  ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

# Plot deaths
g_death_cont_old <- ggplot(cDeaths.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Reference)", y = "BNP (% of Reference)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PD") + 
  ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

# Plot doses
g_dose_cont_old <- ggplot(cDoses.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Reference)", y = "BNP (% of Reference)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Doses") + 
  ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()
