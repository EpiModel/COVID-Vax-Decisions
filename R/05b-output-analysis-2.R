# Old Targeting Scenarios --------------------------------------------------

table2.supp <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                  "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                  "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5",
                                  "BNP - Old Targeting 1", "BNP - Old Targeting 2", "BNP - Old Targeting 3", 
                                  "BNP - Old Targeting 4", "BNP - Old Targeting 5", "MNP - Old Targeting 1", 
                                  "MNP - Old Targeting 2", "MNP - Old Targeting 3", "MNP - Old Targeting 4", "MNP - Old Targeting 5",
                                  "BNP - Young Targeting 1", "BNP - Young Targeting 2", "BNP - Young Targeting 3", 
                                  "BNP - Young Targeting 4", "BNP - Young Targeting 5", "MNP - Young Targeting 1", 
                                  "MNP - Young Targeting 2", "MNP - Young Targeting 3", "MNP - Young Targeting 4", "MNP - Young Targeting 5"),
                     Age1Dose1 = NA, Age1Dose2 = NA, Age1Dose3 = NA, Age1Dose4 = 0,
                     Age2Dose1 = NA, Age2Dose2 = NA, Age2Dose3 = NA, Age2Dose4 = NA,
                     Age3Dose1 = NA, Age3Dose2 = NA, Age3Dose3 = NA, Age3Dose4 = NA)
table2.supp[1:11, ] <- table2[1:11, ]

table3.supp <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                       "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                       "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5",
                                       "BNP - Old Targeting 1", "BNP - Old Targeting 2", "BNP - Old Targeting 3", 
                                       "BNP - Old Targeting 4", "BNP - Old Targeting 5", "MNP - Old Targeting 1", 
                                       "MNP - Old Targeting 2", "MNP - Old Targeting 3", "MNP - Old Targeting 4", "MNP - Old Targeting 5",
                                       "BNP - Young Targeting 1", "BNP - Young Targeting 2", "BNP - Young Targeting 3", 
                                       "BNP - Young Targeting 4", "BNP - Young Targeting 5", "MNP - Young Targeting 1", 
                                       "MNP - Young Targeting 2", "MNP - Young Targeting 3", "MNP - Young Targeting 4", "MNP - Young Targeting 5"),
                     CInc = NA, IncAverted = NA, PctAverted = NA, AvertedPerDose = NA)
table3.supp[1:11, ] <- table3[1:11, ]

table4.supp <- data.frame(scenario = c("Baseline", "BNP - No Targeting 1", "BNP - No Targeting 2", "BNP - No Targeting 3", 
                                       "BNP - No Targeting 4", "BNP - No Targeting 5", "MNP - No Targeting 1", 
                                       "MNP - No Targeting 2", "MNP - No Targeting 3", "MNP - No Targeting 4", "MNP - No Targeting 5",
                                       "BNP - Old Targeting 1", "BNP - Old Targeting 2", "BNP - Old Targeting 3", 
                                       "BNP - Old Targeting 4", "BNP - Old Targeting 5", "MNP - Old Targeting 1", 
                                       "MNP - Old Targeting 2", "MNP - Old Targeting 3", "MNP - Old Targeting 4", "MNP - Old Targeting 5",
                                       "BNP - Young Targeting 1", "BNP - Young Targeting 2", "BNP - Young Targeting 3", 
                                       "BNP - Young Targeting 4", "BNP - Young Targeting 5", "MNP - Young Targeting 1", 
                                       "MNP - Young Targeting 2", "MNP - Young Targeting 3", "MNP - Young Targeting 4", "MNP - Young Targeting 5"),
                     CDeaths = NA, DeathsAverted = NA, PctAverted = NA, AvertedPerDose = NA)
table4.supp[1:11, ] <- table4[1:11, ]

# sim.old.target1010.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at baseline
# load and merge files
sim.old.1010 <- merge_simfiles(1010, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1010$param$hosp.nudge.prob
sim.old.1010$param$bt.nudge.prob
# vaccine coverage
table2.supp <- table2_fill(sim.old.1010, 12, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1010, 12, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1010, 12, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.old.1055 <- merge_simfiles(1055, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1055$param$hosp.nudge.prob
sim.old.1055$param$bt.nudge.prob
# vaccine coverage
table2.supp <- table2_fill(sim.old.1055, 13, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1055, 13, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1055, 13, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.old.1110 <- merge_simfiles(1110, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1110$param$hosp.nudge.prob
sim.old.1110$param$bt.nudge.prob
# vaccine coverage
table2.supp <- table2_fill(sim.old.1110, 14, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1110, 14, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1110, 14, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.taget.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.old.1065 <- merge_simfiles(1065, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1065$param$hosp.nudge.prob
sim.old.1065$param$bt.nudge.prob
# vaccine coverage
table2.supp <- table2_fill(sim.old.1065, 15, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1065, 15, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1065, 15, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at 0
# load and merge files
sim.old.1120 <- merge_simfiles(1120, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1120$param$hosp.nudge.prob
sim.old.1120$param$bt.nudge.prob
# vaccine coverage
table2.supp <- table2_fill(sim.old.1120, 16, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1120, 16, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1120, 16, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

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


# Old Targeting Scenarios: Misc NP varying --------------------------------------------------

# sim.old.target1010.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at baseline
# load and merge files
sim.old.1010.2 <- merge_simfiles(1010, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1010.2$param$hosp.nudge.prob
sim.old.1010.2$param$bt.nudge.prob
sim.old.1010.2$param$misc.nudge.prob.1
sim.old.1010.2$param$misc.nudge.prob.2
sim.old.1010.2$param$misc.nudge.prob.3
# vaccine coverage
table2.supp <- table2_fill(sim.old.1010.2, 17, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1010.2, 17, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1010.2, 17, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1055.X.rds - hosp.nudge prob at baseline, misc.nudge.prob halved
# load and merge files
sim.old.1055.2 <- merge_simfiles(1055, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1055.2$param$hosp.nudge.prob
sim.old.1055.2$param$bt.nudge.prob
sim.old.1055.2$param$misc.nudge.prob.1
sim.old.1055.2$param$misc.nudge.prob.2
sim.old.1055.2$param$misc.nudge.prob.3
# vaccine coverage
table2.supp <- table2_fill(sim.old.1055.2, 18, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1055.2, 18, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1055.2, 18, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1110.X.rds - hosp.nudge prob at baseline, misc.nudge.prob at 0
# load and merge files
sim.old.1110.2 <- merge_simfiles(1110, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1110.2$param$hosp.nudge.prob
sim.old.1110.2$param$bt.nudge.prob
sim.old.1110.2$param$misc.nudge.prob.1
sim.old.1110.2$param$misc.nudge.prob.2
sim.old.1110.2$param$misc.nudge.prob.3
# vaccine coverage
table2.supp <- table2_fill(sim.old.1110.2, 19, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1110.2, 19, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1110.2, 19, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.taget.1065.X.rds - hosp.nudge.prob doubled, misc.nudge.prob halved
# load and merge files
sim.old.1065.2 <- merge_simfiles(1065, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1065.2$param$hosp.nudge.prob
sim.old.1065.2$param$bt.nudge.prob
sim.old.1065.2$param$misc.nudge.prob.1
sim.old.1065.2$param$misc.nudge.prob.2
sim.old.1065.2$param$misc.nudge.prob.3
# vaccine coverage
table2.supp <- table2_fill(sim.old.1065.2, 20, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1065.2, 20, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1065.2, 20, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.old.target.1120.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at 0
# load and merge files
sim.old.1120.2 <- merge_simfiles(1120, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1120.2$param$hosp.nudge.prob
sim.old.1120.2$param$bt.nudge.prob
sim.old.1120.2$param$misc.nudge.prob.1
sim.old.1120.2$param$misc.nudge.prob.2
sim.old.1120.2$param$misc.nudge.prob.3
# vaccine coverage
table2.supp <- table2_fill(sim.old.1120.2, 21, table2.supp)
# cases
table3.supp <- table3_fill(sim.old.1120.2, 21, table3.supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4.supp <- table4_fill(sim.old.1120.2, 21, table4.supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc.old.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.old.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.old.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/old-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$misc.nudge.prob.1[5] / 0.12
  
  if (is.na(scale.1) | (is.na(scale.2))) {
    stop("Missing parameter")
  }
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.old.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.old.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.old.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.old.2[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths.old.2[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses.old.2[121, ] <- c(1.0, 1.0, ref_doses)

# Plot cases
g_inc_cont_old <- ggplot(cInc.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
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

g_inc_cont_old_2 <- ggplot(cInc.old.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
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
g_death_cont_old <- ggplot(cDeaths.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Breakthrough NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.d, maxval.d), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed()

g_death_cont_old_2 <- ggplot(cDeaths.old.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
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
g_dose_cont_old <- ggplot(cDoses.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hosp. NP (% of Ref.)", y = "Breakthrough NP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval.v, maxval.v), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed()

g_dose_cont_old_2 <- ggplot(cDoses.old.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
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

combined1 <- g_dose_cont_old + g_dose_cont_old_2 + plot_annotation(tag_levels = list(c("A1", "A2")), title = "Total Vaccine Doses Administered") 
combined2 <- g_inc_cont_old + g_inc_cont_old_2 + plot_annotation(tag_levels = list(c("B1", "B2")), title = "Infections per 100'000 Person-Days") 
combined3 <- g_death_cont_old + g_death_cont_old_2 + plot_annotation(tag_levels = list(c("C1", "C2")), title = "Deaths per 100'000 Person-Days") 

wrap_elements(combined1) / wrap_elements(combined2) / wrap_elements(combined3)
ggsave("FigureA1.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')
