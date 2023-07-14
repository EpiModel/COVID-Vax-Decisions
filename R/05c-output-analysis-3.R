
# Young Targeting Scenarios --------------------------------------------------

# sim.young.target1010.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at baseline
# load and merge files
sim.young.1010 <- merge_simfiles(1010, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1010$param$hosp.nudge.prob
sim.young.1010$param$bt.nudge.prob
# vaccine coverage
table2_supp <- table2_fill(sim.young.1010, 22, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1010, 22, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1010, 22, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.young.1055 <- merge_simfiles(1055, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1055$param$hosp.nudge.prob
sim.young.1055$param$bt.nudge.prob
# vaccine coverage
table2_supp <- table2_fill(sim.young.1055, 23, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1055, 23, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1055, 23, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.young.1110 <- merge_simfiles(1110, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1110$param$hosp.nudge.prob
sim.young.1110$param$bt.nudge.prob
# vaccine coverage
table2_supp <- table2_fill(sim.young.1110, 24, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1110, 24, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1110, 24, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.taget.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.young.1065 <- merge_simfiles(1065, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1065$param$hosp.nudge.prob
sim.young.1065$param$bt.nudge.prob
# vaccine coverage
table2_supp <- table2_fill(sim.young.1065, 25, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1065, 25, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1065, 25, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at 0
# load and merge files
sim.young.1120 <- merge_simfiles(1120, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1120$param$hosp.nudge.prob
sim.young.1120$param$bt.nudge.prob
# vaccine coverage
table2_supp <- table2_fill(sim.young.1120, 26, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1120, 26, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1120, 26, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[3] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[3] / 0.125
  
  if (is.na(scale.1) | (is.na(scale.2))) {
    stop("Missing parameter")
  }
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.young[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths.young[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses.young[121, ] <- c(1.0, 1.0, ref_doses)



# Young Targeting Scenarios: Misc NP varying --------------------------------------------------

# sim.young.target1010.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at baseline
# load and merge files
sim.young.1010.2 <- merge_simfiles(1010, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1010.2$param$hosp.nudge.prob
sim.young.1010.2$param$bt.nudge.prob
sim.young.1010.2$param$misc.nudge.prob.1
sim.young.1010.2$param$misc.nudge.prob.2
sim.young.1010.2$param$misc.nudge.prob.3
# vaccine coverage
table2_supp <- table2_fill(sim.young.1010.2, 27, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1010.2, 27, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1010.2, 27, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1055.X.rds - hosp.nudge prob at baseline, misc.nudge.prob halved
# load and merge files
sim.young.1055.2 <- merge_simfiles(1055, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1055.2$param$hosp.nudge.prob
sim.young.1055.2$param$bt.nudge.prob
sim.young.1055.2$param$misc.nudge.prob.1
sim.young.1055.2$param$misc.nudge.prob.2
sim.young.1055.2$param$misc.nudge.prob.3
# vaccine coverage
table2_supp <- table2_fill(sim.young.1055.2, 28, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1055.2, 28, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1055.2, 28, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1110.X.rds - hosp.nudge prob at baseline, misc.nudge.prob at 0
# load and merge files
sim.young.1110.2 <- merge_simfiles(1110, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1110.2$param$hosp.nudge.prob
sim.young.1110.2$param$bt.nudge.prob
sim.young.1110.2$param$misc.nudge.prob.1
sim.young.1110.2$param$misc.nudge.prob.2
sim.young.1110.2$param$misc.nudge.prob.3
# vaccine coverage
table2_supp <- table2_fill(sim.young.1110.2, 29, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1110.2, 29, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1110.2, 29, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.taget.1065.X.rds - hosp.nudge.prob doubled, misc.nudge.prob halved
# load and merge files
sim.young.1065.2 <- merge_simfiles(1065, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1065.2$param$hosp.nudge.prob
sim.young.1065.2$param$bt.nudge.prob
sim.young.1065.2$param$misc.nudge.prob.1
sim.young.1065.2$param$misc.nudge.prob.2
sim.young.1065.2$param$misc.nudge.prob.3
# vaccine coverage
table2_supp <- table2_fill(sim.young.1065.2, 30, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1065.2, 30, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1065.2, 30, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# sim.young.target.1120.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at 0
# load and merge files
sim.young.1120.2 <- merge_simfiles(1120, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1120.2$param$hosp.nudge.prob
sim.young.1120.2$param$bt.nudge.prob
sim.young.1120.2$param$misc.nudge.prob.1
sim.young.1120.2$param$misc.nudge.prob.2
sim.young.1120.2$param$misc.nudge.prob.3
# vaccine coverage
table2_supp <- table2_fill(sim.young.1120.2, 31, table2_supp)
# cases
table3_supp <- table3_fill(sim.young.1120.2, 31, table3_supp, ref_inf_rate = ref_inf_rate, ref_inf_count = ref_inf_count, ref_doses = ref_doses)
# deaths
table4_supp <- table4_fill(sim.young.1120.2, 31, table4_supp, ref_death_rate = ref_death_rate, ref_death_count = ref_death_count, ref_doses = ref_doses)

# Contour Plots: Misc NP Varying -----------------------------------------------------------

cInc.young.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.young.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.young.2 <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), misc.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/young-targeting-sims-2", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[3] / 0.102
  scale.2 <- temp$param$misc.nudge.prob.1[3] / 0.18
  
  if (is.na(scale.1) | (is.na(scale.2))) {
    stop("Missing parameter")
  }
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow) / colSums(temp$epi$num) * 100000)
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.young.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.young.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.young.2[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.young.2[121, ] <- c(1.0, 1.0, ref_inf_rate)
cDeaths.young.2[121, ] <- c(1.0, 1.0, ref_death_rate)
cDoses.young.2[121, ] <- c(1.0, 1.0, ref_doses)

# Plot cases
minval <- floor(min(min(cInc.young$cInc), min(cInc.young.2$cInc, na.rm = TRUE)))
maxval <- ceiling(max(max(cInc.young$cInc), max(cInc.young.2$cInc, na.rm = TRUE)))
g_inc_cont_young <- ggplot(cInc.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Ref.)", y = "BNP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed()

g_inc_cont_young_2 <- ggplot(cInc.young.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Reference)", y = "MNP (% of Reference)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval)) + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.justification = "left")

# Plot deaths
minval <- min(min(cDeaths.young$cDeaths), min(cDeaths.young.2$cDeaths, na.rm = TRUE))
maxval <- max(max(cDeaths.young$cDeaths), max(cDeaths.young.2$cDeaths, na.rm = TRUE))
g_death_cont_young <- ggplot(cDeaths.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Ref.)", y = "BNP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed()

g_death_cont_young_2 <- ggplot(cDeaths.young.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Ref.)", y = "MNP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval)) + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() + 
  theme(legend.position = "right", legend.justification = "left")

# Plot doses
minval <- min(min(cDoses.young$cDoses), min(cDoses.young.2$cDoses, na.rm = TRUE))
maxval <- max(max(cDoses.young$cDoses), max(cDoses.young.2$cDoses, na.rm = TRUE))
g_dose_cont_young <- ggplot(cDoses.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Ref.)", y = "BNP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval), guide = "none") + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed()

g_dose_cont_young_2 <- ggplot(cDoses.young.2, aes(hosp.nudge.prob.scale, misc.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Ref.)", y = "MNP (% of Ref.)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "", limits = c(minval, maxval)) + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2))) + 
  coord_fixed() +
  theme(legend.position = "right", legend.justification = "left")

combined1 <- g_dose_cont_young + g_dose_cont_young_2 + plot_annotation(tag_levels = list(c("A1", "A2")), title = "Total Vaccine Doses Administered") 
combined2 <- g_inc_cont_young + g_inc_cont_young_2 + plot_annotation(tag_levels = list(c("B1", "B2")), title = "Infections per 100'000 Person-Days") 
combined3 <- g_death_cont_young + g_death_cont_young_2 + plot_annotation(tag_levels = list(c("C1", "C2")), title = "Deaths per 100'000 Person-Days") 

wrap_elements(combined1) / wrap_elements(combined2) / wrap_elements(combined3)

ggsave("FigureA2.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')
