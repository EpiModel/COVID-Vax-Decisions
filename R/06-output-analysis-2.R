
# Setup -------------------------------------------------------------------

library("EpiModel")

merge_simfiles <- function(simno, indir = "data/", vars = NULL,  truncate.at = NULL, verbose = TRUE) {
  
  fn <- list.files(indir, pattern = paste0("sim.old.target.", simno, ".[0-9].rds"), full.names = TRUE)
  
  if (length(fn) == 0) {
    stop("No files of that simno in the specified indir", call. = FALSE)
  }
  
  for (i in seq_along(fn)) {
    sim <- readRDS(fn[i])
    
    if (!is.null(truncate.at)) {
      sim <- truncate_sim(sim, truncate.at)
    }
    
    sim$network <- NULL
    sim$attr <- NULL
    sim$temp <- NULL
    sim$el <- NULL
    sim$p <- NULL
    
    if (inherits(sim, "list") && all(c("epi", "param", "control") %in% names(sim))) {
      class(sim) <- "netsim"
    }
    
    if (!is.null(vars)) {
      sim$epi <- sim$epi[vars]
      sim$stats <- NULL
      if (!is.null(sim$riskh)) {
        sim$riskh <- NULL
      }
    }
    
    if (i == 1) {
      out <- sim
    } else {
      out <- merge(out, sim, param.error = FALSE, keep.other = FALSE)
    }
    
    if (verbose == TRUE) {
      cat("File ", i, "/", length(fn), " Loaded ... \n", sep = "")
    }
  }
  
  return(out)
}

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
table3 <- table3_fill(sim.old.1010, 7, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1010, 7, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.old.target.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.old.1055 <- merge_simfiles(1055, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1055$param$hosp.nudge.prob
sim.old.1055$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1055, 8, table2)
# cases
table3 <- table3_fill(sim.old.1055, 8, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1055, 8, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.old.target.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.old.1110 <- merge_simfiles(1110, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1110$param$hosp.nudge.prob
sim.old.1110$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1110, 9, table2)
# cases
table3 <- table3_fill(sim.old.1110, 9, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1110, 9, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.old.taget.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.old.1065 <- merge_simfiles(1065, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1065$param$hosp.nudge.prob
sim.old.1065$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1065, 10, table2)
# cases
table3 <- table3_fill(sim.old.1065, 10, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1065, 10, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.old.target.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at 0
# load and merge files
sim.old.1120 <- merge_simfiles(1120, indir = "data/output/old-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.old.1120$param$hosp.nudge.prob
sim.old.1120$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.old.1120, 11, table2)
# cases
table3 <- table3_fill(sim.old.1120, 11, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.old.1120, 11, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

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
  
  scenario.cInc <- median(colSums(temp$epi$se.flow)) / 1.67
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow)) / 1.67
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.old[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.old[121, ] <- c(1.0, 1.0, ref_inf / 1.67)
cInc.old$tertiles <- cut(cInc.old$cInc, 3, labels = c(0, 1, 2))

cDeaths.old[121, ] <- c(1.0, 1.0, ref_deaths / 1.67)
cDeaths.old$tertiles <- cut(cDeaths.old$cDeaths, 3, labels = c(0, 1, 2))

cDoses.old[121, ] <- c(1.0, 1.0, ref_doses)
cDoses.old$tertiles <- cut(cDoses.old$cDoses, 3, labels = c(0, 1, 2))

# Plot cases
g_inc_cont_old <- ggplot(cInc.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY") + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

g_inc_dis_old <- ggplot(cInc.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY", labels = c("67'100 - 67'400", "67'400 - 67'700", "67'700 - 68'000")) + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

# Plot deaths
g_death_cont_old <- ggplot(cDeaths.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY") + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

g_death_dis_old <- ggplot(cDeaths.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY", labels = c("117 - 118", "118 - 120", "120 - 121")) + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()


# Plot doses
g_dose_cont_old <- ggplot(cDoses.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Doses") + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()

g_dose_dis_old <- ggplot(cDoses.old, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Doses", labels = c("149'700 - 149'800", "149'800 - 149'900", "149'900 - 150'000")) + ggtitle("Targeting Older Adults (65+)") +
  coord_fixed()