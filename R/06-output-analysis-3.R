
# Setup -------------------------------------------------------------------

library("EpiModel")

merge_simfiles <- function(simno, indir = "data/", vars = NULL,  truncate.at = NULL, verbose = TRUE) {
  
  fn <- list.files(indir, pattern = paste0("sim.young.target.", simno, ".[0-9].rds"), full.names = TRUE)
  
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

# Young Targeting Scenarios --------------------------------------------------

# sim.young.target1010.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at baseline
# load and merge files
sim.young.1010 <- merge_simfiles(1010, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1010$param$hosp.nudge.prob
sim.young.1010$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.young.1010, 12, table2)
# cases
table3 <- table3_fill(sim.young.1010, 12, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.young.1010, 12, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.young.target.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.young.1055 <- merge_simfiles(1055, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1055$param$hosp.nudge.prob
sim.young.1055$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.young.1055, 13, table2)
# cases
table3 <- table3_fill(sim.young.1055, 13, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.young.1055, 13, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.young.target.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.young.1110 <- merge_simfiles(1110, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1110$param$hosp.nudge.prob
sim.young.1110$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.young.1110, 14, table2)
# cases
table3 <- table3_fill(sim.young.1110, 14, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.young.1110, 14, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.young.taget.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.young.1065 <- merge_simfiles(1065, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1065$param$hosp.nudge.prob
sim.young.1065$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.young.1065, 15, table2)
# cases
table3 <- table3_fill(sim.young.1065, 15, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.young.1065, 15, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.young.target.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at 0
# load and merge files
sim.young.1120 <- merge_simfiles(1120, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.young.1120$param$hosp.nudge.prob
sim.young.1120$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.young.1120, 16, table2)
# cases
table3 <- table3_fill(sim.young.1120, 16, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.young.1120, 16, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses.young <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  temp <- merge_simfiles(i, indir = "data/output/young-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[3] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[3] / 0.125
  
  scenario.cInc <- median(colSums(temp$epi$se.flow)) / 1.67
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow)) / 1.67
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses.young[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc.young[121, ] <- c(1.0, 1.0, ref_inf / 1.67)
cInc.young$tertiles <- cut(cInc.young$cInc, 3, labels = c(0, 1, 2))

cDeaths.young[121, ] <- c(1.0, 1.0, ref_deaths / 1.67)
cDeaths.young$tertiles <- cut(cDeaths.young$cDeaths, 3, labels = c(0, 1, 2))

cDoses.young[121, ] <- c(1.0, 1.0, ref_doses)
cDoses.young$tertiles <- cut(cDoses.young$cDoses, 3, labels = c(0, 1, 2))

# Plot cases
g_inc_cont_young <- ggplot(cInc.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY") + ggtitle("Targeting Younger Adults (18 - 64)")

g_inc_dis_young <- ggplot(cInc.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY", labels = c("? - ?", "? - ?", "? - ?")) + ggtitle("Targeting Younger Adults (18 - 64)")

# Plot deaths
g_death_cont_young <- ggplot(cDeaths.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY") + ggtitle("Targeting Younger Adults (18 - 64)")

g_death_dis_young <- ggplot(cDeaths.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY", labels = c("? - ?", "? - ?", "? - ?")) + ggtitle("Targeting Younger Adults (18 - 64)")


# Plot doses
d_dose_cont_young <- ggplot(cDoses.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Doses") + ggtitle("Targeting Younger Adults (18 - 64)")

g_dose_dis_young <- ggplot(cDoses.young, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Hospitalization Nudge Probability (% of Baseline)", y = "Breakthrough Nudge Probability (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Doses", labels = c("? - ?", "? - ?", "? - ?")) + ggtitle("Targeting Younger Adults (18 - 64)")
