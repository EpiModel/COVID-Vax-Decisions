
# Setup -------------------------------------------------------------------

library("EpiModel")

merge_simfiles <- function(simno, indir = "data/", vars = NULL,  truncate.at = NULL, verbose = TRUE) {
  
  fn <- list.files(indir, pattern = paste0("sim.", simno, ".[0-9].rds"), full.names = TRUE)
  
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

table2 <- data.frame(scenario = c("Baseline", "No Targeting 1", "No Targeting 2", "No Targeting 3", 
                                  "No Targeting 4", "No Targeting 5", "Old Targeting 1", "Old Targeting 2", 
                                  "Old Targeting 3", "Old Targeting 4", "Old Targeting 5", "Young Targeting 1", 
                                  "Young Targeting 2", "Young Targeting 3", "Young Targeting 4", 
                                  "Young Targeting 5"),
                     Age1Dose1 = NA, Age1Dose2 = NA, Age1Dose3 = NA, Age1Dose4 = 0,
                     Age2Dose1 = NA, Age2Dose2 = NA, Age2Dose3 = NA, Age2Dose4 = NA,
                     Age3Dose1 = NA, Age3Dose2 = NA, Age3Dose3 = NA, Age3Dose4 = NA)

table3 <- data.frame(scenario = c("Baseline", "No Targeting 1", "No Targeting 2", "No Targeting 3", 
                                  "No Targeting 4", "No Targeting 5", "Old Targeting 1", "Old Targeting 2", 
                                  "Old Targeting 3", "Old Targeting 4", "Old Targeting 5", "Young Targeting 1", 
                                  "Young Targeting 2", "Young Targeting 3", "Young Targeting 4", 
                                  "Young Targeting 5"),
                     CInc = NA, IncAverted = NA, PctAverted = NA, AvertedPerDose = NA)

table4 <- data.frame(scenario = c("Baseline", "No Targeting 1", "No Targeting 2", "No Targeting 3", 
                                  "No Targeting 4", "No Targeting 5", "Old Targeting 1", "Old Targeting 2", 
                                  "Old Targeting 3", "Old Targeting 4", "Old Targeting 5", "Young Targeting 1", 
                                  "Young Targeting 2", "Young Targeting 3", "Young Targeting 4", 
                                  "Young Targeting 5"),
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

table4_fill <- function(sim, rownum, table4, ref_deaths = NA, ref_doses = NA){
  deaths <- sim$epi$d.h.flow
  cDeaths <- quantile(colSums(deaths), probs = c(.25, 0.5, .75))
  cDeaths_scaled <- round(cDeaths / 1.67, 1)
  table4[rownum, 2] <- paste0(cDeaths_scaled[2], " (", cDeaths_scaled[1], ", ", cDeaths_scaled[3], ")")
  
  if (!is.na(ref_deaths)){
    deaths_averted_scaled.1 <- round(ref_deaths / 1.67 - cDeaths_scaled[1], 1)
    deaths_averted_scaled.2 <- round(ref_deaths / 1.67 - cDeaths_scaled[2], 1)
    deaths_averted_scaled.3 <- round(ref_deaths / 1.67 - cDeaths_scaled[3], 1)
    
    table4[rownum, 3] <- paste0(deaths_averted_scaled.2, " (", deaths_averted_scaled.3, ", ", deaths_averted_scaled.1, ")")
    
    pct_averted.1 <- round((ref_deaths - cDeaths[1]) / ref_deaths, 3) * 100
    pct_averted.2 <- round((ref_deaths - cDeaths[2]) / ref_deaths, 3) * 100
    pct_averted.3 <- round((ref_deaths - cDeaths[3]) / ref_deaths, 3) * 100
    
    table4[rownum, 4] <- paste0(pct_averted.2, " (", pct_averted.3, ", ", pct_averted.1, ")")
    
    total_doses <- colSums(sim$epi$nVax1) + colSums(sim$epi$nVax2) + colSums(sim$epi$nVax3) + colSums(sim$epi$nVax4) 
    addtl_doses <- total_doses - ref_doses
    total_deaths <- colSums(sim$epi$d.h.flow)
    total_deaths_averted <- ref_deaths - total_deaths 
    averted_per_dose <- total_deaths_averted / addtl_doses
    
    averted_per_dose <- round(quantile(averted_per_dose, probs = c(.25, 0.5, .75)), 1)
    
    table4[rownum, 5] <- paste0(averted_per_dose[2], " (", averted_per_dose[1], ", ", averted_per_dose[3], ")")
  }
  return(table4)
}

table3_fill <- function(sim, rownum, table3, ref_inf = NA, ref_doses = NA){
  inc <- sim$epi$se.flow
  cInc <- quantile(colSums(inc), probs = c(.25, 0.5, .75))
  cInc_scaled <- round(cInc / 1.67, 1)
  table3[rownum, 2] <- paste0(cInc_scaled[2], " (", cInc_scaled[1], ", ", cInc_scaled[3], ")")
  
  if (!is.na(ref_inf)){
    inf_averted_scaled.1 <- round(ref_inf / 1.67 - cInc_scaled[1], 1)
    inf_averted_scaled.2 <- round(ref_inf / 1.67 - cInc_scaled[2], 1)
    inf_averted_scaled.3 <- round(ref_inf / 1.67 - cInc_scaled[3], 1)
    
    table3[rownum, 3] <- paste0(inf_averted_scaled.2, " (", inf_averted_scaled.3, ", ", inf_averted_scaled.1, ")")
    
    pct_averted.1 <- round((ref_inf - cInc[1]) / ref_inf, 3) * 100
    pct_averted.2 <- round((ref_inf - cInc[2]) / ref_inf, 3) * 100
    pct_averted.3 <- round((ref_inf - cInc[3]) / ref_inf, 3) * 100
    
    table3[rownum, 4] <- paste0(pct_averted.2, " (", pct_averted.3, ", ", pct_averted.1, ")")
    
    total_doses <- colSums(sim$epi$nVax1) + colSums(sim$epi$nVax2) + colSums(sim$epi$nVax3) + colSums(sim$epi$nVax4) 
    addtl_doses <- total_doses - ref_doses
    total_inf <- colSums(sim$epi$se.flow)
    total_inf_averted <- ref_inf - total_inf
    averted_per_dose <- total_inf_averted / addtl_doses
    
    averted_per_dose <- round(quantile(averted_per_dose, probs = c(.25, 0.5, .75)), 1)
    
    table3[rownum, 5] <- paste0(averted_per_dose[2], " (", averted_per_dose[1], ", ", averted_per_dose[3], ")")
  }
  
  return(table3)
}

# Baseline -------------------------------------------------------------------

baseline <- readRDS("data/output/baseline-sims.rds")
baseline <- truncate_sim(baseline, 181)

# Vaccine coverage by age group and dose
table2 <- table2_fill(baseline, 1, table2)
ref_doses <- median(colSums(baseline$epi$nVax1) + colSums(baseline$epi$nVax2) + colSums(baseline$epi$nVax3) + colSums(baseline$epi$nVax4))

# Infections
table3 <- table3_fill(baseline, 1, table3)
ref_inf <- quantile(colSums(baseline$epi$se.flow), probs = 0.5)

# Deaths
table4 <- table4_fill(baseline, 1, table4)
ref_deaths <- quantile(colSums(baseline$epi$d.h.flow), probs = 0.5)

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
table3 <- table3_fill(sim.1010, 2, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1010, 2, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.1055.X.rds - hosp.nudge prob at baseline, bt.nudge.prob halved
# load and merge files
sim.1055 <- merge_simfiles(1055, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1055$param$hosp.nudge.prob
sim.1055$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1055, 3, table2)
# cases
table3 <- table3_fill(sim.1055, 3, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1055, 3, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.1110.X.rds - hosp.nudge prob at baseline, bt.nudge.prob at 0
# load and merge files
sim.1110 <- merge_simfiles(1110, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1110$param$hosp.nudge.prob
sim.1110$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1110, 4, table2)
# cases
table3 <- table3_fill(sim.1110, 4, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1110, 4, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.1065.X.rds - hosp.nudge.prob doubled, bt.nudge.prob halved
# load and merge files
sim.1065 <- merge_simfiles(1065, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1065$param$hosp.nudge.prob
sim.1065$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1065, 5, table2)
# cases
table3 <- table3_fill(sim.1065, 5, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1065, 5, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# sim.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at at 0
# NOTE - hosp.flag tends to be triggered later in this scenario than when bt.nudge.prob is higher!!
# load and merge files
sim.1120 <- merge_simfiles(1120, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
# check params
sim.1120$param$hosp.nudge.prob
sim.1120$param$bt.nudge.prob
# vaccine coverage
table2 <- table2_fill(sim.1120, 6, table2)
# cases
table3 <- table3_fill(sim.1120, 6, table3, ref_inf = ref_inf, ref_doses = ref_doses)
# deaths
table4 <- table4_fill(sim.1120, 6, table4, ref_deaths = ref_deaths, ref_doses = ref_doses)

# Contour Plots -----------------------------------------------------------

cInc <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cInc = rep(NA, 121))
cDeaths <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDeaths = rep(NA, 121))
cDoses <- data.frame(hosp.nudge.prob.scale = rep(NA, 121), bt.nudge.prob.scale = rep(NA, 121), cDoses = rep(NA, 121))

for (i in 1001:1120) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/no-targeting-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[5] / 0.125
  
  scenario.cInc <- median(colSums(temp$epi$se.flow)) / 1.67
  scenario.cDeaths <- median(colSums(temp$epi$d.h.flow)) / 1.67
  scenario.cDoses <- median(colSums(temp$epi$nVax1) + colSums(temp$epi$nVax2) + colSums(temp$epi$nVax3) + colSums(temp$epi$nVax4))
  
  cInc[(i - 1000), ] <- c(scale.1, scale.2, scenario.cInc)
  cDeaths[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDeaths)
  cDoses[(i - 1000), ] <- c(scale.1, scale.2, scenario.cDoses)
  
  remove(temp)
}

cInc[121, ] <- c(1.0, 1.0, ref_inf / 1.67)
cInc$tertiles <- cut(cInc$cInc, 3, labels = c(0, 1, 2))

cDeaths[121, ] <- c(1.0, 1.0, ref_deaths / 1.67)
cDeaths$tertiles <- cut(cDeaths$cDeaths, 3, labels = c(0, 1, 2))

cDoses[121, ] <- c(1.0, 1.0, ref_doses)
cDoses$tertiles <- cut(cDoses$cDoses, 3, labels = c(0, 1, 2))

library("viridis")
library("ggplot2")
library("metR")

# Plot cases
g_inc_cont <- ggplot(cInc, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cInc), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY") + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()

g_inc_dis <- ggplot(cInc, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cInc), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cInc), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Inf / 100'000 PY", labels = c("67'100 - 67'400", "67'400 - 67'700", "67'700 - 67'900")) + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()

# Plot deaths
g_death_cont <- ggplot(cDeaths, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDeaths), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY") + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()

g_death_dis <- ggplot(cDeaths, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDeaths), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDeaths), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Deaths / 100'000 PY", labels = c("116 - 118", "118 - 120", "120 - 122")) + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()


# Plot doses
g_dose_cont <- ggplot(cDoses, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = cDoses), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "D", direction = 1, name = "Doses") + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()

g_dose_dis <- ggplot(cDoses, aes(hosp.nudge.prob.scale, bt.nudge.prob.scale)) +
  geom_raster(aes(fill = tertiles), interpolate = TRUE) +
  #geom_contour(aes(z = cDoses), col = "white", alpha = 0.5, lwd = 0.5) +
  #geom_text_contour(aes(z = cDoses), stroke = 0.1) +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "HNP  (% of Baseline)", y = "BNP (% of Baseline)") +
  scale_fill_viridis(discrete = TRUE, alpha = 1, option = "D", direction = 1, name = "Doses", labels = c("149'700 - 150'200", "150'200 - 150'800", "150'800 - 151'300")) + ggtitle("Targeting All Adults (18+)") +
  coord_fixed()
