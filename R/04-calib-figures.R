# Setup -------------------------------------------------------------------

suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(patchwork)))
suppressWarnings(suppressMessages(library(EpiModelCOVID)))

times <- data.frame(DayNum = c(608, 577, 546, 516, 485, 455, 424, 396, 365, 334,
                               304, 273, 243, 212, 181, 151, 120, 90, 59, 31),
                    Month = c("Aug-22", "Jul-22", "Jun-22", "May-22", "Apr-22",
                              "Mar-22", "Feb-22", "Jan-22", "Dec-21", "Nov-21",
                              "Oct-21", "Sep-21", "Aug-21", "Jul-21", "Jun-21",
                              "May-21", "Apr-21", "Mar-21", "Feb-21", "Jan-21"))
times <- times %>% map_df(rev)

colors1 <- c("Result" = "#00BFC4", "Target (Empirical)" = "#F8766d")
colors <- c("Dose 1: Result" = "deepskyblue", "Dose 2: Result" = "chartreuse", 
            "Dose 3: Result" = "deeppink", "Dose 4: Result" = "purple",
            "Dose 1: Target" = "dodgerblue4", "Dose 2: Target" = "darkgreen", 
            "Dose 3: Target" = "darkred", "Dose 4: Target" = "darkorchid4")

merge_simfiles <- function(simno, indir = "data/", vars = NULL,  truncate.at = NULL, verbose = TRUE) {
  
  fn <- list.files(indir, pattern = paste0("sim.*", simno, ".[0-9].rds"), full.names = TRUE)
  
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

baseline_sims <- merge_simfiles(1000, indir = "data/output/no-targeting-sims", truncate.at = 181, verbose = TRUE)
baseline_sims$param$hosp.nudge.prob
baseline_sims$param$bt.nudge.prob

# Cases -------------------------------------------------------------------

case_targets <- c(8373, 3165, 1590, 1301, 784, 382, 1439, 7256, 6079,
                  1981, 958, 6259, 19544, 3194, 1278, 701, 1749, 2988,
                  4000, 3504)

for (i in 1:128){
  cases <- baseline_sims[["epi"]][["se.flow"]][, i]
  cases <- data.frame(ts = 1:length(cases), cases = cases)
  cases$cumCases <- cumsum(cases$cases)
  
  cases <- merge(times, cases, by.x = "DayNum", by.y = "ts")
  cases$cases <- cases$cumCases - lag(cases$cumCases)
  cases$cases[1] <- cases$cumCases[1]
  cases <- cases %>% rowid_to_column("ROW")
  assign(paste0("cases_", i), cases)
}

names <- paste0("cases_", 1:128)
cases_all_runs <- bind_rows(mget(names))
cases_all_runs <- cases_all_runs %>% group_by(ROW) %>% 
  summarise(Month = min(Month), minResult = min(cases), meanResult = mean(cases), maxResult = max(cases))

cases_all_runs$target <- case_targets

f1 <- ggplot(data = cases_all_runs, aes(x = ROW, y = minResult)) + 
  geom_ribbon(data = cases_all_runs, aes(ymin = minResult, ymax = maxResult), fill="#00BFC4", alpha=0.5) + 
  geom_line(aes(y = meanResult, color = "Result"), linewidth = 1) + 
  geom_point(aes(y = target, color = "Target (Empirical)"), size = 2) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 4, 7, 10, 13, 16, 19),
                     labels = c("1" = "Jan 21", "4" = "Apr 21", "7" = "Jul 21", 
                                "10" = "Oct 21", "13" = "Jan 22", "16" = "Apr 22",
                                "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 26500)) + 
  xlab("Month") +
  ylab("Incident Infections") +
  scale_color_manual(values = colors1, name = "") +
  ggtitle("Infections") + theme_classic()


# Hospitalizations --------------------------------------------------------

hosp_targets <- c(113, 47, 27, 24, 16, 9, 27, 98, 66, 24, 12, 44, 88, 31, 9, 5,
                  9, 19, 29, 26)

for (i in 1:128){
  hosp <- baseline_sims[["epi"]][["ich.flow"]][, i]
  hosp <- data.frame(ts = 1:length(hosp), hosp = hosp)
  hosp$cumHosp <- cumsum(hosp$hosp)
  
  hosp <- merge(times, hosp, by.x = "DayNum", by.y = "ts")
  hosp$hosp <- hosp$cumHosp - lag(hosp$cumHosp)
  hosp$hosp[1] <- hosp$cumHosp[1]
  hosp <- hosp %>% rowid_to_column("ROW")
  assign(paste0("hosp_", i), hosp)
}

names <- paste0("hosp_", 1:128)
hosp_all_runs <- bind_rows(mget(names))
hosp_all_runs <- hosp_all_runs %>% group_by(ROW) %>% 
  summarise(Month = min(Month), minResult = min(hosp), meanResult = mean(hosp), maxResult = max(hosp))

hosp_all_runs$target <- hosp_targets

f2 <- ggplot(data = hosp_all_runs, aes(x = ROW, y = minResult)) + 
  geom_ribbon(data = hosp_all_runs, aes(ymin = minResult, ymax = maxResult), fill="#00BFC4", alpha=0.5) + 
  geom_line(aes(y = meanResult, color = "Result"), linewidth = 1) + 
  geom_point(aes(y = target, color = "Target (Empirical)"), size = 2) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 4, 7, 10, 13, 16, 19),
                     labels = c("1" = "Jan 21", "4" = "Apr 21", "7" = "Jul 21", 
                                "10" = "Oct 21", "13" = "Jan 22", "16" = "Apr 22",
                                "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 150)) + 
  xlab("Month") +
  ylab("Incident Hospitalizations") +
  scale_color_manual(values = colors1, name = "") +
  ggtitle("Hospitalizations") + theme_classic()

# Deaths ------------------------------------------------------------------

death_targets <- c(25, 23, 14, 9, 5, 4, 2, 9, 26, 22, 8, 7, 11, 20, 13, 5, 2,
                   2, 3, 5 )

for (i in 1:128){
  death <- baseline_sims[["epi"]][["d.h.flow"]][, i]
  death <- data.frame(ts = 1:length(death), death = death)
  death$cumDeath <- cumsum(death$death)
  
  death <- merge(times, death, by.x = "DayNum", by.y = "ts")
  death$death <- death$cumDeath - lag(death$cumDeath)
  death$death[1] <- death$cumDeath[1] 
  death <- death %>% rowid_to_column("ROW")
  assign(paste0("death_", i), death)
}

names <- paste0("death_", 1:128)
death_all_runs <- bind_rows(mget(names))
death_all_runs <- death_all_runs %>% group_by(ROW) %>% 
  summarise(Month = min(Month), minResult = min(death), meanResult = mean(death), maxResult = max(death))

death_all_runs$target <- death_targets

f3 <- ggplot(data = death_all_runs, aes(x = ROW, y = minResult)) + 
  geom_ribbon(data = death_all_runs, aes(ymin = minResult, ymax = maxResult), fill="#00BFC4", alpha=0.5) + 
  geom_line(aes(y = meanResult, color = "Result"), linewidth = 1) + 
  geom_point(aes(y = target, color = "Target (Empirical)"), size = 2) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 4, 7, 10, 13, 16, 19),
                     labels = c("1" = "Jan 21", "4" = "Apr 21", "7" = "Jul 21", 
                                "10" = "Oct 21", "13" = "Jan 22", "16" = "Apr 22",
                                "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, 55)) + 
  xlab("Month") +
  ylab("Incident Deaths") +
  scale_color_manual(values = colors1, name = "") +
  ggtitle("Deaths") + theme_classic()

combined <- f1 + f2 + f3 & theme(legend.position = "bottom")
combined + plot_layout(ncol = 1, guides = "collect")
ggsave("Figure1.tiff", dpi=300, compression = 'lzw', width = 3000, height = 2400, units = "px")


# Vaccine Coverage --------------------------------------------------------
for (i in 1:128) {
  vax_cov <- data.frame(cov.vax1.0to4 = baseline_sims[["epi"]][["cov_vax1_0to4"]][, i],
                        cov.vax1.5to17 = baseline_sims[["epi"]][["cov_vax1_5to17"]][, i],
                        cov.vax1.18to64 = baseline_sims[["epi"]][["cov_vax1_18to64"]][, i],
                        cov.vax1.65p = baseline_sims[["epi"]][["cov_vax1_65p"]][, i],
                        cov.vax2.0to4 = baseline_sims[["epi"]][["cov_vax2_0to4"]][, i],
                        cov.vax2.5to17 = baseline_sims[["epi"]][["cov_vax2_5to17"]][, i],
                        cov.vax2.18to64 = baseline_sims[["epi"]][["cov_vax2_18to64"]][, i],
                        cov.vax2.65p = baseline_sims[["epi"]][["cov_vax2_65p"]][, i],
                        cov.vax3.5to17 = baseline_sims[["epi"]][["cov_vax3_5to17"]][, i],
                        cov.vax3.18to49 = baseline_sims[["epi"]][["cov_vax3_18to49"]][, i],
                        cov.vax3.50to64 = baseline_sims[["epi"]][["cov_vax3_50to64"]][, i],
                        cov.vax3.65p = baseline_sims[["epi"]][["cov_vax3_65p"]][, i],
                        cov.vax4.50to64 = baseline_sims[["epi"]][["cov_vax4_50to64"]][, i],
                        cov.vax4.65p = baseline_sims[["epi"]][["cov_vax4_65p"]][, i])
  vax_cov[is.na(vax_cov)] <- 0
  colnames(vax_cov) <- c("cov.vax1.0to4", "cov.vax1.5to17", "cov.vax1.18to64",
                         "cov.vax1.65p", "cov.vax2.0to4", "cov.vax2.5to17", 
                         "cov.vax2.18to64", "cov.vax2.65p", "cov.vax3.5to17",
                         "cov.vax3.18to49", "cov.vax3.50to64", "cov.vax3.65p",
                         "cov.vax4.50to64", "cov.vax4.65p")
  vax_cov <- as.data.frame(sapply(vax_cov, function(x) round(x, 3)*100))
  vax_cov$ts <- 1:nrow(vax_cov)
  vax_cov <- merge(times, vax_cov, by.x = "DayNum", by.y = "ts")
  vax_cov <- vax_cov %>% rowid_to_column("ROW")
  assign(paste0("vax_cov_summary_", i), vax_cov)
}

names <- paste0("vax_cov_summary_", 1:128)
vax_all_runs <- bind_rows(mget(names))

vax_all_runs <- vax_all_runs %>% group_by(ROW) %>% 
  summarise(mean.vax1.0to4 = mean(cov.vax1.0to4), 
            mean.vax1.5to17 = mean(cov.vax1.5to17), 
            mean.vax1.18to64 = mean(cov.vax1.18to64),
            mean.vax1.65p = mean(cov.vax1.65p),
            mean.vax2.0to4 = mean(cov.vax2.0to4),
            mean.vax2.5to17 = mean(cov.vax2.5to17),
            mean.vax2.18to64 = mean(cov.vax2.18to64),
            mean.vax2.65p = mean(cov.vax2.65p),
            mean.vax3.5to17 = mean(cov.vax3.5to17),
            mean.vax3.18to49 = mean(cov.vax3.18to49),
            mean.vax3.50to64 = mean(cov.vax3.50to64),
            mean.vax3.65p = mean(cov.vax3.65p),
            mean.vax4.50to64 = mean(cov.vax4.50to64),
            mean.vax4.65p = mean(cov.vax4.65p),
            min.vax1.0to4 = min(cov.vax1.0to4), 
            min.vax1.5to17 = min(cov.vax1.5to17), 
            min.vax1.18to64 = min(cov.vax1.18to64),
            min.vax1.65p = min(cov.vax1.65p),
            min.vax2.0to4 = min(cov.vax2.0to4),
            min.vax2.5to17 = min(cov.vax2.5to17),
            min.vax2.18to64 = min(cov.vax2.18to64),
            min.vax2.65p = min(cov.vax2.65p),
            min.vax3.5to17 = min(cov.vax3.5to17),
            min.vax3.18to49 = min(cov.vax3.18to49),
            min.vax3.50to64 = min(cov.vax3.50to64),
            min.vax3.65p = min(cov.vax3.65p),
            min.vax4.50to64 = min(cov.vax4.50to64),
            min.vax4.65p = min(cov.vax4.65p),
            max.vax1.0to4 = max(cov.vax1.0to4), 
            max.vax1.5to17 = max(cov.vax1.5to17), 
            max.vax1.18to64 = max(cov.vax1.18to64),
            max.vax1.65p = max(cov.vax1.65p),
            max.vax2.0to4 = max(cov.vax2.0to4),
            max.vax2.5to17 = max(cov.vax2.5to17),
            max.vax2.18to64 = max(cov.vax2.18to64),
            max.vax2.65p = max(cov.vax2.65p),
            max.vax3.5to17 = max(cov.vax3.5to17),
            max.vax3.18to49 = max(cov.vax3.18to49),
            max.vax3.50to64 = max(cov.vax3.50to64),
            max.vax3.65p = max(cov.vax3.65p),
            max.vax4.50to64 = max(cov.vax4.50to64),
            max.vax4.65p = max(cov.vax4.65p))

vax_targets <- rbind(vax_targets, c("February 2021", rep(NA, 14)))
vax_targets <- rbind(vax_targets, c("January 2021", rep(NA, 14)))
names(vax_targets) <- c("Month", "target.vax1.0to4", "target.vax1.5to17", 
                        "target.vax1.18to64", "target.vax1.65p", "target.vax2.0to4", 
                        "target.vax2.5to17", "target.vax2.18to64", "target.vax2.65p", 
                        "target.vax3.5to17", "target.vax3.18to49", "target.vax3.50to64", 
                        "target.vax3.65p", "target.vax4.50to64", "target.vax4.65p")
vax_targets <- vax_targets %>% mutate_at(c("target.vax1.0to4", "target.vax1.5to17", 
                                           "target.vax1.18to64", "target.vax1.65p", 
                                           "target.vax2.0to4", "target.vax2.5to17", 
                                           "target.vax2.18to64", "target.vax2.65p", 
                                           "target.vax3.5to17", "target.vax3.18to49", 
                                           "target.vax3.50to64", "target.vax3.65p", 
                                           "target.vax4.50to64", "target.vax4.65p"), as.numeric)
vax_targets <- vax_targets %>% map_df(rev)
vax_all_runs <- cbind(vax_all_runs, vax_targets)

# Age Group 1 -------------------------------------------------------------

vax_all_runs_1 <- vax_all_runs[, c("ROW", "mean.vax1.0to4", "mean.vax2.0to4", 
                                   "min.vax1.0to4",  "min.vax2.0to4", 
                                   "max.vax1.0to4", "max.vax2.0to4", 
                                   "target.vax1.0to4", "target.vax2.0to4")]

v1 <- ggplot(data = vax_all_runs_1, aes(x = ROW, y = mean.vax1.0to4)) + 
  geom_ribbon(data=vax_all_runs_1, aes(ymin = min.vax1.0to4, ymax = max.vax1.0to4), fill="deepskyblue", alpha = 0.5) + 
  geom_ribbon(data=vax_all_runs_1, aes(ymin = min.vax2.0to4, ymax = max.vax2.0to4), fill="chartreuse", alpha = 0.5) +
  geom_line(aes(y = mean.vax1.0to4, color = "Dose 1: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax2.0to4, color = "Dose 2: Result"), linewidth = 1) + 
  geom_line(aes(y = target.vax1.0to4, color = "Dose 1: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax2.0to4, color = "Dose 2: Target"), linewidth = 1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 7, 13, 19),
                     labels = c("1" = "Jan 21", "7" = "Jul 21",
                                "13" = "Jan 22", "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Month") +
  ylab("Vaccine Coverage (%)") +
  ggtitle("Ages 0 to 4") + 
  scale_color_manual(values = colors, guide = "none") +
  theme_classic()


# Age Group 2 -------------------------------------------------------------

vax_all_runs_2 <- vax_all_runs[, c("ROW", "mean.vax1.5to17", "mean.vax2.5to17", 
                                   "mean.vax3.5to17", "min.vax1.5to17", "min.vax2.5to17", 
                                   "min.vax3.5to17", "max.vax1.5to17", "max.vax2.5to17", 
                                   "max.vax3.5to17", "target.vax1.5to17", 
                                   "target.vax2.5to17", "target.vax3.5to17")]

v2 <- ggplot(data = vax_all_runs_2, aes(x = ROW, y = mean.vax1.5to17)) + 
  geom_ribbon(data=vax_all_runs_2, aes(ymin = min.vax1.5to17, ymax = max.vax1.5to17), fill="deepskyblue", alpha=0.5) + 
  geom_ribbon(data=vax_all_runs_2, aes(ymin = min.vax2.5to17, ymax = max.vax2.5to17), fill="chartreuse", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_2, aes(ymin = min.vax3.5to17, ymax = max.vax3.5to17), fill="deeppink", alpha=0.5) +
  geom_line(aes(y = mean.vax1.5to17, color = "Dose 1: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax2.5to17, color = "Dose 2: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax3.5to17, color = "Dose 3: Result"), linewidth = 1) +
  geom_line(aes(y = target.vax1.5to17, color = "Dose 1: Target"), linewidth = 1) + 
  geom_line(aes(y = target.vax2.5to17, color = "Dose 2: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax3.5to17, color = "Dose 3: Target"), linewidth = 1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 7, 13, 19),
                     labels = c("1" = "Jan 21", "7" = "Jul 21", 
                                "13" = "Jan 22", "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Month") +
  ylab("Vaccine Coverage (%)") +
  ggtitle("Ages 5 to 17") + 
  scale_color_manual(values = colors, guide = "none") +
  theme_classic()



# Age Group 3 -------------------------------------------------------------

vax_all_runs_3 <- vax_all_runs[, c("ROW", "mean.vax1.18to64", "mean.vax2.18to64", 
                                   "mean.vax3.18to49", "min.vax1.18to64", 
                                   "min.vax2.18to64", "min.vax3.18to49", 
                                   "max.vax1.18to64", "max.vax2.18to64", 
                                   "max.vax3.18to49", "target.vax1.18to64", 
                                   "target.vax2.18to64", "target.vax3.18to49")]

v3 <- ggplot(data = vax_all_runs_3, aes(x = ROW, y = mean.vax1.18to64)) + 
  geom_ribbon(data=vax_all_runs_3, aes(ymin = min.vax1.18to64, ymax = max.vax1.18to64), fill="deepskyblue", alpha=0.5) + 
  geom_ribbon(data=vax_all_runs_3, aes(ymin = min.vax2.18to64, ymax = max.vax2.18to64), fill="chartreuse", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_3, aes(ymin = min.vax3.18to49, ymax = max.vax3.18to49), fill="deeppink", alpha=0.5) +
  geom_line(aes(y = mean.vax1.18to64, color = "Dose 1: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax2.18to64, color = "Dose 2: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax3.18to49, color = "Dose 3: Result"), linewidth = 1) +
  geom_line(aes(y = target.vax1.18to64, color = "Dose 1: Target"), linewidth = 1) + 
  geom_line(aes(y = target.vax2.18to64, color = "Dose 2: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax3.18to49, color = "Dose 3: Target"), linewidth = 1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 7, 13, 19),
                     labels = c("1" = "Jan 21", "7" = "Jul 21", 
                                "13" = "Jan 22", "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Month") +
  ylab("Vaccine Coverage (%)") +
  ggtitle("Ages 18 to 49") + 
  scale_color_manual(values = colors, guide = "none") +
  theme_classic()


# Age Group 4 -------------------------------------------------------------

vax_all_runs_4 <- vax_all_runs[, c("ROW", "mean.vax1.18to64", "mean.vax2.18to64", 
                                   "mean.vax3.50to64", "mean.vax4.50to64", 
                                   "min.vax1.18to64", "min.vax2.18to64", 
                                   "min.vax3.50to64", "min.vax4.50to64", 
                                   "max.vax1.18to64", "max.vax2.18to64", 
                                   "max.vax3.50to64", "max.vax4.50to64", 
                                   "target.vax1.18to64", "target.vax2.18to64", 
                                   "target.vax3.50to64", "target.vax4.50to64")]

v4 <- ggplot(data = vax_all_runs_4, aes(x = ROW, y = mean.vax1.18to64)) + 
  geom_ribbon(data=vax_all_runs_4, aes(ymin = min.vax1.18to64, ymax = max.vax1.18to64), fill="deepskyblue", alpha=0.5) + 
  geom_ribbon(data=vax_all_runs_4, aes(ymin = min.vax2.18to64, ymax = max.vax2.18to64), fill="chartreuse", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_4, aes(ymin = min.vax3.50to64, ymax = max.vax3.50to64), fill="deeppink", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_4, aes(ymin = min.vax4.50to64, ymax = max.vax4.50to64), fill="purple", alpha=0.5) +
  geom_line(aes(y = mean.vax1.18to64, color = "Dose 1: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax2.18to64, color = "Dose 2: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax3.50to64, color = "Dose 3: Result"), linewidth = 1) +
  geom_line(aes(y = mean.vax4.50to64, color = "Dose 4: Result"), linewidth = 1) +
  geom_line(aes(y = target.vax1.18to64, color = "Dose 1: Target"), linewidth = 1) + 
  geom_line(aes(y = target.vax2.18to64, color = "Dose 2: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax3.50to64, color = "Dose 3: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax4.50to64, color = "Dose 4: Target"), linewidth = 1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 7, 13, 19),
                     labels = c("1" = "Jan 21", "7" = "Jul 21", 
                                "13" = "Jan 22", "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Month") +
  ylab("Vaccine Coverage (%)") +
  ggtitle("Ages 50 to 64") + 
  scale_color_manual(values = colors, guide = "none") +
  theme_classic()


# Age Group 5 -------------------------------------------------------------

vax_all_runs_5 <- vax_all_runs[, c("ROW", "mean.vax1.65p", "mean.vax2.65p", 
                                   "mean.vax3.65p", "mean.vax4.65p", "min.vax1.65p", 
                                   "min.vax2.65p", "min.vax3.65p", "min.vax4.65p", 
                                   "max.vax1.65p", "max.vax2.65p", "max.vax3.65p", 
                                   "max.vax4.65p", "target.vax1.65p", "target.vax2.65p", 
                                   "target.vax3.65p", "target.vax4.65p")]

v5 <- ggplot(data = vax_all_runs_5, aes(x = ROW, y = mean.vax1.65p)) + 
  geom_ribbon(data=vax_all_runs_5, aes(ymin = min.vax1.65p, ymax = max.vax1.65p), fill="deepskyblue", alpha=0.5) + 
  geom_ribbon(data=vax_all_runs_5, aes(ymin = min.vax2.65p, ymax = max.vax2.65p), fill="chartreuse", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_5, aes(ymin = min.vax3.65p, ymax = max.vax3.65p), fill="deeppink", alpha=0.5) +
  geom_ribbon(data=vax_all_runs_5, aes(ymin = min.vax4.65p, ymax = max.vax4.65p), fill="purple", alpha=0.5) +
  geom_line(aes(y = mean.vax1.65p, color = "Dose 1: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax2.65p, color = "Dose 2: Result"), linewidth = 1) + 
  geom_line(aes(y = mean.vax3.65p, color = "Dose 3: Result"), linewidth = 1) +
  geom_line(aes(y = mean.vax4.65p, color = "Dose 4: Result"), linewidth = 1) +
  geom_line(aes(y = target.vax1.65p, color = "Dose 1: Target"), linewidth = 1) + 
  geom_line(aes(y = target.vax2.65p, color = "Dose 2: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax3.65p, color = "Dose 3: Target"), linewidth = 1) +
  geom_line(aes(y = target.vax4.65p, color = "Dose 4: Target"), linewidth = 1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 20),
                     breaks = c(1, 7, 13, 19),
                     labels = c("1" = "Jan 21", "7" = "Jul 21", 
                                "13" = "Jan 22", "19" = "Jul 22")) +
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("Month") +
  ylab("Vaccine Coverage (%)") +
  ggtitle("Ages 65+") + 
  scale_color_manual(values = colors, name = "") + theme_classic() +
  theme(legend.position="bottom")

combined <- v1 + v2 + v3 + v4 + v5
combined + plot_layout(ncol = 3)
ggsave("Figure2.tiff", dpi=300, compression = 'lzw', width = 3000, height = 2400, units = "px")
rm(list=setdiff(ls(), c("est", "vax_targets", "mr_vec", "baseline_sims", "merge_simfiles")))
