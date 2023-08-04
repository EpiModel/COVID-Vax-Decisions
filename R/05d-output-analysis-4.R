
# Setup -------------------------------------------------------------------

library("EpiModel")
library("ggplot2")
library("patchwork")

createPlotData <- function(sim)
{
  cases <- sim[["epi"]][["se.flow"]]
  cases$median = apply(cases, 1, median, na.rm = TRUE)
  
  vax1 <- sim[["epi"]][["nVax1"]]
  vax1$median = apply(vax1, 1, median, na.rm = TRUE)
  vax1$cMedian = cumsum(vax1$median)

  vax2 <- sim[["epi"]][["nVax2"]]
  vax2$median = apply(vax2, 1, median, na.rm = TRUE)
  vax2$cMedian = cumsum(vax2$median)

  vax3 <- sim[["epi"]][["nVax3"]]
  vax3$median = apply(vax3, 1, median, na.rm = TRUE)
  vax3$cMedian = cumsum(vax3$median)

  vax4 <- sim[["epi"]][["nVax4"]]
  vax4$median = apply(vax4, 1, median, na.rm = TRUE)
  vax4$cMedian = cumsum(vax4$median)
  
  plot_data <- data.frame(Day = 1:608, Cases = cases$median, cVax1 = vax1$cMedian, cVax2 = vax2$cMedian, cVax3 = vax3$cMedian, cVax4 = vax4$cMedian)
  
  return(plot_data)
}

colors <- c("Dose 1" = "dodgerblue4", "Dose 2" = "darkgreen", "Dose 3" = "darkred", "Dose 4" = "darkorchid4")

# Baseline ----------------------------------------------------------------

plot_data1 <- createPlotData(baseline_sims)

# No targeting ------------------------------------------------------------

# hosp.nudge.prob doubled, bt.nudge.prob at baseline
sim.1010 <- merge_simfiles(1010, indir = "data/output/no-targeting-sims", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data2 <- createPlotData(sim.1010)

# hosp.nudge prob at baseline, bt.nudge.prob halved
sim.1055 <- merge_simfiles(1055, indir = "data/output/no-targeting-sims", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data3 <- createPlotData(sim.1055)

# hosp.nudge prob at baseline, bt.nudge.prob at 0
sim.1110 <- merge_simfiles(1110, indir = "data/output/no-targeting-sims", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data4 <- createPlotData(sim.1110)

# hosp.nudge.prob doubled, bt.nudge.prob halved
sim.1065 <- merge_simfiles(1065, indir = "data/output/no-targeting-sims", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data5 <- createPlotData(sim.1065)

# sim.1120.X.rds - hosp.nudge.prob doubled, bt.nudge.prob at at 0
sim.1120 <- merge_simfiles(1120, indir = "data/output/no-targeting-sims", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data6 <- createPlotData(sim.1120)


# hosp.nudge.prob doubled, misc.nudge.prob at baseline
sim.1010.2 <- merge_simfiles(1010, indir = "data/output/no-targeting-sims-2", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data7 <- createPlotData(sim.1010.2)

# hosp.nudge prob at baseline, misc.nudge.prob halved
sim.1055.2 <- merge_simfiles(1055, indir = "data/output/no-targeting-sims-2", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data8 <- createPlotData(sim.1055.2)

# hosp.nudge prob at baseline, misc.nudge.prob at 0
sim.1110.2 <- merge_simfiles(1110, indir = "data/output/no-targeting-sims-2", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data9 <- createPlotData(sim.1110.2)

# hosp.nudge.prob doubled, misc.nudge.prob halved
sim.1065.2 <- merge_simfiles(1065, indir = "data/output/no-targeting-sims-2", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data10 <- createPlotData(sim.1065.2)

# sim.1120.X.rds - hosp.nudge.prob doubled, misc.nudge.prob at 0
sim.1120.2 <- merge_simfiles(1120, indir = "data/output/no-targeting-sims-2", vars = NULL, truncate.at = 181, verbose = TRUE)
plot_data11 <- createPlotData(sim.1120.2)


# Take Differences --------------------------------------------------------

# Scenario 2
plot_data2$cVax1_diff <- plot_data2$cVax1 - plot_data1$cVax1
plot_data2$cVax2_diff <- plot_data2$cVax2 - plot_data1$cVax2
plot_data2$cVax3_diff <- plot_data2$cVax3 - plot_data1$cVax3
plot_data2$cVax4_diff <- plot_data2$cVax4 - plot_data1$cVax4

p2 <- ggplot(data = plot_data2, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4.5, name = "Cum. Addtl. Doses", labels = c("0", "1'000", "2'000", "3'000", "4'000"))) + 
  geom_line(aes(y = cVax1_diff / 4.5, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4.5, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4.5, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4.5, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("A: Hosp. NP Doubled") + theme_classic() + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

# Scenario 3
plot_data3$cVax1_diff <- plot_data3$cVax1 - plot_data1$cVax1
plot_data3$cVax2_diff <- plot_data3$cVax2 - plot_data1$cVax2
plot_data3$cVax3_diff <- plot_data3$cVax3 - plot_data1$cVax3
plot_data3$cVax4_diff <- plot_data3$cVax4 - plot_data1$cVax4

p3 <- ggplot(data = plot_data3, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 0.1, name = "Cum. Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 0.1, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 0.1, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 0.1, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 0.1, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("B: Breakthrough NP Halved") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

# Scenario 4
plot_data4$cVax1_diff <- plot_data4$cVax1 - plot_data1$cVax1
plot_data4$cVax2_diff <- plot_data4$cVax2 - plot_data1$cVax2
plot_data4$cVax3_diff <- plot_data4$cVax3 - plot_data1$cVax3
plot_data4$cVax4_diff <- plot_data4$cVax4 - plot_data1$cVax4

p4 <- ggplot(data = plot_data4, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 0.1, name = "Cum. Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 0.1, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 0.1, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 0.1, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 0.1, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("C: Breakthrough NP at 0") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

# Scenario 5
plot_data5$cVax1_diff <- plot_data5$cVax1 - plot_data1$cVax1
plot_data5$cVax2_diff <- plot_data5$cVax2 - plot_data1$cVax2
plot_data5$cVax3_diff <- plot_data5$cVax3 - plot_data1$cVax3
plot_data5$cVax4_diff <- plot_data5$cVax4 - plot_data1$cVax4

p5 <- ggplot(data = plot_data5, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4.5, name = "Cum. Addtl. Doses", labels = c("0", "1'000", "2'000", "3'000", "4'000"))) + 
  geom_line(aes(y = cVax1_diff / 4.5, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4.5, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4.5, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4.5, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("D: Hosp. NP Doubled and Breakthrough NP Halved") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

# Scenario 6
plot_data6$cVax1_diff <- plot_data6$cVax1 - plot_data1$cVax1
plot_data6$cVax2_diff <- plot_data6$cVax2 - plot_data1$cVax2
plot_data6$cVax3_diff <- plot_data6$cVax3 - plot_data1$cVax3
plot_data6$cVax4_diff <- plot_data6$cVax4 - plot_data1$cVax4

p6 <- ggplot(data = plot_data6, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4.5, name = "Cum. Addtl. Doses", labels = c("0", "1'000", "2'000", "3'000", "4'000"))) + 
  geom_line(aes(y = cVax1_diff / 4.5, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4.5, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4.5, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4.5, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("E: Hosp. NP Doubled and Breakthrough NP at 0") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

# Scenario 8
plot_data8$cVax1_diff <- plot_data8$cVax1 - plot_data1$cVax1
plot_data8$cVax2_diff <- plot_data8$cVax2 - plot_data1$cVax2
plot_data8$cVax3_diff <- plot_data8$cVax3 - plot_data1$cVax3
plot_data8$cVax4_diff <- plot_data8$cVax4 - plot_data1$cVax4

p8 <- ggplot(data = plot_data8, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 37, name = "Cum. Addtl. Doses", labels = c("0", "10'000", "20'000", "30'000"))) + 
  geom_line(aes(y = cVax1_diff / 37, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 37, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 37, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 37, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("F: Misc. NP Halved") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))


# Scenario 9
plot_data9$cVax1_diff <- plot_data9$cVax1 - plot_data1$cVax1
plot_data9$cVax2_diff <- plot_data9$cVax2 - plot_data1$cVax2
plot_data9$cVax3_diff <- plot_data9$cVax3 - plot_data1$cVax3
plot_data9$cVax4_diff <- plot_data9$cVax4 - plot_data1$cVax4

p9 <- ggplot(data = plot_data9, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 37, name = "Cum. Addtl. Doses", labels = c("0", "10'000", "20'000", "30'000"))) + 
  geom_line(aes(y = cVax1_diff / 37, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 37, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 37, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 37, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("G: Misc. NP at 0") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))


# Scenario 10
plot_data10$cVax1_diff <- plot_data10$cVax1 - plot_data1$cVax1
plot_data10$cVax2_diff <- plot_data10$cVax2 - plot_data1$cVax2
plot_data10$cVax3_diff <- plot_data10$cVax3 - plot_data1$cVax3
plot_data10$cVax4_diff <- plot_data10$cVax4 - plot_data1$cVax4

p10 <- ggplot(data = plot_data10, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 37, name = "Cum. Addtl. Doses", labels = c("0", "10'000", "20'000", "30'000"))) + 
  geom_line(aes(y = cVax1_diff / 37, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 37, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 37, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 37, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("H: Hosp. NP Doubled and Misc. NP Halved") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))



# Scenario 11
plot_data11$cVax1_diff <- plot_data11$cVax1 - plot_data1$cVax1
plot_data11$cVax2_diff <- plot_data11$cVax2 - plot_data1$cVax2
plot_data11$cVax3_diff <- plot_data11$cVax3 - plot_data1$cVax3
plot_data11$cVax4_diff <- plot_data11$cVax4 - plot_data1$cVax4

p11 <- ggplot(data = plot_data11, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity", color = "lightblue") + 
  scale_y_continuous("Incidence", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 37, name = "Cum. Addtl. Doses", labels = c("0", "10'000", "20'000", "30'000"))) + 
  geom_line(aes(y = cVax1_diff / 37, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 37, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 37, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 37, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "Jan 21", "182" = "Jul 21", 
                                "366" = "Jan 22", "547" = "Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("I: Hosp. NP Doubled and Misc. NP at 0") + theme_classic() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)))

combined <- p2 + p3 + p4 + p5 + p6 + p8 + p9 + p10 + p11 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect", nrow = 5)
ggsave("Figure4.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')
