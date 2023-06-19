
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


# Create Plots ------------------------------------------------------------

g1 <- ggplot(data = plot_data1, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
    sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("Reference") + theme_classic()

g2 <- ggplot(data = plot_data2, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("HNP Doubled, BNP at Reference") + theme_classic()

g3 <- ggplot(data = plot_data3, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("HNP at Reference, BNP Halved") + theme_classic()

g4 <- ggplot(data = plot_data4, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("HNP at Reference, BNP at 0") + theme_classic()

g5 <- ggplot(data = plot_data5, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("HNP Doubled, BNP Halved") + theme_classic()

g6 <- ggplot(data = plot_data6, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 80, name = "Doses Administered")) + 
  geom_line(aes(y = cVax1 / 80, color = "Dose 1")) + 
  geom_line(aes(y = cVax2 / 80, color = "Dose 2")) + 
  geom_line(aes(y = cVax3 / 80, color = "Dose 3")) + 
  geom_line(aes(y = cVax4 /80, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  ggtitle("HNP Doubled, BNP at 0") + theme_classic()

combined <- g1 + g2 + g3 + g4 + g5 + g6 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect", nrow = 3)
#ggsave("Figure6-Option1.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')


# Take Differences --------------------------------------------------------

# Scenario 2
plot_data2$cVax1_diff <- plot_data2$cVax1 - plot_data1$cVax1
plot_data2$cVax2_diff <- plot_data2$cVax2 - plot_data1$cVax2
plot_data2$cVax3_diff <- plot_data2$cVax3 - plot_data1$cVax3
plot_data2$cVax4_diff <- plot_data2$cVax4 - plot_data1$cVax4

p2 <- ggplot(data = plot_data2, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 4, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("HNP Doubled, BNP at Reference - For All Adults") + theme_classic()

# Scenario 3
plot_data3$cVax1_diff <- plot_data3$cVax1 - plot_data1$cVax1
plot_data3$cVax2_diff <- plot_data3$cVax2 - plot_data1$cVax2
plot_data3$cVax3_diff <- plot_data3$cVax3 - plot_data1$cVax3
plot_data3$cVax4_diff <- plot_data3$cVax4 - plot_data1$cVax4

p3 <- ggplot(data = plot_data3, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 0.1, name = "Cumulative Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 0.1, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 0.1, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 0.1, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 0.1, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("HNP at Reference, BNP Halved - For All Adults") + theme_classic()

# Scenario 4
plot_data4$cVax1_diff <- plot_data4$cVax1 - plot_data1$cVax1
plot_data4$cVax2_diff <- plot_data4$cVax2 - plot_data1$cVax2
plot_data4$cVax3_diff <- plot_data4$cVax3 - plot_data1$cVax3
plot_data4$cVax4_diff <- plot_data4$cVax4 - plot_data1$cVax4

p4 <- ggplot(data = plot_data4, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 0.1, name = "Cumulative Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 0.1, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 0.1, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 0.1, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 0.1, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("HNP at Reference, BNP at 0 - For All Adults") + theme_classic()

# Scenario 5
plot_data5$cVax1_diff <- plot_data5$cVax1 - plot_data1$cVax1
plot_data5$cVax2_diff <- plot_data5$cVax2 - plot_data1$cVax2
plot_data5$cVax3_diff <- plot_data5$cVax3 - plot_data1$cVax3
plot_data5$cVax4_diff <- plot_data5$cVax4 - plot_data1$cVax4

p5 <- ggplot(data = plot_data5, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 4, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("HNP Doubled, BNP Halved - For All Adults") + theme_classic()

# Scenario 6
plot_data6$cVax1_diff <- plot_data6$cVax1 - plot_data1$cVax1
plot_data6$cVax2_diff <- plot_data6$cVax2 - plot_data1$cVax2
plot_data6$cVax3_diff <- plot_data6$cVax3 - plot_data1$cVax3
plot_data6$cVax4_diff <- plot_data6$cVax4 - plot_data1$cVax4

p6 <- ggplot(data = plot_data6, aes(x = Day, y = Cases)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous("Incident Infections", expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative Addtl. Doses")) + 
  geom_line(aes(y = cVax1_diff / 4, color = "Dose 1")) + 
  geom_line(aes(y = cVax2_diff / 4, color = "Dose 2")) + 
  geom_line(aes(y = cVax3_diff / 4, color = "Dose 3")) + 
  geom_line(aes(y = cVax4_diff / 4, color = "Dose 4")) + 
  scale_color_manual(values = colors, name = "") + 
  xlab("Date") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 608),
                     breaks = c(1, 182, 366, 547),
                     labels = c("1" = "01 Jan 21", "182" = "01 Jul 21", 
                                "366" = "01 Jan 22", "547" = "01 Jul 22")) +
  coord_cartesian(ylim=c(0, 900)) + 
  ggtitle("HNP Doubled, BNP at 0 - For All Adults") + theme_classic()

combined <- p2 + p3 + p4 + p5 + p6 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect", nrow = 3)
ggsave("Figure9.tiff", width = 12, height = 12, units = "in", dpi=300, compression = 'lzw')
