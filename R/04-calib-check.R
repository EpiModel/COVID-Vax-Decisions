
# Setup -------------------------------------------------------------------

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(dplyr)))
times <- data.frame(DayNum = c(788, 757, 726, 696, 665, 635, 604, 576, 545, 514, 
                               484, 453, 423, 392, 361, 331, 300, 270, 239, 211,
                               180, 149, 119, 88, 58, 27),
                    Month = c("Aug-22", "Jul-22", "Jun-22", "May-22", "Apr-22",
                              "Mar-22", "Feb-22", "Jan-22", "Dec-21", "Nov-21",
                              "Oct-21", "Sep-21", "Aug-21", "Jul-21", "Jun-21",
                              "May-21", "Apr-21", "Mar-21", "Feb-21", "Jan-21",
                              "Dec-20", "Nov-20", "Oct-20", "Sep-20", "Aug-20",
                              "Jul-20"))
times <- times %>% map_df(rev)

# Cases -------------------------------------------------------------------

allCases <- sim[["epi"]][["se.flow"]]
allCases[1, ] <- 0
allCases$ts <- 1:nrow(allCases)
allCases$cumAllCases1 <- cumsum(allCases$sim1)
allCases$cumAllCases2 <- cumsum(allCases$sim2)
allCases$cumAllCases3 <- cumsum(allCases$sim3)

cases <- merge(times, allCases, by.x = "DayNum", by.y = "ts")
cases$AllCases1 <- cases$cumAllCases1 - lag(cases$cumAllCases1)
cases$AllCases2 <- cases$cumAllCases2 - lag(cases$cumAllCases2)
cases$AllCases3 <- cases$cumAllCases3 - lag(cases$cumAllCases3)
cases$AllCases1[1] <- cases$cumAllCases1[1]
cases$AllCases2[1] <- cases$cumAllCases2[1]
cases$AllCases3[1] <- cases$cumAllCases3[1]
cases <- cases[7:26, c(1, 2, 9, 10, 11)]


# Deaths ------------------------------------------------------------------

d.h.flow <- sim[["epi"]][["d.h.flow"]]
d.h.flow[1,] <- 0
d.h.flow$ts <- 1:nrow(d.h.flow)
d.h.flow$cumDeaths1 <- cumsum(d.h.flow$sim1)
d.h.flow$cumDeaths2 <- cumsum(d.h.flow$sim2)
d.h.flow$cumDeaths3 <- cumsum(d.h.flow$sim3)

deaths <- merge(times, d.h.flow, by.x = "DayNum", by.y = "ts")
deaths$Deaths1 <- deaths$cumDeaths1 - lag(deaths$cumDeaths1)
deaths$Deaths2 <- deaths$cumDeaths2 - lag(deaths$cumDeaths2)
deaths$Deaths3 <- deaths$cumDeaths3 - lag(deaths$cumDeaths3)
deaths$Deaths1[1] <- deaths$cumDeaths1[1]
deaths$Deaths2[1] <- deaths$cumDeaths2[1]
deaths$Deaths3[1] <- deaths$cumDeaths3[1]
deaths <- deaths[7:26, c(1, 2 , 9, 10, 11)]

# Vaccine coverage --------------------------------------------------------

for (i in 1:3) {
  vax_cov <- data.frame(cov.vax1.0to4 = sim[["epi"]][["cov_vax1_0to4"]][, i],
                        cov.vax1.5to17 = sim[["epi"]][["cov_vax1_5to17"]][, i],
                        cov.vax1.18to64 = sim[["epi"]][["cov_vax1_18to64"]][, i],
                        cov.vax1.65p = sim[["epi"]][["cov_vax1_65p"]][, i],
                        cov.vax2.0to4 = sim[["epi"]][["cov_vax2_0to4"]][, i],
                        cov.vax2.5to17 = sim[["epi"]][["cov_vax2_5to17"]][, i],
                        cov.vax2.18to64 = sim[["epi"]][["cov_vax2_18to64"]][, i],
                        cov.vax2.65p = sim[["epi"]][["cov_vax2_65p"]][, i],
                        cov.vax3.5to17 = sim[["epi"]][["cov_vax3_5to17"]][, i],
                        cov.vax3.18to49 = sim[["epi"]][["cov_vax3_18to49"]][, i],
                        cov.vax3.50to64 = sim[["epi"]][["cov_vax3_50to64"]][, i],
                        cov.vax3.65p = sim[["epi"]][["cov_vax3_65p"]][, i],
                        cov.vax4.50to64 = sim[["epi"]][["cov_vax4_50to64"]][, i],
                        cov.vax4.65p = sim[["epi"]][["cov_vax4_65p"]][, i])
  vax_cov[is.na(vax_cov)] <- 0
  colnames(vax_cov) <- c("cov.vax1.0to4", "cov.vax1.5to17", "cov.vax1.18to64",
                         "cov.vax1.65p", "cov.vax2.0to4", "cov.vax2.5to17", 
                         "cov.vax2.18to64", "cov.vax2.65p", "cov.vax3.5to17",
                         "cov.vax3.18to49", "cov.vax3.50to64", "cov.vax3.65p",
                         "cov.vax4.50to64", "cov.vax4.65p")
  vax_cov <- as.data.frame(sapply(vax_cov, function(x) round(x, 3)*100))
  vax_cov$ts <- 1:nrow(vax_cov)
  vax_cov <- merge(times, vax_cov, by.x = "DayNum", by.y = "ts")
  assign(paste0("vax_cov_summary", i), vax_cov)
}
