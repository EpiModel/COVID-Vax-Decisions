
# Setup -------------------------------------------------------------------

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(dplyr)))
times <- data.frame(DayNum = c(608, 577, 546, 516, 485, 455, 424, 396, 365, 334, 
                               304, 273, 243, 212, 181, 151, 120, 90, 59, 31),
                    Month = c("Aug-22", "Jul-22", "Jun-22", "May-22", "Apr-22",
                              "Mar-22", "Feb-22", "Jan-22", "Dec-21", "Nov-21",
                              "Oct-21", "Sep-21", "Aug-21", "Jul-21", "Jun-21",
                              "May-21", "Apr-21", "Mar-21", "Feb-21", "Jan-21"))
times <- times %>% map_df(rev)

# Cases -------------------------------------------------------------------

nDx.pos <- sim[["epi"]][["nDx.pos"]]
nDx.pos$sim1[1] <- 0
nDx.pos$ts <- 1:nrow(nDx.pos)
nDx.pos$cumDxCases <- cumsum(nDx.pos$sim1)

allCases <- sim[["epi"]][["se.flow"]]
allCases$sim1[1] <- 0
allCases$ts <- 1:nrow(allCases)
allCases$cumAllCases <- cumsum(allCases$sim1)

cases <- merge(times, nDx.pos, by.x = "DayNum", by.y = "ts")
cases <- merge(cases, allCases, by.x = "DayNum", by.y = "ts")
cases$DxCases <- cases$cumDxCases - lag(cases$cumDxCases)
cases$DxCases[1] <- cases$cumDxCases[1]
cases$AllCases <- cases$cumAllCases - lag(cases$cumAllCases)
cases$AllCases[1] <- cases$cumAllCases[1]
cases <- cases[, c(1, 2, 7, 8)]


# Deaths ------------------------------------------------------------------

d.h.flow <- sim[["epi"]][["d.h.flow"]]
d.h.flow$sim1[1] <- 0
d.h.flow$ts <- 1:nrow(d.h.flow)
d.h.flow$cumDeaths <- cumsum(d.h.flow$sim1)

deaths <- merge(times, d.h.flow, by.x = "DayNum", by.y = "ts")
deaths$Deaths <- deaths$cumDeaths - lag(deaths$cumDeaths)
deaths$Deaths[1] <- deaths$cumDeaths[1]
deaths <- deaths[, c(1, 2 , 5)]

# Vaccine coverage --------------------------------------------------------

vax_cov <- data.frame(cov.vax1.0to4 = sim[["epi"]][["cov_vax1_0to4"]],
                      cov.vax1.5to17 = sim[["epi"]][["cov_vax1_5to17"]],
                      cov.vax1.18to64 = sim[["epi"]][["cov_vax1_18to64"]],
                      cov.vax1.65p = sim[["epi"]][["cov_vax1_65p"]],
                      cov.vax2.0to4 = sim[["epi"]][["cov_vax2_0to4"]],
                      cov.vax2.5to17 = sim[["epi"]][["cov_vax2_5to17"]],
                      cov.vax2.18to64 = sim[["epi"]][["cov_vax2_18to64"]],
                      cov.vax2.65p = sim[["epi"]][["cov_vax2_65p"]],
                      cov.vax3.5to17 = sim[["epi"]][["cov_vax3_5to17"]],
                      cov.vax3.18to49 = sim[["epi"]][["cov_vax3_18to49"]],
                      cov.vax3.50to64 = sim[["epi"]][["cov_vax3_50to64"]],
                      cov.vax3.65p = sim[["epi"]][["cov_vax3_65p"]],
                      cov.vax4.50to64 = sim[["epi"]][["cov_vax4_50to64"]],
                      cov.vax4.65p = sim[["epi"]][["cov_vax4_65p"]])
vax_cov[is.na(vax_cov)] <- 0
colnames(vax_cov) <- c("cov.vax1.0to4", "cov.vax1.5to17", "cov.vax1.18to64",
                       "cov.vax1.65p", "cov.vax2.0to4", "cov.vax2.5to17", 
                       "cov.vax2.18to64", "cov.vax2.65p", "cov.vax3.5to17",
                       "cov.vax3.18to49", "cov.vax3.50to64", "cov.vax3.65p",
                       "cov.vax4.50to64", "cov.vax4.65p")
vax_cov <- as.data.frame(sapply(vax_cov, function(x) round(x, 3)*100))
vax_cov$ts <- 1:nrow(vax_cov)
vax_cov <- merge(times, vax_cov, by.x = "DayNum", by.y = "ts")

# Combine and output ------------------------------------------------------

calib_results <- merge(cases, deaths)
calib_results <- merge(calib_results, vax_cov)
calib_results <- arrange(calib_results, desc(DayNum))
write.csv(calib_results, "temp.csv")
