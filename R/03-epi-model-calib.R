
library("EpiModelCOVID")

# Read in fitted network models
#est <- readRDS("data/input/est.rds")
est <- readRDS("../COVID-Vax-Decisions/data/input/est-100000.rds")

# Model parameters
param <- param.net(
  inf.prob = c(0.05, 0.05), # per layer; calibrated
  inf.diff = 0.015,
  inf.boost.start = 180 + c(-30, 350),
  inf.boost.stop = 180 + c(15, 396),
  inf.supp.start =  180 + c(16, 274, 397),
  inf.supp.stop = 180 + c(151, 334, 608),
  act.rate = c(3, 1), # per layer; calibrated
  inf.prob.a.rr = 0.5, # from Kristin
  act.rate.dx.inter.rr = 1, # turning off for now
  act.rate.dx.inter.time = Inf, # turning off for now
  act.rate.sympt.inter.rr = 1, # turning off for now
  act.rate.sympt.inter.time = Inf, # turning off for now
  prop.clinical = c(0.573, 0.642, 0.760, 0.800, 0.813, 0.814, 0.769, 0.723, 0.666), # by decade of age; from Kristin
  prop.hospit = c(0.060, 0.063, 0.081, 0.154, 0.207, 0.268, 0.357, 0.465, 0.539), # by decade of age; from Kristin
  ea.rate = 1 / 5.5, # from https://academic.oup.com/cid/article/74/9/1678/6359063 (duration of latent period)
  ar.rate = 1 / 5.0, # from Kristin (duration of subclinical infectious period)
  eip.rate = 1 / 5.5, # from https://academic.oup.com/cid/article/74/9/1678/6359063 (duration of latent period)
  ipic.rate = 1 / 1.5, # from Kristin (duration of preclinical infectious period)
  icr.rate = 1 / 3.5, # from Kristin (duration of clinical infectious period prior to recovery)
  ich.rate = 1 / 3, # from Kristin (duration of clinical infectious period prior to hosp)
  hr.rate = 1 / 4, # from Kristin (duration of hospitalization prior to recovery)
  rs.rate = 1 / 270, # https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/vaccine-induced-immunity.html 
  pcr.sens = 0.8, # from Kristin
  dx.rate.sympt = 0.1, # from Katy
  dx.rate.other = 0.01, # from Katy
  allow.rescreen = TRUE, # assumed
  vax1.start = 180 + c(534, 132, 84, 74, 11), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.start = 180 + c(Inf, 368, 323, 323, 265), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.start = 180 + c(Inf, Inf, Inf, 453, 453), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax1.rate = c(0.0005, 0.002, 0.012, 0.012, 0.02), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax1.rate.half.life = c(365, 200, 80, 80, 100), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.rate = c(0.01, 0.54, 0.65, 0.65, 0.25), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.rate.half.life = c(365, 80, 30, 30, 40), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.rate = c(0, 0.005, 0.01, 0.025, 0.02), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.rate.half.life = c(NA, 60, 35, 30, 50), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.rate = c(0, 0, 0, 0.005, 0.005), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.rate.half.life = c(NA, NA, NA, 60, 80), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.interval = 21, # 3 weeks
  vax3.interval = 180, # 6 months
  vax4.interval = 120, # 4 months
  se.prob = c(0.18, 0.18, 0.18, 0.18), # per dose; from Chrissian et al
  # inf.nudge.prob = 0.072, # from Yadete et al (65.8 vs. 58.6% booster willingness for those with vs. without infected friends/family)
  # se.nudge.prob = 0.073, # from Chrissian et al (61.6 vs. 68.8% booster willingness for those with vs. without missed work due to SE)
  # bt.nudge.prob = 0.125, # from Dziedzic et al (66 vs. 78.5% booster willingness for those with vs. without previous infection)
  inf.nudge.prob = 0,
  se.nudge.prob = 0,
  bt.nudge.prob = 0,
  vax1.rr.infect = 0.324, # from Katy
  vax2.rr.infect = 0.112, # from Katy
  vax3.rr.infect = 0.12, # from Katy
  vax4.rr.infect = 0.12, # assumed
  vax1.rr.clinical = 0.4, # from Katy
  vax2.rr.clinical = 0.09, # from Katy
  vax3.rr.clinical = 0.09, # from Katy
  vax4.rr.clinical = 0.09, # assumed
  vax1.rr.hosp = 0.3, # from Katy
  vax2.rr.hosp = 0.02, # from Katy
  vax3.rr.hosp = 0.07, # from Katy
  vax4.rr.hosp = 0.07, # assumed
  a.rate = mean(mr_vec), # assumed 
  arrival.age = 0, # assumed
  mort.rates = mr_vec, # from Kristin (age <1, 1-4, 5-9, 10-14, ... 80-84, 85+)
  mort.dis.mult = 180, # from Katy
  half.life = 80, # from Katy
  # vax.willing.prob = c(0.7, 0.75, 0.85) # from Kelly et al; age 18 - 49, 50 - 64, 65+
  vax.willing.prob = c(1, 1, 1)
)

init <- init.net(e.num = 1600)

control <- control.net(
  nsteps = 180 + 608,
  nsims = 10,
  ncores = 1,
  initialize.FUN = init_covid_vax_decisions,
  aging.FUN = aging_covid,
  departures.FUN = deaths_covid_vax_decisions,
  arrivals.FUN = arrival_covid_vax_decisions,
  resim_nets.FUN = resim_nets_covid_vax_decisions,
  infection.FUN = infect_covid_vax_decisions,
  recovery.FUN = progress_covid_vax_decisions,
  dx.FUN = dx_covid,
  vax.FUN = vax_covid_vax_decisions,
  prevalence.FUN = prevalence_covid_vax_decisions,
  module.order = c(
    "aging.FUN",
    "departures.FUN",
    "arrivals.FUN",
    "resim_nets.FUN",
    "infection.FUN",
    "recovery.FUN",
    "dx.FUN",
    "vax.FUN",
    "prevalence.FUN"
  ),
  resimulate.network = TRUE,
  tergmLite = TRUE,
  save.nwstats = TRUE,
  nwstats.formula.1 = "formation",
  nwstats.formula.2 = "formation",
  nwstats.formula.3 = "formation",
  verbose = TRUE
)

sim <- netsim(est, param, init, control)
df <- as.data.frame(sim)
nws <- get_nwstats(sim)

