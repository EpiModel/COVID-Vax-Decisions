
library("EpiModelCOVID")

# Read in fitted network models
est <- readRDS("data/input/est.rds")

# Model parameters
source("R/01-epi-params.R")
param <- param.net(
  inf.prob = c(0.11, 0.11, 0.11), # per layer; from Kristin
  act.rate = c(5, 1, 1), # from corporate model; original source unclear
  inf.prob.a.rr = 0.5, # from Kristin
  act.rate.dx.inter.rr = 1, # turning off for now
  act.rate.dx.inter.time = Inf, # turning off for now
  act.rate.sympt.inter.rr = 1, # turning off for now
  act.rate.sympt.inter.time = Inf, # turning off for now
  prop.clinical = c(0.573, 0.642, 0.760, 0.800, 0.813, 0.814, 0.769, 0.723, 0.666), # by decade of age; from Kristin
  prop.hospit = c(0.060, 0.063, 0.081, 0.154, 0.207, 0.268, 0.357, 0.465, 0.539), # by decade of age; from Kristin
  ea.rate = 1 / 4.0, # from Kristin (duration of latent period)
  ar.rate = 1 / 5.0, # from Kristin (duration of subclinical infectious period)
  eip.rate = 1 / 4.0, # from Kristin (duration of latent period)
  ipic.rate = 1 / 1.5, # from Kristin (duration of preclinical infectious period)
  icr.rate = 1 / 3.5, # from Kristin (duration of clinical infectious period prior to recovery)
  ich.rate = 1 / 3, # from Kristin (duration of clinical infectious period prior to hosp)
  hr.rate = 1 / 4, # from Kristin (duration of hospitalization prior to recovery)
  rs.rate = 1 / 390, # from Katy (time from recovered to susceptible)
  pcr.sens = 0.8, # from Kristin
  dx.rate.sympt = 0.1, # from Katy
  dx.rate.other = 0.01, # from Katy
  allow.rescreen = TRUE, # assumed
  vax1.start = c(534, 132, 84, 74, 11), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.start = c(Inf, 368, 323, 323, 265), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.start = c(Inf, Inf, Inf, 453, 453), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax1.rate = c(0.001, 0.02, 0.11, 0.11, 0.43), # from Katy; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.rate = c(0.0005, 0.0006, 0.008, 0.008, 0.0075), # from Katy; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.rate = c(0, 0.01, 0.01, 0.05, 0.043), # from Katy; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.rate = c(0, 0, 0, 0.05, 0.05), # assumed based on vax3.rate; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.interval = 21, # 3 weeks
  vax3.interval = 180, # 6 months
  vax4.interval = 120, # 4 months
  se.prob = c(0.18, 0.18, 0.18, 0.18), # per dose; from Chrissian et al
  inf.nudge.prob = 0.072, # from Yadete et al (65.8 vs. 58.6% booster willingness for those with vs. without infected friends/family)
  se.nudge.prob = 0.073, # from Chrissian et al (61.6 vs. 68.8% booster willingness for those with vs. without missed work due to SE)
  bt.nudge.prob = 0.125, # from Dziedzic et al (66 vs. 78.5% booster willingness for those with vs. without previous infection)
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
  vax.willing.prob = c(0.7, 0.75, 0.85) # from Kelly et al; age 18 - 49, 50 - 64, 65+
)

init <- init.net(e.num = 100)

control <- control.net(
  nsteps = 608,
  nsims = 1,
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

