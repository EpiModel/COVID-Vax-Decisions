
library("EpiModelCOVID")

# Read in fitted network models
est <- readRDS("data/input/est.rds")

# Model parameters
source("R/01-epi-params.R")
param <- param.net(
  inf.prob = c(0.11, 0.11, 0.11),
  act.rate = c(5, 1, 1),
  inf.prob.a.rr = 0.5,
  act.rate.dx.inter.rr = 0.1,
  act.rate.dx.inter.time = 1,
  act.rate.sympt.inter.rr = 0.5,
  act.rate.sympt.inter.time = 1,
  prop.clinical = c(0.40, 0.25, 0.37, 0.42, 0.51, 0.59, 0.72, 0.76),
  prop.hospit = c(0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  ea.rate = 1 / 4.0,
  ar.rate = 1 / 5.0,
  eip.rate = 1 / 4.0,
  ipic.rate = 1 / 1.5,
  icr.rate = 1 / 3.5,
  ich.rate = 1 / 3.5,
  hr.rate = 1 / 7,
  rs.rate = 1 / 7,
  pcr.sens = 0.8,
  dx.rate.sympt = 0.1,
  dx.rate.other = 0.01,
  allow.rescreen = TRUE,
  vax1.start = 0,
  vax3.start = 40,
  vax4.start = 60,
  vax1.rate = c(0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  vax2.rate = c(0, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  vax3.rate = c(0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  vax4.rate = c(0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  vax2.interval = 21,
  vax3.interval = 20,
  vax4.interval = 20,
  se.prob = c(0.3, 0.4, 0.4, 0.4),
  se.rr = 0.8,
  prevax.inf.rr = 1.2,
  postvax.inf.rr = 0.8,
  vax1.rr.infect = 0.75,
  vax2.rr.infect = 0.25,
  vax3.rr.infect = 0.25,
  vax4.rr.infect = 0.25,
  vax1.rr.clinical = 0.05,
  vax2.rr.clinical = 0.05,
  vax3.rr.clinical = 0.05,
  vax4.rr.clinical = 0.05,
  vax1.rr.hosp = 0.5,
  vax2.rr.hosp = 0.5,
  vax3.rr.hosp = 0.5,
  vax4.rr.hosp = 0.5,
  a.rate = mean(mr_vec),
  arrival.age = 0,
  mort.rates = mr_vec,
  mort.dis.mult = 180,
  half.life = 80
)

init <- init.net(e.num = 100)

control <- control.net(
  nsteps = 100,
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
  verbose = FALSE
)

sim <- netsim(est, param, init, control)
df <- as.data.frame(sim)
nws <- get_nwstats(sim)

