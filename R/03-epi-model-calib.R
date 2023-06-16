library("EpiModelCOVID")

# Model parameters
param <- param.net(
  inf.prob = c(0.05, 0.05), # per layer; calibrated
  inf.add = c(0.017, 0.017), # calibrated
  inf.sub = c(0.02, 0.02, 0.02),
  inf.boost.start = 180 + c(-60, 335), # calibrated
  inf.boost.stop = 180 + c(15, 396), # calibrated
  inf.supp.start =  180 + c(16, 258, 397), # calibrated
  inf.supp.stop = 180 + c(180, 334, 608), # calibrated
  act.rate = c(1, 3), # per layer; calibrated
  inf.prob.a.rr = 0.5,
  act.rate.dx.inter.rr = 1, 
  act.rate.dx.inter.time = Inf,
  act.rate.sympt.inter.rr = 1, 
  act.rate.sympt.inter.time = Inf, 
  prop.clinical = c(0.573, 0.642, 0.760, 0.800, 0.813, 0.814, 0.769, 0.723, 0.666), # by decade of age
  prop.hospit = c(0.060, 0.063, 0.081, 0.154, 0.207, 0.268, 0.357, 0.465, 0.539) / 10, # by decade of age
  hosp.boost.mult = 2.25,
  hosp.boost.start = 378, 
  hosp.boost.stop = 423,
  hosp.supp.mult = 0.75,
  hosp.supp.start = 546,
  hosp.supp.stop = 604,
  ea.rate = 1 / 5.5, # from https://academic.oup.com/cid/article/74/9/1678/6359063 (duration of latent period)
  ar.rate = 1 / 5.0, 
  eip.rate = 1 / 5.5, # from https://academic.oup.com/cid/article/74/9/1678/6359063 (duration of latent period)
  ipic.rate = 1 / 1.5, 
  icr.rate = 1 / 3.5,
  ich.rate = 1 / 3, 
  hr.rate = 1 / 15.5, # from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7899024/
  rs.rate = 1 / 300, # from https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/vaccine-induced-immunity.html 
  pcr.sens = 0.8, 
  dx.rate.sympt = 0.1, 
  dx.rate.other = 0.01,
  allow.rescreen = TRUE, 
  vax1.start = 180 + c(534, 132, 84, 74, 11), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.start = 180 + c(Inf, 368, 323, 323, 265), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.start = 180 + c(Inf, Inf, Inf, 453, 453), # age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax1.rate = c(0.0005, 0.0012, 0.016, 0.016, 0.018), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.rate = c(0.01, 0.015, 0.30, 0.30, 0.45), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax3.rate = c(0, 0.003, 0.045, 0.038, 0.015), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax4.rate = c(0, 0, 0, 0.005, 0.008), # manually calibrated; age 0-4, 5-17, 18-49, 50 - 64, 65+
  vax2.interval = 21, # 3 weeks
  vax3.interval = 180, # 6 months
  vax4.interval = 120, # 4 months
  se.prob = c(0.18, 0.18, 0.18, 0.18), # per dose; from Chrissian et al
  hosp.nudge.prob = c(NA, NA, .102, .102, .102), # by age group; derived from https://www.kff.org/coronavirus-covid-19/poll-finding/kff-covid-19-vaccine-monitor-september-2021/
  se.nudge.prob = c(NA, NA, 0.073, 0.073, 0.073), # by age group; from Chrissian et al (61.6 vs. 68.8% booster willingness for those with vs. without missed work due to SE)
  bt.nudge.prob = c(NA, NA, 0.125, 0.125, 0.125), # by age group; from Dziedzic et al (66 vs. 78.5% booster willingness for those with vs. without previous infection)
  misc.nudge.prob.1 = c(NA, NA, .18, .18, .12), # for dose 1; by age group; calibrated
  misc.nudge.prob.2 = c(NA, NA, .75, .43, .35), # for dose 2; by age group; calibrated
  misc.nudge.prob.3 = c(NA, NA, 0, 0.50, 0.42), # for dose 3; by age group; calibrated
  vax1.rr.infect = 0.324, 
  vax2.rr.infect = 0.112,
  vax3.rr.infect = 0.12, 
  vax4.rr.infect = 0.12, 
  vax1.rr.clinical = 0.4, 
  vax2.rr.clinical = 0.09, 
  vax3.rr.clinical = 0.09, 
  vax4.rr.clinical = 0.09,
  vax1.rr.hosp = 0.3, 
  vax2.rr.hosp = 0.02,
  vax3.rr.hosp = 0.07,
  vax4.rr.hosp = 0.07, 
  a.rate = mean(mr_vec), 
  arrival.age = 0, 
  mort.rates = mr_vec, # age <1, 1-4, 5-9, 10-14, ... 80-84, 85+
  mort.dis.mult = 180 * 10, 
  half.life = 80, 
  vax.willing.prob = c(0.7, 0.75, 0.91), # from Kelly et al and Nikolovski et al; age 18 - 49, 50 - 64, 65+
  hosp.th = 0.00036, #from covid.cdc.gov/covid-data-tracker/#hospital-capacity
  hh.pairs = hhPairs 
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
rm(list=setdiff(ls(), c("est", "vax_targets", "mr_vec", "sim")))

