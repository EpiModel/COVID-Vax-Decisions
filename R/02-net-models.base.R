library("EpiModelCOVID")
source("R/01-epi-params.R")

# Set network
n <- 100000
nw <- network_initialize(n)

# Initialize demographic attributes that are relevant to nw structure
age <- round(rnorm(n, 40, 21), 1) #from Katy's thesis
age <- pmax(age, 0)
age <- pmin(age, 99)

age.breaks <- seq(0, 100, 10)
age.grp <- cut(age, age.breaks, labels = FALSE, right = FALSE)

nw <- set_vertex_attribute(nw, "age", age)
nw <- set_vertex_attribute(nw, "age.grp", age.grp)

## Within household network
md.hh <- 2.7 # from Kristin's dissertation: census estimate of people per household
target.stats.hh <- c(md.hh * n / 2, 
                     as.numeric(table(age.grp)[3] * md.hh / 2) * 0.78,
                     as.numeric(table(age.grp)[4] * md.hh / 2) * 0.45,
                     as.numeric(table(age.grp)[5] * md.hh / 2) * 0.27,
                     as.numeric(table(age.grp)[6] * md.hh / 2) * 0.31,
                     as.numeric(table(age.grp)[7] * md.hh / 2) * 0.37,
                     as.numeric(table(age.grp)[8] * md.hh / 2) * 0.37,
                     as.numeric(table(age.grp)[9] * md.hh / 2) * 0.37,
                     as.numeric(table(age.grp)[10] * md.hh / 2) * 0.37)

formation.hh <- ~edges + nodematch("age.grp", diff = TRUE, levels = 3:10) # count of edges, count of edges w/in age.grp

coef.diss.hh <- dissolution_coefs(dissolution = ~ offset(edges), duration = 1e5)
est.hh <- netest(
  nw,
  formation.hh,
  target.stats.hh,
  coef.diss.hh,
  keep.fit = TRUE,
  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

# Diagnose
summary(est.hh)
dx.hh <- netdx(est.hh, nsims = 25, ncores = 5, nsteps = 600, dynamic = TRUE,
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
               nwstats.formula = ~edges + nodefactor("age.grp") + nodematch("age.grp", diff = TRUE))
plot(dx.hh)

## Community network
md.cc <- 13.8 # from Nelson et al
target.stats.cc <- c(md.cc * n / 2,
                     as.numeric(table(age.grp)[3] * md.cc / 2) * 0.52,
                     as.numeric(table(age.grp)[4] * md.cc / 2) * 0.29,
                     as.numeric(table(age.grp)[5] * md.cc / 2) * 0.31,
                     as.numeric(table(age.grp)[6] * md.cc / 2) * 0.17,
                     as.numeric(table(age.grp)[7] * md.cc / 2) * 0.17,
                     as.numeric(table(age.grp)[8] * md.cc / 2) * 0.17,
                     as.numeric(table(age.grp)[9] * md.cc / 2) * 0.17,
                     as.numeric(table(age.grp)[10] * md.cc / 2) * 0.17)
formation.cc <- ~edges + nodematch("age.grp", diff = TRUE, levels = 3:10)
coef.diss.cc <- dissolution_coefs(dissolution = ~ offset(edges), duration = 1)

est.cc <- netest(
  nw,
  formation.cc,
  target.stats.cc,
  coef.diss.cc,
  keep.fit = TRUE,
  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

dx.cc <- netdx(est.cc, nsims = 1000, dynamic = FALSE, 
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
               nwstats.formula = ~edges + nodefactor("age.grp") + nodematch("age.grp", diff = TRUE))

est <- list(est.hh, est.cc)
est <- lapply(est, trim_netest)
saveRDS(est, file = "data/input/est.rds")
