library("EpiModelCOVID")
source("R/01-epi-params.R")

# Set network
n <- 10000
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
target.stats.hh <- md.hh * n / 2

formation.hh <- ~edges # count of edges

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
               nwstats.formula = ~edges + nodefactor("age.grp") + nodematch("age.grp"))
plot(dx.hh)

## Within office network -- to be removed
md.oo <- 5 # assumed
target.stats.oo <- md.oo * n / 2
formation.oo <- ~edges
coef.diss.oo <- dissolution_coefs(dissolution = ~ offset(edges), duration = 1e5)

est.oo <- netest(
  nw,
  formation.oo,
  target.stats.oo,
  coef.diss.oo,
  keep.fit = TRUE,
  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

## Community network
md.cc <- 5 # from Kristin's dissertation: population contact patterns during pandemic; need to update
target.stats.cc <- md.cc * n / 2
formation.cc <- ~edges
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
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6))

est <- list(est.hh, est.oo, est.cc)
est <- lapply(est, trim_netest)
saveRDS(est, file = "data/input/est.rds")
