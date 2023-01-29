library("EpiModelCOVID")

# Estimate household network ----------------------------------------------

# Set household attribute
nw <- set.vertex.attribute(nw, "household", persons.by.hh$hh)

# Calculate targets assuming each hh is completely connected 
# total number of edges should = total number of within-household edges
match.edges.hh <- as.numeric(table(persons.by.hh$hh)) * (as.numeric(table(persons.by.hh$hh)) - 1) / 2
edges.hh <- sum(match.edges.hh)
target.stats.hh <- c(edges.hh, edges.hh) 

# Estimate household nw
formation.hh <- ~edges + nodematch("household") 
coef.diss.hh <- dissolution_coefs(dissolution = ~ offset(edges), duration = 1e5)
est.hh <- netest(nw, formation.hh, target.stats.hh, coef.diss.hh, keep.fit = TRUE,
                 set.control.ergm = control.ergm(MCMLE.maxit = 500))

# Diagnose
summary(est.hh)
dx.hh <- netdx(est.hh, nsims = 25, ncores = 5, nsteps = 600, dynamic = TRUE,
               set.control.ergm = control.simulate.formula(MCMC.burnin = 1e6),
               nwstats.formula = ~edges + nodefactor("household") + nodematch("household"))
plot(dx.hh)


# Estimate community network ----------------------------------------------

## Community network
md.cc <- 13.8 # from Nelson et al
target.stats.cc <- c(md.cc * n / 2,
                     as.numeric(table(age.grp)[3] * md.cc / 3), # from Nelson et al
                     as.numeric(table(age.grp)[1] * md.cc / 2) * 0.69, # from Prem et al
                     as.numeric(table(age.grp)[2] * md.cc / 2) * 0.81, # from Prem et al
                     as.numeric(table(age.grp)[3] * md.cc / 2) * 0.21) # from Prem et al
formation.cc <- ~edges + nodefactor("age.grp", levels = 3) + 
  nodematch("age.grp", diff = TRUE, levels = 1:3)
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
               nwstats.formula = ~edges + nodefactor("age.grp") + 
                 nodematch("age.grp", diff = TRUE))


# Save results ------------------------------------------------------------


est <- list(est.hh, est.cc)
est <- lapply(est, trim_netest)
saveRDS(est, file = "data/input/est.rds")
