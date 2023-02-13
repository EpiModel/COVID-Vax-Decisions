library("EpiModelCOVID")

# Prepare household network ----------------------------------------------

# Set household attribute
nw <- set.vertex.attribute(nw, "household", persons.by.hh$hh)

# Find edge list
hhPairs <- merge(persons.by.hh, persons.by.hh, by = "hh")
hhPairs <- subset(hhPairs, (ids.x < ids.y))
hhPairs <- hhPairs[, c(2, 4)]
names(hhPairs) <- c(".head", ".tail")

# Estimate community network ----------------------------------------------

## Community network
md.cc <- 13.8 # from Nelson et al
target.stats.cc <- c(md.cc * n / 2,
                     as.numeric(table(age.grp)[3] * md.cc * 0.41), # from Nelson et al
                     as.numeric(table(age.grp)[1] * md.cc / 2) * 0.69, # from Prem et al
                     as.numeric(table(age.grp)[2] * md.cc / 2) * 0.81, # from Prem et al
                     as.numeric(table(age.grp)[3] * md.cc * 0.41 / 2) * 0.21) # from Prem et al 
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
               nwstats.formula = ~edges + nodefactor("age.grp", levels = 1:3) + 
                 nodematch("age.grp", diff = TRUE))


# Save results ------------------------------------------------------------

est <- list(est.cc)
est <- lapply(est, trim_netest)
saveRDS(est, file = "data/input/est.rds")
