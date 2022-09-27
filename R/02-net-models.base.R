library("EpiModelCOVID")
source("R/01-epi-params.R")

# Set network
n <- 1000
nw <- network_initialize(n)

age <- round(rnorm(n, 40, 21), 1)
age <- pmax(age, 0)
age <- pmin(age, 99)

age.breaks <- seq(0, 100, 10)
age.grp <- cut(age, age.breaks, labels = FALSE, right = FALSE)

nw <- set_vertex_attribute(nw, "age", age)
nw <- set_vertex_attribute(nw, "age.grp", age.grp)

## Within household network
md.hh <- 2.5
target.stats.hh <- md.hh * n / 2

formation.hh <- ~edges

coef.diss.hh <- dissolution_coefs(dissolution = ~ offset(edges), duration = 1e5)
est.hh <- netest(
  nw,
  formation.hh,
  target.stats.hh,
  coef.diss.hh,
  keep.fit = TRUE,
  set.control.ergm = control.ergm(MCMLE.maxit = 500)
)

## Within office network
md.oo <- 5
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
md.cc <- 2
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

est <- list(est.hh, est.oo, est.cc)
est <- lapply(est, trim_netest)
saveRDS(est, file = "data/input/est.rds")
