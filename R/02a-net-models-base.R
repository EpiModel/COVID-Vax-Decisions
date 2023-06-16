
# Setup -------------------------------------------------------------------

# Load packages
library("EpiModelCOVID")

# Set network
n <- 100000
nw <- network_initialize(n)

# Initialize age - from http://wonder.cdc.gov/bridged-race-v2020.html
age.pyr <- c( 0.01167, 0.01179, 0.01209, 0.01232, 0.01260, 0.01279, 0.01271,
              0.01266, 0.01296, 0.01306, 0.01313, 0.01323, 0.01380, 0.01388,
              0.01383, 0.01367, 0.01369, 0.01355, 0.01351, 0.01392, 0.01391,
              0.01353, 0.01348, 0.01344, 0.01344, 0.01375, 0.01402, 0.01433,
              0.01462, 0.01480, 0.01470, 0.01413, 0.01367, 0.01334, 0.01330,
              0.01350, 0.01303, 0.01328, 0.01330, 0.01326, 0.01366, 0.01284,
              0.01262, 0.01256, 0.01218, 0.01269, 0.01242, 0.01279, 0.01338,
              0.01398, 0.01392, 0.01293, 0.01256, 0.01251, 0.01272, 0.01326,
              0.01324, 0.01297, 0.01274, 0.01265, 0.01250, 0.01197, 0.01164,
              0.01156, 0.01102, 0.01076, 0.01022, 0.00975, 0.00930, 0.00899,
              0.00880, 0.00851, 0.00841, 0.00857, 0.00626, 0.00609, 0.00576,
              0.00574, 0.00482, 0.00433, 0.00400, 0.00357, 0.00322, 0.00282,
              0.00259, 0.00223, 0.00193, 0.00166, 0.00144, 0.00124, 0.00107,
              0.00092, 0.00080, 0.00069, 0.00059, 0.00051, 0.00044, 0.00038,
              0.00033, 0.00028)
age <- sample(x = 0:99, size = n, prob = age.pyr, replace = TRUE)
age_noise <- runif(n)
age <- age + age_noise

# Initialize age groups: under 18 vs. 18 - 64 vs. 65+
age.breaks <- c(0, 18, 65, 100)
age.grp <- cut(age, age.breaks, labels = FALSE, right = FALSE)

# Set nw attributes
nw <- set_vertex_attribute(nw, "age", age)
nw <- set_vertex_attribute(nw, "age.grp", age.grp)

# Estimate community network ----------------------------------------------

md.cc <- 13.8 # from Nelson et al
target.stats.cc <- c(md.cc * n / 2,
                     as.numeric(table(age.grp)[3] * md.cc * 0.41), # from Nelson et al
                     as.numeric(table(age.grp)[1] * md.cc / 2) * 0.69, # from Prem et al. Note: replaced md.cc with 17.11 after initial run to account for differential md by age
                     as.numeric(table(age.grp)[2] * md.cc / 2) * 0.81, # from Prem et al. Note: replaced md.cc with 14.52 after initial run to account for differential md by age
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
rm(list=setdiff(ls(), c("vax_targets", "mr_vec", "est")))
