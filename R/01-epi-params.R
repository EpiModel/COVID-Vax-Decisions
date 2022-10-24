
mortality_rate <- c(
  607.6, 29.6, 12.8, 21.6, 62.8, 116.1, 142.8, 186.5, 228.3, 300.4, 416.1, 
  600.4, 945.0, 1453.1, 1952.3, 2817.0, 4368.9, 7158.8, 15626.4
)

mr_pp_pd <- mortality_rate / 1e5 / 365

age_spans <- c(1, 4, rep(5, 16), 1)
mr_vec <- rep(mr_pp_pd, times = age_spans)

