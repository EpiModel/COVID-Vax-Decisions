
mortality_rate <- c(
  588.45, 24.8, 11.7, 14.55, 47.85, 88.2, 105.65, 127.2, 154.3, 206.5,
  309.3, 495.1, 736.85, 1051.15, 1483.45, 2294.15, 3642.95, 6139.4, 13938.3
)

mr_pp_pd <- mortality_rate / 1e5 / 365

age_spans <- c(1, 4, rep(5, 16), 1)
mr_vec <- rep(mr_pp_pd, times = age_spans)

