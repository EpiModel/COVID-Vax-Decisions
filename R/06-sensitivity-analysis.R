sens_Inc <- data.frame(hosp.nudge.prob.scale = rep(NA, 78), bt.nudge.prob.scale = rep(NA, 78), 
                       misc.nudge.prob.scale = rep(NA, 78), nat.imm.dur = rep(NA, 78),
                       vax.hl = rep(NA, 78), cInc = rep(NA, 78))
for (i in 1001:1078) {
  
  print(i)
  
  temp <- merge_simfiles(i, indir = "data/output/sensitivity-sims", vars = NULL,  truncate.at = 181, verbose = TRUE)
  
  scale.1 <- temp$param$hosp.nudge.prob[5] / 0.102
  scale.2 <- temp$param$bt.nudge.prob[5] / 0.125
  scale.3 <- temp$param$misc.nudge.prob.1[5] / 0.12
  dur1 <- 1 / temp$param$rs.rate
  dur2 <- temp$param$half.life
  
  scenario.cInc <- median(colSums(temp$epi$se.flow) / colSums(temp$epi$num) * 100000)
  
  sens_Inc[(i - 1000), ] <- c(scale.1, scale.2, scale.3, dur1, dur2, scenario.cInc)
  
  remove(temp)
}
