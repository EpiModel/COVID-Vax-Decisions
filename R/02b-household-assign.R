
# Setup -------------------------------------------------------------------

# Load packages
library("EpiModelCOVID")
library("dplyr")
library("tidyr")
library("gbp")

# Retrieve attributes
age.grp <- est[[1]]$newnetwork$attr$age.grp
n <- est[[1]]$newnetwork$gal$n

# Set number of households
hh.size <- 2.7 #from https://www.census.gov/quickfacts/GA 
n.hh <- round(n / hh.size)
ids <- 1:n
hh.ids <- 1:n.hh

# Create empty data frames to track hh assignment
hh.by.age <- data.frame(hh.ids = hh.ids, mem.u18 = NA, mem.18t64 = NA, mem.65p = NA)
persons.by.hh <- data.frame(ids = ids, age.grp = age.grp, hh = NA)

# Set proportions
prop.hh.with.child <- 0.292 # from Census Table H2 2020
prop.hh.with.adult <- 0.791 # from Census Table H2 2020
prop.hh.with.elderly <- 0.314 # from Census Table H2 2020
prop.children.with.adult <- 0.979 # from Census Table C4 2021

# Household Assignment ----------------------------------------------------

# Determine which households will have a member under 18 
hh.u18 <- sample(x = hh.ids, size = round(prop.hh.with.child * n.hh))
hh.by.age$mem.u18 = ifelse(hh.by.age$hh.ids %in% hh.u18, TRUE, FALSE)

# Assign children under 18 to the selected households
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 1)[1:length(hh.u18)], ]$hh <- hh.u18 
children.hh.assign <- sample(x = hh.u18, size = length(which(age.grp == 1)) - length(hh.u18), replace = TRUE)
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 1)[(length(hh.u18) + 1):length(which(age.grp == 1))], ]$hh <- children.hh.assign 

# Determine which hh with children will have at least one 18 - 64 member
num.children.wo.adult <- round((1-prop.children.with.adult) * (sum(age.grp == 1)))
num.children <- persons.by.hh[persons.by.hh$age.grp == 1, ] %>% group_by(hh) %>% summarize(num = n())
hh.select <- gbp1d_solver_dpp(p = num.children$num, w = num.children$num, c = num.children.wo.adult)
hh.wo.adult <- num.children$hh[as.logical(hh.select$k)]
hh.with.adult <- setdiff(num.children$hh, hh.wo.adult)
hh.by.age$mem.18t64 = ifelse(hh.by.age$hh.ids %in% hh.with.adult, TRUE, NA)

# Determine which hh without children will have at least one 18 - 64 member
num.hh.add.adult <- round(n.hh * prop.hh.with.adult - sum(hh.by.age$mem.18t64 == TRUE, na.rm = TRUE))
hh.add.adult <- sample(x = hh.by.age[is.na(hh.by.age$mem.18t64) & hh.by.age$mem.u18 == FALSE, ]$hh.ids,
                       size = num.hh.add.adult)
hh.by.age[hh.by.age$hh.ids %in% hh.add.adult, ]$mem.18t64 <- TRUE
hh.by.age[is.na(hh.by.age$mem.18t64), ]$mem.18t64 <- FALSE
hh.18t64 <- hh.by.age[hh.by.age$mem.18t64 == TRUE, ]$hh.ids

# Assign adults to the selected households
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 2)[1:length(hh.18t64)], ]$hh <- hh.18t64 
adults.hh.assign <- sample(x = hh.18t64, size = length(which(age.grp == 2)) - length(hh.18t64), replace = TRUE)
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 2)[(length(hh.18t64) + 1):length(which(age.grp == 2))], ]$hh <- adults.hh.assign

# Determine which hh will have a 65+ member
hh.must.elderly <- hh.by.age[hh.by.age$mem.18t64 == FALSE, ]$hh.ids
hh.by.age[hh.by.age$mem.18t64 == FALSE, ]$mem.65p <- TRUE
num.hh.add.elderly <- round(n.hh * prop.hh.with.elderly - length(hh.must.elderly))
hh.add.elderly <- sample(x = hh.by.age[is.na(hh.by.age$mem.65p), ]$hh.ids, size = num.hh.add.elderly)
hh.by.age[hh.by.age$hh.ids %in% hh.add.elderly, ]$mem.65p <- TRUE
hh.by.age[is.na(hh.by.age$mem.65p), ]$mem.65p <- FALSE
hh.65p <- hh.by.age[hh.by.age$mem.65p == TRUE, ]$hh.ids

# Assign elderly to selected households
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 3)[1:length(hh.65p)], ]$hh <- hh.65p 
elderly.hh.assign <- sample(x = hh.65p, size = length(which(age.grp == 3)) - length(hh.65p), replace = TRUE)
persons.by.hh[persons.by.hh$ids %in% which(age.grp == 3)[(length(hh.65p) + 1):length(which(age.grp == 3))], ]$hh <- elderly.hh.assign 

# Check Household Assignment ----------------------------------------------

# Rules 1 - 3: Proportions of households with at least one child/adult/elderly person are as expected 
hh.check1 <- persons.by.hh %>% group_by(age.grp) %>% summarize(num.hh = n_distinct(hh))
hh.check1$pct.hh <- round(hh.check1$num.hh / length(unique(persons.by.hh$hh)), 3) * 100

# Rule 4: Average household size is as expected
hh.check2 <- persons.by.hh %>% group_by(hh) %>% summarize(num.person = n())
mean.hh.check2 <- round(mean(hh.check2$num.person), 1)

# 5. Every household with a child must also have at least one adult
hh.check3 <- persons.by.hh %>% group_by(hh) %>% summarize(min.age.grp = min(age.grp), max.age.grp = max(age.grp))
orphans <- nrow(hh.check3[hh.check3$min.age.grp == 1 & hh.check3$max.age.grp == 1, ])

# 6. 97.9% of children live with an adult in the 18-64 age range
hh.check4 <- persons.by.hh %>% group_by(hh, age.grp) %>% summarize(num.person = n())
hh.check4 <- spread(hh.check4, key = age.grp, value = num.person)
hh.check4 <- hh.check4[!is.na(hh.check4$`1`) & !is.na(hh.check4$`2`), ]
children.in.hh.w.adult <- round(sum(hh.check4$`1`) / length(which(age.grp == 1)), 3) * 100

# Prepare household network ----------------------------------------------

# Set household attribute
est[[1]]$newnetwork <- set.vertex.attribute(est[[1]]$newnetwork, "household", persons.by.hh$hh)

# Save household edge list
hhPairs <- merge(persons.by.hh, persons.by.hh, by = "hh")
hhPairs <- subset(hhPairs, (ids.x < ids.y))
hhPairs <- hhPairs[, c(2, 4)]
names(hhPairs) <- c(".head", ".tail")
rm(list=setdiff(ls(), c("est", "hhPairs", "vax_targets", "mr_vec")))
