
# Setup -------------------------------------------------------------------

# Load packages
library("EpiModelCOVID")
library("dplyr")
library("tidyr")
library("gbp")

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
