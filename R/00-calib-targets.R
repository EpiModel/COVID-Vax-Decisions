library(readxl)
library(tidyverse)
library(dplyr)

#Population sizes from 2021 ACS data
Pop0To4 <- 625969
Pop5To17 <- 1896155
Pop18To49 <- 4644066
Pop18To64 <- 6691755
Pop50To64 <- 2047689
Pop65Plus <- 1585687

#Vaccines administered from CDC data
vax_data <- read_excel("data/input/vax_data.xlsx")
vax_data$Month <- as.Date(vax_data$Date) 
vax_data <- vax_data[, c(39, seq(3, 37, 2))]

#combine data for 5+ and 12+ groups
vax_data$Administered_Dose1_Recip_5Plus[vax_data$Administered_Dose1_Recip_5Plus == 0 | is.na(vax_data$Administered_Dose1_Recip_5Plus)] <- vax_data$Administered_Dose1_Recip_12Plus[vax_data$Administered_Dose1_Recip_5Plus == 0 | is.na(vax_data$Administered_Dose1_Recip_5Plus)]
vax_data$Series_Complete_5Plus[vax_data$Series_Complete_5Plus == 0 | is.na(vax_data$Series_Complete_5Plus)] <- vax_data$Series_Complete_12Plus[vax_data$Series_Complete_5Plus == 0 | is.na(vax_data$Series_Complete_5Plus)]
vax_data$Additional_Doses_5Plus[vax_data$Additional_Doses_5Plus == 0 | is.na(vax_data$Additional_Doses_5Plus)] <- vax_data$Additional_Doses_12Plus[vax_data$Additional_Doses_5Plus == 0 | is.na(vax_data$Additional_Doses_5Plus)]

#split into non-overlapping age groups
vax_data$Dose1_0To4 <- vax_data$Administered_Dose1_Recip - vax_data$Administered_Dose1_Recip_5Plus
vax_data$Dose1_5To17 <- vax_data$Administered_Dose1_Recip_5Plus - vax_data$Administered_Dose1_Recip_18Plus
vax_data$Dose1_18To64 <- vax_data$Administered_Dose1_Recip_18Plus - vax_data$Administered_Dose1_Recip_65Plus
vax_data$Dose1_65Plus <- vax_data$Administered_Dose1_Recip_65Plus

vax_data$Dose2_0To4 <- vax_data$Series_Complete_Yes - vax_data$Series_Complete_5Plus
vax_data$Dose2_5To17 <- vax_data$Series_Complete_5Plus - vax_data$Series_Complete_18Plus
vax_data$Dose2_18To64 <- vax_data$Series_Complete_18Plus - vax_data$Series_Complete_65Plus
vax_data$Dose2_65Plus <- vax_data$Series_Complete_65Plus

vax_data$Dose3_5To17 <- vax_data$Additional_Doses_5Plus - vax_data$Additional_Doses_18Plus
vax_data$Dose3_18To49 <- vax_data$Additional_Doses_18Plus - vax_data$Additional_Doses_50Plus
vax_data$Dose3_50To64 <- vax_data$Additional_Doses_50Plus - vax_data$Additional_Doses_65Plus
vax_data$Dose3_65Plus <- vax_data$Additional_Doses_65Plus

vax_data$Dose4_50To64 <- vax_data$Second_Booster_50Plus - vax_data$Second_Booster_65Plus
vax_data$Dose4_65Plus <- vax_data$Second_Booster_65Plus

#Calculate percent coverage
vax_data$Dose1_0To4 <- vax_data$Dose1_0To4 / Pop0To4
vax_data$Dose1_5To17 <- vax_data$Dose1_5To17 / Pop5To17
vax_data$Dose1_18To64 <- vax_data$Dose1_18To64 / Pop18To64
vax_data$Dose1_65Plus <- vax_data$Dose1_65Plus / Pop65Plus

vax_data$Dose2_0To4 <- vax_data$Dose2_0To4 / Pop0To4
vax_data$Dose2_5To17 <- vax_data$Dose2_5To17 / Pop5To17
vax_data$Dose2_18To64 <- vax_data$Dose2_18To64 / Pop18To64
vax_data$Dose2_65Plus <- vax_data$Dose2_65Plus / Pop65Plus

vax_data$Dose3_5To17 <- vax_data$Dose3_5To17 / Pop5To17
vax_data$Dose3_18To49 <- vax_data$Dose3_18To49 / Pop18To49
vax_data$Dose3_50To64 <- vax_data$Dose3_50To64 / Pop50To64
vax_data$Dose3_65Plus <- vax_data$Dose3_65Plus / Pop65Plus

vax_data$Dose4_50To64 <- vax_data$Dose4_50To64 / Pop50To64
vax_data$Dose4_65Plus <- vax_data$Dose4_65Plus / Pop65Plus

vax_data[, c(20:33)] <- round(vax_data[, c(20:33)] * 100, 1)

vax_targets <- vax_data[1:18, c(1, 20:33)]

#subset to only relevant time frames
vax_targets$Dose1_0To4[vax_targets$Month < as.Date("2022-07-01")] <- NA
vax_targets$Dose1_5To17[vax_targets$Month < as.Date("2021-06-01")] <- NA
vax_targets$Dose1_18To64[vax_targets$Month < as.Date("2021-04-01")] <- NA

vax_targets$Dose2_0To4[vax_targets$Month < as.Date("2022-07-01")] <- NA
vax_targets$Dose2_5To17[vax_targets$Month < as.Date("2021-06-01")] <- NA
vax_targets$Dose2_18To64[vax_targets$Month < as.Date("2021-04-01")] <- NA

vax_targets$Dose3_5To17[vax_targets$Month < as.Date("2022-02-01")] <- NA
vax_targets$Dose3_18To49[vax_targets$Month < as.Date("2021-12-01")] <- NA
vax_targets$Dose3_50To64[vax_targets$Month < as.Date("2021-12-01")] <- NA
vax_targets$Dose3_65Plus[vax_targets$Month < as.Date("2021-10-01")] <- NA

vax_targets$Month <- format(as.Date(vax_targets$Month), "%B %Y")

rm(list=setdiff(ls(), c("vax_targets")))
