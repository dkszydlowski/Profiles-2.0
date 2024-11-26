#### clean profiles and correct for calibration ####

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


#### correct for jumps due to calibration ####
plot_profiles_heatmap(allProfilesCombined, 2024, "pH", plot.lake = "L", log.trans = TRUE)

#### 2024 ####
# pH has the biggest calibration jump in July 2024
# change in values before to after calibration:
# 4.8 -->	4
# 7.83 -->	7
# 9.9	--> 10

# Because the change was relatively consistent between the 4 and 8, can either
# add 0.8 to values after the calibration or subtract 0.8 from values before the calibration.
# Values after the calibration seem more in line with our wet chemistry data, so will modify
# values before the calibration to match

prof.cal <- allProfilesCombined %>%
  mutate(pH = replace(pH, doy <= 212 & year == 2024 & !is.na(pH), pH[doy <= 212 & year == 2024 & !is.na(pH)] - 0.8))

# make a heatmap to check results
plot_profiles_heatmap(prof.cal, 2024, "pH", plot.lake = "all", log.trans = TRUE)


### DO ###
plot_profiles_heatmap(prof.cal, 2024, "do_percent", plot.lake = "all", log.trans = TRUE)

# there is one day in 2024 in Peter when DO was incredibly high throughout the water column,
# and we believe it is due to sonde error (perhaps a bubble)

prof.cal = prof.cal %>% mutate(do_percent = replace(do_percent, lake == "R" & year == 2024 & doy == 165, NA))
prof.cal = prof.cal %>% mutate(do_mgL = replace(do_mgL, lake == "R" & year == 2024 & doy == 165, NA))

# verify by plotting
plot_profiles_heatmap(prof.cal, 2024, "do_mgL", plot.lake = "all", log.trans = TRUE)


#### 2022 ####
# there is very high, abnormal DO on doy 173 in L and R
plot_profiles_heatmap(prof.cal, 2022, "do_mgL", plot.lake = "all", log.trans = TRUE)

prof.cal = prof.cal %>% mutate(do_percent = replace(do_percent, (lake == "L" | lake == "R") & year == 2022 & doy == 173, NA))
prof.cal = prof.cal %>% mutate(do_mgL = replace(do_mgL, (lake == "L" | lake == "R")  & year == 2022 & doy == 173, NA))

# verify
plot_profiles_heatmap(prof.cal, 2022, "do_mgL", plot.lake = "all", log.trans = TRUE)

# there is also very low DO in R on doy 192

prof.cal = prof.cal %>% mutate(do_percent = replace(do_percent, (lake == "R") & year == 2022 & doy == 192, NA))
prof.cal = prof.cal %>% mutate(do_mgL = replace(do_mgL, (lake == "R")  & year == 2022 & doy == 192, NA))


plot_profiles_heatmap(prof.cal, plot.year = "all", "temp", plot.lake = "all", log.trans = TRUE)




#### creat a flag column for data that I don't trust
# for now, this is only pH for a couple of points in 2022 when 
# I suspect that the sonde was stored in DI water instead of tap water

# create empty flag column
prof.cal = prof.cal %>% mutate(flag = NA)

# flag high pH day
prof.cal = prof.cal %>%
  mutate(flag = replace(flag, year == 2022 & (doy == 154 | doy %in% c(166:174)), "pH"))

# flag high pH week when DKS had covid
prof.cal = prof.cal %>% 
  mutate(flag = replace(flag, year == 2022 & lake == "T" & doy %in% c(155, 156), "pH"))

# test replacing flagged rows with NA
test = prof.cal %>% mutate(pH = replace(pH, flag == "pH", NA))

# plot test data
plot_profiles_heatmap(test, plot.year = "all", "pH", plot.lake = "all", log.trans = TRUE)

remove(test)



##### save the cleaned data #####
# one version as a csv and one as an R file
write.csv(prof.cal, "./data/formatted data/cleaned uninterpolated profiles.csv", row.names = FALSE)
save(prof.cal, file = "./data/formatted data/cleaned uninterpolated profiles.RData")

