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



#### 2022 ####
plot_profiles_heatmap(prof.cal, 2024, "do_percent", plot.lake = "all", log.trans = TRUE)

# there is one day in Peter when DO was incredibly high throughout the water column,
# and we believe it is due to sonde error (perhaps a bubble)

prof.cal = 

