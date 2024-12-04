### calculate carbon to chlorophyll ratios ###

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


#### 2022 ####

# read in carbon data
c = get(load("./data/formatted data/carbon to chlorophyll/carbon 2022.RData"))

# read in chl data
chl = get(load("./data/formatted data/manual chlorophyll/routines chl 2024 and 2022.RData"))

# combine c to chl datasets for 2022
chl = chl %>% filter(year == 2022)

c = c %>% mutate(doy = yday(date))

c.chl = c %>% left_join(chl, by = c("lake", "year", "doy", "ZID"))

# there are some dates we need to reconcile that don't match between C and chl...
c.chl.empty = c.chl %>% filter(is.na(manual.chl))

# in carbon they are listed as
# Tuesday 2022-06-29 180

# Paul 2022-06-28 179

# this one is 
# Tuesday 2022-06-18 169


# in chl they are listed as
# Tuesday 2022-06-28 179

# Paul 2022-06-27 178

# 


c.chl.t = c.chl %>% filter(lake == "T")
