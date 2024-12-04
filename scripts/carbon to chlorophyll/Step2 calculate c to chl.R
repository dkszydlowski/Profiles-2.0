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

# calculate C:chl ratios
c.chl = c.chl %>% mutate(c.to.chl = carbon/manual.chl)




#### Correct C:chl to portion of terrestrial carbon from Carpenter et al. 2016 Ecology Letters
# use 38% terrestrial for L, 47% for R, and 57% for T
#  https://doi.org/10.1111/ele.12558

t.terr = 0.57
r.terr = 0.47
l.terr = 0.38

c.chl = c.chl %>% mutate(c.to.chl.corr = NA)

c.chl = c.chl %>% rowwise %>%  mutate(c.to.chl.corr = replace(c.to.chl.corr, lake == "L", (1-l.terr)*c.to.chl))
c.chl = c.chl %>% rowwise %>%  mutate(c.to.chl.corr = replace(c.to.chl.corr, lake == "R", (1-r.terr)*c.to.chl))
c.chl = c.chl %>% rowwise %>%  mutate(c.to.chl.corr = replace(c.to.chl.corr, lake == "T", (1-t.terr)*c.to.chl))

# visualize c to chl over depths
ggplot(c.chl, aes(x = ZID, y = c.to.chl, group = ZID))+
  geom_boxplot()+
  facet_wrap(~lake)


# save c to chl ratios
write.csv(c.chl, "./data/formatted data/carbon to chlorophyll/carbon to chl 2022.csv", row.names = FALSE)
save(c.chl, file = "./data/formatted data/carbon to chlorophyll/carbon to chl 2022.RData")



