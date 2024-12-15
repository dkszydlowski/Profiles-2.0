
# format historical data so we can investigate DCM

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(rLakeAnalyzer)) install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)

# working with temp.hist and chl.hist from step0 read in historical data

# filter to LRT and W, which had Aquashade added in 2012
temp.hist = temp.hist %>% filter(lakeid %in% c("L", "R", "T", "Ward"))
chl.hist = chl.hist %>% filter(lakeid %in% c("L", "R", "T", "Ward"))

# get mean chl at 1% light by lake and year
mean.chl.hist = chl.hist %>%
  group_by(lakeid, year4, depth_id) %>% 
  summarize(mean.depth = mean(depth, na.rm = TRUE), mean.chla = mean(chla, na.rm = TRUE))


ggplot(mean.chl.hist %>% filter(mean.depth <12 & lakeid == "L"), aes(x = mean.chla, y = mean.depth, color = as.factor(year4)))+
  geom_path()+
  geom_point()+
  facet_wrap(~year4)+
  scale_y_reverse()



ggplot(mean.chl.hist %>% filter(mean.depth <12 & lakeid == "Ward"), aes(x = mean.chla, y = mean.depth, color = as.factor(year4)))+
  geom_path()+
  geom_point()+
  facet_wrap(~year4)+
  scale_y_reverse()



### format the water temp data for rLakeAnalyzer

