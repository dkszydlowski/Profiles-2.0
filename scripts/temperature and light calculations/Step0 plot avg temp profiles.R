# plot average temp profiles over time to get a sense for where thermocline should be

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(rLakeAnalyzer)) install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)


# read in the profiles data
profiles = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

# select just temperature
temp = profiles %>% select(lake, year, doy, depth, temp)

temp.avg = profiles %>% 
  group_by(lake, year, depth) %>% 
  summarize(mean.temp = mean(temp, na.rm = TRUE))

temp.avg <- temp.avg %>%
  arrange(lake, year, depth) %>%
  group_by(lake, year) %>%
  mutate(temp.diff = lag(mean.temp)- (mean.temp)) %>%
  ungroup()


# summarize max temp difference by lake and year
max.diff = temp.avg %>% group_by(lake, year) %>% 
  summarize(max.diff = max(temp.diff, na.rm = TRUE), depth = depth[which.max(temp.diff)])



ggplot(temp.avg, aes(x = mean.temp, y = depth, color = as.factor(year)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+ 
  facet_wrap(~lake)+
  theme_classic()
