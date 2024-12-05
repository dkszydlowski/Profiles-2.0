# visualize and analyze carbon to chlorophyll ratios #

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# read in the c to chl data
c.chl = get(load("./data/formatted data/carbon to chlorophyll/carbon to chl 2022.RData"))


# remove negative vlues
# remove values > 350, which are from same day fluorometer gave negative values
# and are also inconsistent with the literature

c.chl = c.chl %>% filter(c.to.chl.corr < 250 & c.to.chl.corr > 0)


ggplot(c.chl, aes(x = ZID, y = log10(c.to.chl.corr), group = ZID))+
  geom_boxplot()+
  facet_wrap(~lake)


ggplot(c.chl, aes(x = ZID, y = log(c.to.chl.corr), group = ZID))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~lake)


# calculate average carbon to chl
c.chl.avg = c.chl %>%
  group_by(lake, ZID) %>% 
  summarize(mean.c.chl = mean(c.to.chl.corr, na.rm = TRUE),
            sd.c.chl = sd(c.to.chl.corr, na.rm= TRUE),
            mean.depth = mean(depth, na.rm = TRUE))

# calculate error on estimates
c.chl.avg = c.chl.avg %>% mutate(upper = mean.c.chl +sd.c.chl, lower = mean.c.chl - sd.c.chl)


# plot avg
ggplot(c.chl.avg, aes(x = ZID, y = mean.c.chl))+
  geom_point()+
  facet_wrap(~lake)+
  geom_smooth(color = "black", se = FALSE)+
  theme_minimal()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "light depth", y = "mean carbon to chlorophyll")



ggplot(c.chl.avg, aes(x = as.numeric(mean.depth), y = log10(mean.c.chl)))+
  geom_point()+
  facet_wrap(~lake)+
  geom_smooth(color = "black", se = FALSE)+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "light depth", y = "mean carbon to chlorophyll")


# see if there is a relationship between percent light and C:chl
c.chl.avg = c.chl.avg %>%
  mutate(perc.light = NA) %>% 
  mutate(perc.light = replace(perc.light, ZID == 1, 100)) %>%
  mutate(perc.light = replace(perc.light, ZID == 2, 50)) %>%
  mutate(perc.light = replace(perc.light, ZID == 3, 25)) %>%
  mutate(perc.light = replace(perc.light, ZID == 4, 10)) %>%
  mutate(perc.light = replace(perc.light, ZID == 5, 5)) %>%
  mutate(perc.light = replace(perc.light, ZID == 6, 1))
  
  
  
ggplot(c.chl.avg, aes(x = log10(perc.light), y = mean.c.chl))+
  geom_point()+
  facet_wrap(~lake)+
  geom_smooth(color = "black", se = FALSE)+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "log10(percent light)", y = "mean carbon to chlorophyll")




ggplot(c.chl.avg, aes(x = (perc.light), y = mean.c.chl, fill = lake))+
  geom_point(size = 3,pch = 21)+
  facet_wrap(~lake)+
  geom_smooth(se = FALSE, span = 0.5, aes(color = lake))+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  geom_errorbar(
    aes(ymin = lower, ymax = upper, color = lake))+
  labs(x = "percent light", y = "mean carbon to chlorophyll")


ggplot(c.chl.avg, aes(x = log10(perc.light), y = mean.c.chl, fill = lake))+
  geom_point(size = 3,pch = 21)+
  facet_wrap(~lake)+
  geom_smooth(se = FALSE, span = 0.5, aes(color = lake))+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  geom_errorbar(
    aes(ymin = lower, ymax = upper, color = lake))+
  labs(x = "log10(percent light)", y = "mean carbon to chlorophyll")



ggplot(c.chl.avg, aes(x = log10(perc.light), y = log10(mean.c.chl), fill = lake))+
  geom_point(size = 3,pch = 21)+
  facet_wrap(~lake)+
  geom_smooth(se = FALSE, span = 0.5, aes(color = lake))+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "log10(percent light)", y = "mean carbon to chlorophyll")




# C to chl ratios are really related to light  

# plot all points related to light

c.chl = c.chl %>%
  mutate(perc.light = NA) %>% 
  mutate(perc.light = replace(perc.light, ZID == 1, 100)) %>%
  mutate(perc.light = replace(perc.light, ZID == 2, 50)) %>%
  mutate(perc.light = replace(perc.light, ZID == 3, 25)) %>%
  mutate(perc.light = replace(perc.light, ZID == 4, 10)) %>%
  mutate(perc.light = replace(perc.light, ZID == 5, 5)) %>%
  mutate(perc.light = replace(perc.light, ZID == 6, 1))


ggplot(c.chl, aes(x = (perc.light), y = log10(c.to.chl.corr), fill = lake))+
  geom_point(size = 3,pch = 21)+
  facet_wrap(~lake)+
  #geom_smooth(se = FALSE, span = 0.5, aes(color = lake))+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "percent light", y = "mean carbon to chlorophyll")
  



### combine with the actual light profiles ####
# we want a more fine-scale estimate of light percentage

# read in k
# fix the dates for k as well
k = get(load("./data/formatted data/light/light extinction coefficients 2022 and 2024.RData"))

k = k  %>% mutate(doy = replace(doy, doy == "178" & lake == "L", "179"))
k = k %>% mutate(doy = replace(doy, doy == "179" & lake == "T", "180"))

k = k %>% mutate(doy = as.numeric(doy))

# combine k to c.chl
c.chl = c.chl %>% left_join(k, by = c("doy", "lake", "year"))

# calculate the percent light based on k
c.chl = c.chl %>% mutate(perc.light.calc = 100*exp(k*depth))

ggplot(c.chl, aes(x = (perc.light.calc), y = (c.to.chl.corr), fill = lake))+
  geom_point(size = 3,pch = 21)+
  facet_wrap(~lake)+
  #geom_smooth(se = FALSE, span = 0.5, aes(color = lake))+
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  labs(x = "percent light", y = "mean carbon to chlorophyll")
