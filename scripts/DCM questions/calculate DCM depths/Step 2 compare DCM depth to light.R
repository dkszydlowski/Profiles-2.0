# compare DCM depth to light

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


if (!require(zoo)) install.packages('zoo')
library(zoo)

View(k)

light.maps.all.formatted = light.maps.all %>% rename(doy = DOY, lake = Lake) 


light.maps.all.formatted = light.maps.all.formatted %>%
  mutate(lake = replace(lake, lake == "R", "Peter"),
         lake = replace(lake, lake == "L", "Paul"),
         lake = replace(lake, lake == "T", "Tuesday"))


light.maps.all.formatted = light.maps.all.formatted %>% mutate(year = as.factor(year))

light.maps.all.formatted = light.maps.all.formatted %>% group_by(lake, depth, year)  %>% 
  mutate(k = na.approx(k, rule = 2))

light.maps.all.formatted = light.maps.all.formatted %>% filter(depth == 0)

# add k to the DCM data

DCM.depths.light = DCM.depths %>% left_join(light.maps.all.formatted, by = c("doy", "lake", "year"))


# calculate the percent light at the DCM
DCM.depths.light = DCM.depths.light %>% mutate(DCM.perc.light = 100 * exp(k * DCM.depth))


L.DCM.depths = DCM.depths.light %>% filter(lake == "Paul")

ggplot(DCM.depths.light, aes (x = lake, y = DCM.perc.light, fill = lake))+
  geom_boxplot()+
  facet_wrap(~year)+
  theme_classic()

mean(L.DCM.depths$DCM.perc.light, na.rm = TRUE)



## summarize by lake and year

DCM.summary = DCM.depths.light %>%
  filter(!is.na(DCM.depth)) %>% 
  group_by(lake, year) %>% 
  summarize(mean.DCM.light = mean(DCM.perc.light, na.rm = TRUE),
            mean.DCM.depth = mean(DCM.depth, na.rm = TRUE),
            mean.DCM.chl = mean(DCM.chl, na.rm = TRUE),
            n.DCM = n())


ggplot(DCM.depths.light, aes(x = doy, y = DCM.perc.light, color = year))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)+
  theme_classic()


ggplot(DCM.depths.light, aes(x = doy, y = DCM.depth, color = year))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)+
  theme_classic()+
  scale_y_reverse()
