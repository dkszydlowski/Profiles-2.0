# compare DCM depth to light

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


if (!require(zoo)) install.packages('zoo')
library(zoo)

View(k)

# read in the light maps and the light extinction coefficients
light.maps.all = get(load("./data/formatted data/light/light extinction coefficients 2022 and 2024.RData"))

light.maps.all = get(load(file = "./data/formatted data/light/interpolated light 2022 and 2024.RData"))


light.maps.all.formatted = light.maps.all


light.maps.all.formatted = light.maps.all.formatted %>% mutate(year = as.factor(year))

light.maps.all.formatted = light.maps.all.formatted %>% filter(depth == 0)

# add k to the DCM data

# read in the DCM depths
DCM.depths = get(load("./data/formatted data/DCM depths/DCM depths.RData"))

DCM.depths.light = DCM.depths %>% select(-depth) %>% 
  left_join(light.maps.all.formatted, by = c("doy", "lake", "year"))


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
            median.DCM.light = median(DCM.perc.light, na.rm = TRUE),
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




#### heatmap of DCM and light ####



ggplot(light.maps.all %>% filter(!(year == 2024 & doy < 162)),
       aes_string("as.factor(doy)", "depth", fill = "(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(DOY), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_point(data = DCM.depths %>% filter(doy <= 234), 
            aes(x = as.factor(doy), y = DCM.depth, group = interaction(year, lake)), 
            size = 1, inherit.aes = FALSE, color = "lightblue") +
  geom_line(data = k.22.24.exp %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "white") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "lightblue3","lightblue4", "darkblue", "black"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.5, 0.75, 0.9, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(150, 170, 190, 211, 233)))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))

