# calc k and interp light
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


# read in light
light = get(load("./data/formatted data/light/light profiles 2022 and 2024.RData"))


# calculate the light extinction coefficient by day
# we are calculating it as slope of the ln of the % of surface irrance vs. depth

k = light %>% group_by(doy, lake, year) %>% 
  summarize(k = coefficients(lm(log(Percent_surface_irradiance)~depth))[2])

# calculate depth of 1% light based on k
k = k %>% mutate(kcrit.depth = -4.60517/k)

# create a new matrix of blank spaces for light
light.maps <- expand.grid(
  doy = seq(134, 234, 1),
  lake = unique(k$lake),
  year = unique(light$year),
  depth = depths)


#k = k %>% select(-depth)
#k = k %>% select(-perc.light)

light.maps = light.maps %>% full_join(k, by = c("doy", "lake", "year"))

#light.maps = light.maps %>% filter(!is.na(k))

light.maps = light.maps %>%  mutate(perc.light = 100*exp(k*depth))

light.maps <- light.maps %>%
  # mutate(doy = as.factor(doy)) %>%
  complete(doy, depth, lake, year, fill = list(perc.light = NA))


light.maps = light.maps %>% mutate(doy = as.numeric(doy))


light.maps = light.maps %>% group_by(lake, depth, year)  %>% 
  mutate(perc.light = na.approx(perc.light, rule = 2))

ggplot(light.maps, aes_string("as.factor(doy)", "depth", fill = "log(perc.light)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(year~lake)+
  geom_line(aes(x = as.factor(doy), y = kcrit.depth))+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "light", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(140, 150, 170, 190, 211, 233)))


light.maps.all = light.maps

ggplot(light.maps.all, aes_string("as.factor(doy)", "depth", fill = "log(perc.light)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(year~lake)+
  #geom_point(aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = light.maps.all %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "black") +
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "light", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(140, 150, 170, 190, 211, 233)))



ggplot(light.maps.all, aes_string("as.factor(doy)", "depth", fill = "(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = light.maps.all %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "white") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "lightblue3","lightblue4", "darkblue", "black"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.05, 0.2, 0.4, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))



ggplot(light.maps.all, aes_string("as.factor(doy)", "depth", fill = "perc.light")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = light.maps.all %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "white") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "lightblue3","lightblue4", "darkblue", "black"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.5, 0.75, 0.9, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))



ggplot(light.maps.all, aes_string("as.factor(doy)", "depth", fill = "log(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = light.maps.all %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "black") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "#775B3E", "#39312F", "#1C1718"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.33, 0.66, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))



ggplot(light.maps.all %>% filter(!is.na(kcrit.depth)), aes(x = doy, y = kcrit.depth, color = as.factor(year)))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  facet_wrap(~lake)+
  theme_classic()+
  scale_y_reverse()+
  labs(y = "depth of 1% light", x = "day of year")









##### save light.maps and k ######
write.csv(light.maps.all, "./data/formatted data/light/interpolated light profiles 2022 and 2024.csv", row.names = FALSE)
save(light.maps.all, file = "./data/formatted data/light/interpolated light profiles 2022 and 2024.RData")


write.csv(k, "./data/formatted data/light/light extinction coefficients 2022 and 2024.csv", row.names = FALSE)
save(k, file = "./data/formatted data/light/light extinction coefficients 2022 and 2024.RData")




#### make a version of k that is the percent light for all depths and all dates estimated
# over time for both 2022 and 2024

### 2022 ###

k.22 = k %>% filter(year == 2022)

k.22.exp = expand.grid(doy = seq(min(k.22$doy, na.rm = TRUE), max(k.22$doy, na.rm = TRUE)),
                       lake = unique(k.22$lake),
                       depth = seq(0, 8, 0.25))

k.22.exp = k.22.exp %>% full_join(k.22, by = c("doy", "lake")) %>% 
  mutate(year = 2022)


# interpolate k
k.22.exp = k.22.exp %>% group_by(lake, depth) %>% 
  arrange(doy) %>% 
  mutate(k = na.approx(k, rule = 2))


# calculate percent light
k.22.exp = k.22.exp %>% mutate(perc.light = 100*exp(k*depth),
                               kcrit.depth = na.approx(kcrit.depth, rule = 2))




ggplot(k.22.exp, aes_string("as.factor(doy)", "depth", fill = "(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(DOY), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = k.22.exp %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "white") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "lightblue3","lightblue4", "darkblue", "black"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.5, 0.75, 0.9, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))





### 2024 ###

k.24 = k %>% filter(year == 2024)

k.24.exp = expand.grid(doy = seq(min(k.24$doy, na.rm = TRUE), max(k.24$doy, na.rm = TRUE)),
                       lake = unique(k.24$lake),
                       depth = seq(0, 8, 0.25))

k.24.exp = k.24.exp %>% full_join(k.24, by = c("doy", "lake")) %>% 
  mutate(year = 2024)


# interpolate k
k.24.exp = k.24.exp %>% group_by(lake, depth) %>% 
  arrange(doy) %>% 
  mutate(k = na.approx(k, rule = 2))


# calculate percent light
k.24.exp = k.24.exp %>% mutate(perc.light = 100*exp(k*depth),
                               kcrit.depth = na.approx(kcrit.depth, rule = 2))




ggplot(k.24.exp, aes_string("as.factor(doy)", "depth", fill = "(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(DOY), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
  geom_line(data = k.24.exp %>% filter(!is.na(kcrit.depth)), 
            aes(x = as.factor(doy), y = kcrit.depth, group = interaction(year, lake)), 
            linetype = "dashed", size = 0.5, inherit.aes = FALSE, color = "white") +
  labs(x = "day of year", y = "depth (m)") +
  scale_fill_gradientn(
    name = "light",
    colors = c("lightblue", "lightblue3","lightblue4", "darkblue", "black"), # Black at bottom, dark blue in the middle, yellow at the surface
    values = c(0, 0.5, 0.75, 0.9, 1), # Control the distribution of the gradient
    trans = "reverse" # Reverse the scale
  ) +
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))





## combine both expanded k dataframes
k.22.24.exp = rbind(k.22.exp, k.24.exp)



ggplot(k.22.24.exp, aes_string("as.factor(doy)", "depth", fill = "(perc.light)")) +
  geom_tile() +
  theme_classic() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  #geom_point(aes(x = as.factor(DOY), y = kcrit.depth, group = interaction(year, lake)), inherit.aes = FALSE, color = "black") +
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
  scale_x_discrete(breaks = as.character(c(140, 150, 170, 190, 211, 233)))



#### write the interpolated light to a dataframe ###

write.csv(k.22.24.exp, "./data/formatted data/light/interpolated light 2022 and 2024.csv", row.names = FALSE)
save(k.22.24.exp, file = "./data/formatted data/light/interpolated light 2022 and 2024.RData")
