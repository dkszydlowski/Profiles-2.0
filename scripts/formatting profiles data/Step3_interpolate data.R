#### interpolate the data ####

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(zoo)) install.packages('zoo')
library(zoo)

prof.clean = get(load("./data/formatted data/cleaned uninterpolated profiles.RData"))

# remove the flagged data
prof.clean = prof.clean %>% mutate(pH = replace(pH, flag == "pH", NA))

plot_profiles_heatmap(prof.clean, plot.year = "all", "log10(chl_ugL)", plot.lake = "all", log.trans = FALSE)


#### interpolate over depths #####

# first, we need to expand the grid so that all depths are included
# (every 0.25 m from 0 to 8)

# remove depths > 8 from when we tested going deeper with the sonde
prof.clean = prof.clean %>% filter(depth <= 8)
# prof.all = prof.all %>% filter(!is.na(chl_ugL.interp))

# split in two so we can approximate to unique dates per year
prof.clean.22 = prof.clean %>% filter(year == 2022)
prof.clean.24 = prof.clean %>% filter(year == 2024)

# make sure that every depth is represented on every day
depths = seq(0, 8, 0.25)
doys22 = unique(prof.clean.22 %>% pull(doy))
doys24 = unique(prof.clean.24 %>% pull(doy))
lakes = c("L", "R", "T")

# create dataframes with the days, lakes, and depth combinations we want to fill
depths.and.days22 = expand.grid(lake = lakes, doy = doys22, depth = depths, year = 2022)
depths.and.days24 = expand.grid(lake = lakes, doy = doys24, depth = depths, year = 2024)

prof.clean.22 = left_join(depths.and.days22, prof.clean.22, by = c("lake", "doy", "depth", "year"))
prof.clean.24 = left_join(depths.and.days24, prof.clean.24, by = c("lake", "doy", "depth", "year"))



# depths.and.days = depths.and.days %>% arrange(lake, depth, doys2, year)

prof.clean.22 = prof.clean.22 %>% 
  group_by(lake, year, doy) %>% 
  arrange(depth) %>% 
  mutate(chl_ugL = na.approx(chl_ugL, na.rm = FALSE, rule = 2),
         do_percent = na.approx(do_percent, na.rm = FALSE, rule = 2),
         SPC = na.approx(SPC, na.rm = FALSE, rule = 2),
         phyco_cells = na.approx(phyco_cells, na.rm = FALSE, rule = 2),
         pH = na.approx(pH, na.rm = FALSE, rule = 2),
         temp = na.approx(temp, na.rm = FALSE, rule = 2),
         do_mgL = na.approx(do_mgL, na.rm = FALSE, rule = 2))


prof.clean.24 = prof.clean.24 %>% 
  group_by(lake, year, doy) %>% 
  arrange(depth) %>% 
  mutate(chl_ugL = na.approx(chl_ugL, na.rm = FALSE, rule = 2),
         do_percent = na.approx(do_percent, na.rm = FALSE, rule = 2),
         SPC = na.approx(SPC, na.rm = FALSE, rule = 2),
         phyco_cells = na.approx(phyco_cells, na.rm = FALSE, rule = 2),
         pH = na.approx(pH, na.rm = FALSE, rule = 2),
         temp = na.approx(temp, na.rm = FALSE, rule = 2),
         do_mgL = na.approx(do_mgL, na.rm = FALSE, rule = 2))


prof.clean = rbind(prof.clean.22, prof.clean.24)

plot_profiles_heatmap(prof.clean, plot.year = "all", "SPC", plot.lake = "all", log.trans = FALSE)

## save interpolated over depths ##
write.csv(prof.clean, "./data/formatted data/cleaned profiles interpolated over depth.csv", row.names = FALSE)
save(prof.clean, file = "./data/formatted data/cleaned profiles interpolated over depth.RData")



#### interpolate over time ####
# interpolate so that every day has a profile

# make sure that every depth is represented on every day
depths = seq(0, 8, 0.25)
doys22 = seq(min(prof.clean.22$doy), max(prof.clean.22$doy), 1)
doys24 = seq(min(prof.clean.24$doy), max(prof.clean.24$doy), 1)
lakes = c("L", "R", "T")

# create dataframes with the days, lakes, and depth combinations we want to fill
depths.and.days22 = expand.grid(lake = lakes, doy = doys22, depth = depths, year = 2022)
depths.and.days24 = expand.grid(lake = lakes, doy = doys24, depth = depths, year = 2024)

prof.clean.22 = left_join(depths.and.days22, prof.clean.22, by = c("lake", "doy", "depth", "year"))
prof.clean.24 = left_join(depths.and.days24, prof.clean.24, by = c("lake", "doy", "depth", "year"))



prof.clean.22.time = prof.clean.22 %>% 
  group_by(lake, year, depth) %>% 
  arrange(depth) %>% 
  mutate(chl_ugL = na.approx(chl_ugL, na.rm = FALSE, rule = 2),
         do_percent = na.approx(do_percent, na.rm = FALSE, rule = 2),
         SPC = na.approx(SPC, na.rm = FALSE, rule = 2),
         phyco_cells = na.approx(phyco_cells, na.rm = FALSE, rule = 2),
         pH = na.approx(pH, na.rm = FALSE, rule = 2),
         temp = na.approx(temp, na.rm = FALSE, rule = 2),
         do_mgL = na.approx(do_mgL, na.rm = FALSE, rule = 2))


prof.clean.24.time = prof.clean.24 %>% 
  group_by(lake, year, depth) %>% 
  arrange(depth) %>% 
  mutate(chl_ugL = na.approx(chl_ugL, na.rm = FALSE, rule = 2),
         do_percent = na.approx(do_percent, na.rm = FALSE, rule = 2),
         SPC = na.approx(SPC, na.rm = FALSE, rule = 2),
         phyco_cells = na.approx(phyco_cells, na.rm = FALSE, rule = 2),
         pH = na.approx(pH, na.rm = FALSE, rule = 2),
         temp = na.approx(temp, na.rm = FALSE, rule = 2),
         do_mgL = na.approx(do_mgL, na.rm = FALSE, rule = 2))

prof.clean.time = rbind(prof.clean.22.time, prof.clean.24.time)

plot_profiles_heatmap(prof.clean.time, plot.year = "all", "temp", plot.lake = "all", log.trans = FALSE)

write.csv(prof.clean.time, "./data/formatted data/cleaned profiles interpolated over depth and time.csv", row.names = FALSE)
save(prof.clean.time, file = "./data/formatted data/cleaned profiles interpolated over depth and time.RData")


## still need to finalize: couple of days of really low DO in 2024
# do_mgL not interpolated, yet
