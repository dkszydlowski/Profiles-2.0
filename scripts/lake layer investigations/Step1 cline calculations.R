# investigate where the 'clines are of every variable


if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(rLakeAnalyzer)) install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)

library(purrr)

# read in the profiles data
profiles = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))


## salinity cline
# get the average salinity grouped by lake, year, and depth
sal.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.sal = mean(SPC, na.rm = TRUE)) %>% 
  mutate(variable = "SPC")


ggplot(sal.avg, aes(x = mean.sal, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()




## temperature cline
temp.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.temp = mean(temp, na.rm = TRUE)) %>% 
  mutate(variable = "temp")


ggplot(temp.avg, aes(x = mean.temp, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()




## chl_ugL cline
chl_ugL.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.chl_ugL = mean(chl_ugL, na.rm = TRUE)) %>% 
  mutate(variable = "chl_ugL")


ggplot(chl_ugL.avg, aes(x = mean.chl_ugL, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()




## phyco_cells cline
phyco_cells.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.phyco_cells = mean(phyco_cells, na.rm = TRUE)) %>% 
  mutate(variable = "phyco_cells")


ggplot(phyco_cells.avg, aes(x = mean.phyco_cells, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()



## do_percent cline
do_percent.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.do_percent = mean(do_percent, na.rm = TRUE)) %>% 
  mutate(variable = "do_percent")


ggplot(do_percent.avg, aes(x = mean.do_percent, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()



## pH cline
pH.avg = profiles %>% group_by(lake, year, depth) %>% 
  summarize(mean.pH = mean(pH, na.rm = TRUE)) %>% 
  mutate(variable = "pH")


ggplot(pH.avg, aes(x = mean.pH, y = depth))+
  geom_path()+
  geom_point()+
  facet_grid(lake~year)+
  scale_y_reverse()



# combine all the clines
# List of data frames to join
df_list <- list(temp.avg, pH.avg, chl_ugL.avg, phyco_cells.avg, do_percent.avg, sal.avg)

# rename the mean column
df_list <- lapply(df_list, function(df) {
  names(df) <- sub("^mean\\..*", "mean", names(df)) # Replace "mean.*" with "mean"
  df
})

# Full join all data frames by "id"
all.clines <- reduce(df_list, full_join, by = c("lake", "depth", "year", "variable", "mean"))


ggplot(all.clines, aes(x = mean, y = depth, color = variable))+
  geom_point()+
  geom_path()+
  facet_grid(lake~year)+
  scale_y_reverse()


# normalize data so we can compare
all.clines <- all.clines %>%
  group_by(variable) %>% # Group by 'variable' to normalize within each group
  mutate(
    mean_normalized = (mean - mean(mean)) / sd(mean) # Z-score normalization
  ) %>%
  ungroup() # Remove grouping


ggplot(all.clines %>% filter(variable %in% c("chl_ugL", "phyco_cells", "temp")), aes(x = mean_normalized, y = depth, color = variable))+
  geom_point()+
  geom_path(size =0.5)+
  facet_grid(lake~year)+
  scale_y_reverse()+
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  theme_classic()


## DCM seem to form right where the temp stops decreasing