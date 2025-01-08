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
  ungroup() %>% 
  group_by(variable) %>% # Group by 'variable' to normalize within each group
  mutate(
    mean_normalized = (mean - min(mean)) / (max(mean) - min(mean)) # Min-max normalization
  ) %>%
  ungroup() # Remove grouping


# read in the light data
k = get(load(file = "./data/formatted data/light/interpolated light 2022 and 2024.RData"))

# get average depth of 1% light
k.avg = k %>% group_by(lake, year) %>% 
  summarize(perc.1 = mean(kcrit.depth, na.rm = TRUE))

k.avg = k.avg %>% rename(depth = perc.1)







ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "temp")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.7) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  geom_path(size = 1) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "temp" = "black")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen")) +  # Use matching green for fill
  theme_classic()



ggplot(all.clines %>% filter(variable %in% c("chl_ugL")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "chl_ugL"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.7) +  # Fill to the left of chl_ugL curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  geom_path(size = 1) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("chl_ugL" = "forestgreen", "temp" = "black")) +
  scale_fill_manual(values = c("chl_ugL" = "forestgreen")) +  # Use matching green for fill
  theme_classic()

# when 1% of light is solidly in the metalimnion, there is no longer a DCM--it is no longer
# worthwhile for phytoplankton to grow at low light because it doesn't mean more nutrients





ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "do_percent")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.7) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "do_percent", aes(x = mean_nromalized, y = depth, color = "black"))) +
  geom_path(size = 0.8) +
  
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "do_percent" = "blue")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen")) +  # Use matching green for fill
  theme_classic()






ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "SPC")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.7) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "SPC", aes(x = mean_nromalized, y = depth, color = "black"))) +
  geom_path(size = 0.8) +
  
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "SPC" = "blue")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen")) +  # Use matching green for fill
  theme_classic()







ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "pH")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.7) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "pH", aes(x = mean_nromalized, y = depth, color = "black"))) +
  geom_path(size = 0.8) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "pH" = "blue")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen")) +  # Use matching green for fill
  theme_classic()



### add nutrients to these plots
# what percent light extinguishes growth?


# read in the nutrients data
nuts.all = get(load( "./data/formatted data/nutrients/2024 and 2022 TN TP.RData"))

nuts.all.avg = nuts.all %>% filter(is.na(flag)) %>% 
  group_by(lake, year, depth) %>% 
  summarize(TP = mean(TP, na.rm = TRUE), TN = mean(TN, na.rm = TRUE)) %>% 
  ungroup()

# normalize the data so it is between 0 and 1
nuts.all.avg.norm = nuts.all.avg %>% 
  mutate(
    TP_normalized = (TP - min(TP)) / (max(TP) - min(TP)) ,
    TN_normalized = (TN - min(TN)) / (max(TN) - min(TN))
  ) %>%
  ungroup() # Remove grouping

all.clines$depth.cat = NA

# classify cline dataset depths based on metalimnion data
for(i in 1:nrow(all.clines)){
  
  cur.lake = all.clines$lake[i]
  cur.year = all.clines$year[i]
  cur.depth = all.clines$depth[i]
  
  cur.met.top = meta.avg %>% filter(year == cur.year & lake == cur.lake) %>% 
    pull(mean.top)
  
  cur.met.bottom = meta.avg %>% filter(year == cur.year & lake == cur.lake) %>% 
    pull(mean.bottom)
  
  print(cur.met.top)
  print(cur.met.bottom)
  
  if(cur.depth < cur.met.top){
    all.clines$depth.cat[i] = "PML"
  }
  
  if(cur.depth >= cur.met.top & cur.depth <= cur.met.bottom){
    all.clines$depth.cat[i] = "Meta"
  }
  
  if(cur.depth > cur.met.bottom){
    all.clines$depth.cat[i] = "Hypo"
  }
  
  
}



### join the nutrient info to all.clines

cline.depths = all.clines %>% select(lake, year, depth, depth.cat)

nuts.all.avg.norm = nuts.all.avg.norm %>% rename(depth.cat = depth)

cline.depths.nuts = cline.depths %>% left_join(nuts.all.avg.norm, by = c("lake", "year", "depth.cat"))

# summarize to match other clines
TP.clines = cline.depths.nuts %>% 
group_by(lake, year, depth) %>% 
  summarize(mean = mean(TP, na.rm = TRUE), mean_normalized = mean(TP_normalized, na.rm = TRUE)) %>% 
  mutate(variable = "TP")


TN.clines = cline.depths.nuts %>% 
  group_by(lake, year, depth) %>% 
  summarize(mean = mean(TN, na.rm = TRUE), mean_normalized = mean(TN_normalized, na.rm = TRUE)) %>% 
  mutate(variable = "TN")

all.clines = all.clines %>% select(-depth.cat)

TN.TP.clines = rbind(TN.clines, TP.clines) %>% 
  select(lake, year, depth, mean, variable, mean_normalized)

all.clines = rbind(all.clines, TN.TP.clines)







ggplot(all.clines %>% filter(variable %in% c("chl_ugL", "TN")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "TN"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.1) +  # Fill to the left of chl_ugL curve
  geom_path(size = 1) +
  geom_ribbon(data = all.clines %>% filter(variable == "chl_ugL"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.85) +  # Fill to the left of chl_ugL curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("chl_ugL" = "forestgreen", "TN" = "black")) +
  scale_fill_manual(values = c("chl_ugL" = "forestgreen", "TN" = "cyan")) +  # Use matching green for fill
  theme_classic()





ggplot(all.clines %>% filter(variable %in% c("chl_ugL", "TP")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "TP"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.1) +  # Fill to the left of chl_ugL curve
  geom_path(size = 1) +
  geom_ribbon(data = all.clines %>% filter(variable == "chl_ugL"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.85) +  # Fill to the left of chl_ugL curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("chl_ugL" = "forestgreen", "TP" = "black")) +
  scale_fill_manual(values = c("chl_ugL" = "forestgreen", "TP" = "cyan")) +  # Use matching green for fill
  theme_classic()





### phycocyanin

ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "TN")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "TN"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.1) +  # Fill to the left of phyco_cells curve
  geom_path(size = 1) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.85) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "TN" = "black")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen", "TN" = "cyan")) +  # Use matching green for fill
  theme_classic()





ggplot(all.clines %>% filter(variable %in% c("phyco_cells", "TP")), aes(x = mean_normalized, y = depth, color = variable)) +
  geom_ribbon(data = all.clines %>% filter(variable == "TP"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.1) +  # Fill to the left of phyco_cells curve
  geom_path(size = 1) +
  geom_ribbon(data = all.clines %>% filter(variable == "phyco_cells"), 
              aes(xmin = -Inf, xmax = mean_normalized, y = depth, fill = variable), 
              inherit.aes = FALSE, alpha = 0.85) +  # Fill to the left of phyco_cells curve
  geom_hline(data = k.avg, aes(yintercept = depth,
                               group = interaction(lake, year)), color = "gold",
             inherit.aes = FALSE, linetype = "dashed", size = 1) +
  #geom_point(all.clines %>% filter(varible == "temp", aes(x = mean_nromalized, y = depth, color = "black"))) +
  facet_grid(lake ~ year) +
  scale_y_reverse() +
  geom_rect(data = meta.avg, 
            aes(xmin = -Inf, xmax = Inf, ymin = mean.top, ymax = mean.bottom), 
            inherit.aes = FALSE, alpha = 0.2) +
  scale_color_manual(values = c("phyco_cells" = "forestgreen", "TP" = "black")) +
  scale_fill_manual(values = c("phyco_cells" = "forestgreen", "TP" = "cyan")) +  # Use matching green for fill
  theme_classic()
