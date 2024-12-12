# identify when the DCM actually exists
# identify what percent light the DCM exists at


# figure out when the DCM disappears

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# read in the profiles data

prof.all.interp = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

prof.all.interp = prof.all.interp %>% ungroup()

cutoff.depth = 2

# filter so we compare to the top 2 m of the water column
DCM.depths = prof.all.interp %>% 
  filter(depth <= cutoff.depth) %>% 
  group_by(lake, year, doy) %>% 
  summarize(mean.epi.chl = mean(chl_ugL, na.rm = TRUE)) %>% 
  mutate(chl.thresh = mean.epi.chl *1.5) %>% 
  ungroup()

DCM.depths = DCM.depths %>% mutate(DCM.depth = NA, DCM.chl = NA)
# loop through and add the depth of max chlorophyll
for(i in 1:nrow(DCM.depths)){
  
  cur.year = DCM.depths$year[i]
  cur.lake = DCM.depths$lake[i]
  cur.doy = DCM.depths$doy[i]
  cur.thresh = DCM.depths$chl.thresh[i]
  
  cur.data = prof.all.interp %>% 
    filter(year == cur.year, lake == cur.lake, doy == cur.doy, depth > cutoff.depth, 
           chl_ugL >= cur.thresh)
  
  max_chl_depth = cur.data %>%
    filter(chl_ugL == max(chl_ugL, na.rm = TRUE))
  
  # View the depth and corresponding chlorophyll value
  if(nrow(max_chl_depth > 0)){
  DCM.depths$DCM.depth[i] = max_chl_depth %>%
    pull(depth)
  
  DCM.depths$DCM.chl[i] = max_chl_depth %>%
    pull(chl_ugL)
  }
  
  
  
}


DCM.depths = DCM.depths %>% mutate(year = as.factor(year), doy = as.numeric(doy))


ggplot(DCM.depths, aes(x = doy, y = DCM.depth, color = (year)))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_y_reverse()+
  theme_classic()



ggplot(DCM.depths, aes(x = doy, y = (DCM.chl), color = year))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_classic()


### plot DCM depths on the profiles heatmap ###
DCM.depths = DCM.depths %>% mutate(depth = DCM.depth)

ggplot(prof.all.interp, aes_string("doy", "depth", fill = "log10(chl_ugL)"))+
         geom_tile()+
         #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
         theme_classic()+
         scale_y_reverse()+
         facet_grid(year~lake)+
         #scale_fill_distiller(palette = "BrBG")
         scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  geom_point(data = DCM.depths, aes(x = doy, y = depth, group = lake), pch = 21, fill = "white", alpha = 0.7, inherit.aes = FALSE)


# save the DCM depths 
write.csv(DCM.depths, "./data/formatted data/DCM depths/DCM depths.csv", row.names = FALSE)
save(DCM.depths, file = "./data/formatted data/DCM depths/DCM depths.RData")


##### calculate the deep phycocyanin maximum

# filter so we compare to the top 2 m of the water column
DPM.depths = prof.all.interp %>% 
  filter(depth <= cutoff.depth) %>% 
  group_by(lake, year, doy) %>% 
  summarize(mean.epi.phyco = mean(phyco_cells, na.rm = TRUE)) %>% 
  mutate(phyco.thresh = mean.epi.phyco *1.5) %>% 
  ungroup()

DPM.depths = DPM.depths %>% mutate(DPM.depth = NA, DPM.phyco = NA)
# loop through and add the depth of max phycocyanin
for(i in 1:nrow(DPM.depths)){
  
  cur.year = DPM.depths$year[i]
  cur.lake = DPM.depths$lake[i]
  cur.doy = DPM.depths$doy[i]
  cur.thresh = DPM.depths$phyco.thresh[i]
  
  cur.data = prof.all.interp %>% 
    filter(year == cur.year, lake == cur.lake, doy == cur.doy, depth > cutoff.depth, 
           phyco_cells >= cur.thresh)
  
  max_phyco_depth = cur.data %>%
    filter(phyco_cells == max(phyco_cells, na.rm = TRUE))
  
  # View the depth and corresponding phycocyanin value
  if(nrow(max_phyco_depth > 0)){
    DPM.depths$DPM.depth[i] = max_phyco_depth %>%
      pull(depth)
    
    DPM.depths$DPM.phyco[i] = max_phyco_depth %>%
      pull(phyco_cells)
  }
  
  
  
}


DPM.depths = DPM.depths %>% mutate(year = as.factor(year), doy = as.numeric(doy))


ggplot(DPM.depths, aes(x = doy, y = DPM.depth, color = (year)))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_y_reverse()+
  theme_classic()



ggplot(DPM.depths, aes(x = doy, y = (DPM.phyco), color = year))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_classic()


### plot DPM depths on the profiles heatmap ###
DPM.depths = DPM.depths %>% mutate(depth = DPM.depth)

ggplot(prof.all.interp, aes_string("doy", "depth", fill = "log10(phyco_cells)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(year~lake)+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  geom_point(data = DPM.depths, aes(x = doy, y = depth, group = lake), pch = 21, fill = "white", alpha = 0.7, inherit.aes = FALSE)







##### Deep oxygen max (DOM) ######
# will not use 1.5 threshold for DO
cutoff.depth = 0

# filter so we compare to the top 2 m of the water column
DOM.depths = prof.all.interp %>% 
  filter(depth <= cutoff.depth) %>% 
  group_by(lake, year, doy) %>% 
  summarize(mean.epi.do = mean(do_percent, na.rm = TRUE)) %>% 
 # mutate(do.thresh = mean.epi.do *1.5) %>% 
  ungroup()

DOM.depths = DOM.depths %>% mutate(DOM.depth = NA, DOM.do = NA)
# loop through and add the depth of max oxygen
for(i in 1:nrow(DOM.depths)){
  
  cur.year = DOM.depths$year[i]
  cur.lake = DOM.depths$lake[i]
  cur.doy = DOM.depths$doy[i]
  #cur.thresh = DOM.depths$do.thresh[i]
  
  cur.data = prof.all.interp %>% 
    filter(year == cur.year, lake == cur.lake, doy == cur.doy, depth > cutoff.depth)
  
  max_do_depth = cur.data %>%
    filter(do_percent == max(do_percent, na.rm = TRUE))
  
  # View the depth and corresponding oxygen value
  if(nrow(max_do_depth > 0)){
    DOM.depths$DOM.depth[i] = max_do_depth %>%
      pull(depth)
    
    DOM.depths$DOM.do[i] = max_do_depth %>%
      pull(do_percent)
  }
  
  
  
}


DOM.depths = DOM.depths %>% mutate(year = as.factor(year), doy = as.numeric(doy))


ggplot(DOM.depths, aes(x = doy, y = DOM.depth, color = (year)))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  scale_y_reverse()+
  theme_classic()



ggplot(DOM.depths, aes(x = doy, y = (DOM.do), color = year))+
  facet_wrap(~lake)+
  geom_point(size = 2)+
  geom_line(size = 1)+
  theme_classic()


### plot DOM depths on the profiles heatmap ###
DOM.depths = DOM.depths %>% mutate(depth = DOM.depth)

ggplot(prof.all.interp, aes_string("doy", "depth", fill = "(do_percent)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(year~lake)+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  geom_line(data = DOM.depths, aes(x = doy, y = depth, group = lake), linewidth = 0.7, pch = 21, fill = "white", alpha = 0.7, inherit.aes = FALSE)+
  geom_point(data = DCM.depths %>% filter(doy <= 234), 
             aes(x = (doy), y = DCM.depth, group = interaction(year, lake)), 
             size = 1, inherit.aes = FALSE, color = "black")






