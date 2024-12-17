
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
temp.hist = temp.hist %>% rename(datetime = sampledate)

temp.hist.wide = temp.hist %>% 
  filter(depth <= 12) %>% 
  pivot_wider(id_cols = c("lakeid", "year4", "datetime"), names_from = "depth",
                                           names_prefix = "wtr_", names_sep = "_", values_from = "temperature_C")


temp.hist.wide = temp.hist.wide %>%
  select( -wtr_5.8, -wtr_8.5, -wtr_2.8, -wtr_11.5, -wtr_9.5, -wtr_6.75, -wtr_0.25, -wtr_0.75)

temp.hist.wide = temp.hist.wide %>%
  select(order(names(.)))

temp.hist.wide = temp.hist.wide %>% select(-year4)

temp.hist.wide.L = temp.hist.wide %>% filter(lakeid == "L") %>% select(-lakeid) %>% 
  mutate(datetime = ymd(datetime))

metalimnion.L = ts.meta.depths(temp.hist.wide.L, na.rm = TRUE) %>% mutate(lakeid = "L")



temp.hist.wide.R = temp.hist.wide %>% filter(lakeid == "R") %>% select(-lakeid) %>% 
  mutate(datetime = ymd(datetime))

metalimnion.R = ts.meta.depths(temp.hist.wide.R, na.rm = TRUE) %>% mutate(lakeid = "R")



temp.hist.wide.T = temp.hist.wide %>% filter(lakeid == "T") %>% select(-lakeid) %>% 
  mutate(datetime = ymd(datetime))

metalimnion.T = ts.meta.depths(temp.hist.wide.T, na.rm = TRUE) %>% mutate(lakeid = "T")




temp.hist.wide.W = temp.hist.wide %>% filter(lakeid == "Ward") %>% select(-lakeid) %>% 
  mutate(datetime = ymd(datetime))

metalimnion.W = ts.meta.depths(temp.hist.wide.W, na.rm = TRUE) %>% mutate(lakeid = "Ward")

# combine all of the meta depths
metalimnion.all = rbind(metalimnion.L, metalimnion.R, metalimnion.T, metalimnion.W)



### add the 1% light data
# get the 1% light by date and lake
one.perc.light.hist = chl.hist %>% filter(depth_id == 6)

one.perc.light.hist = one.perc.light.hist %>% select(lakeid, sampledate, chla, depth) %>% 
  rename(datetime = sampledate) %>% 
  mutate(datetime = ymd(datetime))

# add to metalimnion.all
metalimnion.all = metalimnion.all %>% left_join(one.perc.light.hist, by = c("lakeid", "datetime"))


# take mean by lake year
metalimnion.avg = metalimnion.all %>% mutate(year = year(datetime)) %>% 
  group_by(lakeid, year) %>% 
  summarize(mean.1.perc.light = mean(depth, na.rm = TRUE), mean.chl = mean(chla, na.rm= TRUE),
            mean.meta.bottom = mean(bottom, na.rm = TRUE))


metalimnion.avg = metalimnion.avg %>% mutate(comp = mean.1.perc.light > mean.meta.bottom)

ggplot(metalimnion.avg, aes(x = comp, y = mean.chl))+
  geom_boxplot()+
  facet_wrap(~lakeid)




### try across ratio ##
df_ratio <- chl.hist %>%
  # Filter only the required depths (1 and 6)
  filter(depth_id %in% c(1, 6))
  # Pivot the data so chla values for depth_id 1 and 6 are in separate columns
  

df_ratio %>% 
  ungroup() %>% 
pivot_wider(
    id_cols = c(lakeid, sampledate),
    names_from = depth_id,
    values_from = chla,
    names_prefix = "chla_depth_"
  )


  # Calculate the ratio
  mutate(chla.ratio = chla_depth_6 / chla_depth_1) %>%
  # Select the required columns
  select(lakeid, sampledate, chla.ratio)
