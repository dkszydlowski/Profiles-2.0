
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


##### ROUTINES #####
#### 2022 ####
chl22 = read.csv("./data/raw data/manual chlorophyll/chl_fluorometer_and_concentrations.csv")

# fix couple of dates that were off from actual sampling date
# but need to check the binder
chl22 = chl22 %>% mutate(Date = replace(Date, Date == "6/27/2022" & Lake == "L", "6/28/2022"))
chl22 = chl22 %>% mutate(Date = replace(Date, Date == "6/28/2022" & Lake == "T", "6/29/2022"))

# recalculate doy
chl22 = chl22 %>% mutate(DoY = yday(mdy(Date)))

# remove rows where lake is NA
chl22 = chl22 %>% filter(!is.na(Lake))

# calculate the nearest quarter meter to the light depth
chl22 = chl22 %>% mutate(depth.rounded = round(Depth_m * 4) / 4)

chl22 = chl22 %>% select(Lake, Year, DoY, ZID, Depth_m, depth.rounded, Mean_Chl)

chl22 = chl22 %>% rename(lake = Lake, year = Year, doy = DoY, depth = Depth_m, manual.chl = Mean_Chl)



chl22 = chl22 %>% filter(year != 2021)


#### 2024 ####
chl24 = get(load(file = "./data/formatted data/manual chlorophyll/routines chl 2024.RData"))

# remove negative chlorophyll values
chl24 = chl24 %>% filter(Chlorophyll >= 0)

# calculate the nearest quarter meter to the light depth
chl24 = chl24 %>% mutate(depth.rounded = round(Depth * 4) / 4)

# create a DoY ccolumn
chl24 = chl24 %>% mutate(DoY = yday(mdy(Date)))

# create a year column
chl24 = chl24 %>% mutate(Year = 2024)

# select the columns we want
chl24 = chl24 %>% select(Lake, Year, DoY, Depth, depth.rounded, Chlorophyll)

# rename the columns to match
chl24 = chl24 %>% rename(lake = Lake, year = Year, doy = DoY, depth = Depth, manual.chl = Chlorophyll)

# summarize by day and depth, take average of reps
chl24 = chl24 %>% 
  group_by(lake, year, doy, depth, depth.rounded) %>% 
  summarize(manual.chl = mean(manual.chl, na.rm = TRUE))

# create a ZID column
chl24 <- chl24 %>%
  group_by(lake, doy) %>%              # Group by lake and DoY
  mutate(ZID = rank(depth)) %>%        # Rank depths within each group
  ungroup()      

# reorder columns how we want them
chl24 = chl24 %>% select(lake, year, doy, ZID, depth, depth.rounded, manual.chl)

# remove one row with NA for depth
chl24 = chl24 %>% filter(!is.na(depth))


# combine the two dataframes
chl.22.24 = rbind(chl22, chl24)

# standardize lake names
chl.22.24 = chl.22.24 %>% mutate(lake = replace(lake, lake == "Tuesday", "T")) %>% 
                          mutate(lake = replace(lake, lake == "Peter", "R")) %>% 
                          mutate(lake = replace(lake, lake == "Paul", "L"))

# save the overall dataframe
write.csv(chl.22.24, "./data/formatted data/manual chlorophyll/routines chl 2024 and 2022.csv", row.names = FALSE)
save(chl.22.24, file = "./data/formatted data/manual chlorophyll/routines chl 2024 and 2022.RData")



### make a quick boxplot
ggplot(chl.22.24, aes(x = as.factor(year), y = log10(manual.chl)))+
  geom_boxplot()+
  facet_wrap(~lake)+
  theme_classic()+
  labs( y = "log10(chlorophyll ug/L)", x = "year")





##### format chl without the acid correction ####

##### ROUTINES #####
#### 2022 ####
chl22 = read.csv("./data/raw data/manual chlorophyll/chl_fluorometer_and_concentrations 2022 no acid correction.csv")

# fix couple of dates that were off from actual sampling date
# but need to check the binder
chl22 = chl22 %>% mutate(Date = replace(Date, Date == "6/27/2022" & Lake == "L", "6/28/2022"))
chl22 = chl22 %>% mutate(Date = replace(Date, Date == "6/28/2022" & Lake == "T", "6/29/2022"))

# recalculate doy
chl22 = chl22 %>% mutate(DoY = yday(mdy(Date)))

# remove rows where lake is NA
chl22 = chl22 %>% filter(!is.na(Lake))

# calculate the nearest quarter meter to the light depth
chl22 = chl22 %>% mutate(depth.rounded = round(Depth_m * 4) / 4)

chl22 = chl22 %>% select(Lake, Year, DoY, ZID, Depth_m, depth.rounded, Mean_Chl)

chl22 = chl22 %>% rename(lake = Lake, year = Year, doy = DoY, depth = Depth_m, manual.chl = Mean_Chl)



chl22 = chl22 %>% filter(year != 2021)


#### 2024 ####
chl24 = get(load(file = "./data/formatted data/manual chlorophyll/routines chl 2024 no acid correction.RData"))

# remove negative chlorophyll values
chl24 = chl24 %>% filter(Chlorophyll >= 0)

# calculate the nearest quarter meter to the light depth
chl24 = chl24 %>% mutate(depth.rounded = round(Depth * 4) / 4)

# create a DoY ccolumn
chl24 = chl24 %>% mutate(DoY = yday(mdy(Date)))

# create a year column
chl24 = chl24 %>% mutate(Year = 2024)

# select the columns we want
chl24 = chl24 %>% select(Lake, Year, DoY, Depth, depth.rounded, Chlorophyll)

# rename the columns to match
chl24 = chl24 %>% rename(lake = Lake, year = Year, doy = DoY, depth = Depth, manual.chl = Chlorophyll)

# summarize by day and depth, take average of reps
chl24 = chl24 %>% 
  group_by(lake, year, doy, depth, depth.rounded) %>% 
  summarize(manual.chl = mean(manual.chl, na.rm = TRUE))

# create a ZID column
chl24 <- chl24 %>%
  group_by(lake, doy) %>%              # Group by lake and DoY
  mutate(ZID = rank(depth)) %>%        # Rank depths within each group
  ungroup()      

# reorder columns how we want them
chl24 = chl24 %>% select(lake, year, doy, ZID, depth, depth.rounded, manual.chl)

# remove one row with NA for depth
chl24 = chl24 %>% filter(!is.na(depth))


# combine the two dataframes
chl.22.24 = rbind(chl22, chl24)

# standardize lake names
chl.22.24 = chl.22.24 %>% mutate(lake = replace(lake, lake == "Tuesday", "T")) %>% 
  mutate(lake = replace(lake, lake == "Peter", "R")) %>% 
  mutate(lake = replace(lake, lake == "Paul", "L"))

# save the overall dataframe
write.csv(chl.22.24, "./data/formatted data/manual chlorophyll/routines chl 2024 and 2022 no acid correction.csv", row.names = FALSE)
save(chl.22.24, file = "./data/formatted data/manual chlorophyll/routines chl 2024 and 2022 no acid correction.RData")
