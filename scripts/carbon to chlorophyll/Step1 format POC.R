# Step1 calculate carbon from raw TOC data

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(readxl)) install.packages('rea')
library(tidyverse)

#### 2022 ####
c = read.csv("./data/raw data/carbon/Wilkinson Szydlowski CN 041923 DKS.csv")
# Note, made a change to original file because date listed as this 
# Tuesday 2022-06-18
# should have been 2022-06-18

c = c %>% 
  rename(carbon = Carbon..mg.) %>% 
  filter(!is.na(lake) & !is.na(carbon))

# convert carbon to concentration by dividing by 0.15 L (volume filtered)
c = c %>% mutate(carbon = carbon/0.15)

# convert to ug/L to match chlorophyll
c = c %>% mutate(carbon = carbon*1000)


c = c %>% rename(depth = poc)
c$depth = as.integer(c$depth) # make depth an integer


# remove any carbon rows without a date
c = c %>% filter(lake %in% c("L", "R", "T"))

c$depth = as.integer(c$depth)

# visualize carbon over depths
ggplot(c, aes(x = depth, y = carbon, group = depth))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~lake)


# format c for saving, with lake, year, date, and ZID
c = c %>% mutate(year = year(mdy(date)), date = mdy(date)) %>% 
  rename(ZID = depth) %>% 
  select(lake, year, date, ZID, carbon)


# save the file 
write.csv(c, "./data/formatted data/carbon to chlorophyll/carbon 2022.csv", row.names = FALSE)
save(c, file = "./data/formatted data/carbon to chlorophyll/carbon 2022.RData")


