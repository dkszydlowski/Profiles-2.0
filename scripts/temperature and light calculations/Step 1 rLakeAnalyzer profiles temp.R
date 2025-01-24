## format profiles temperature for rLakeAnalyzer

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(rLakeAnalyzer)) install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)

# read in the profiles data
profiles = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

# select just temperature
temp = profiles %>% select(lake, year, doy, depth, temp)

# create a date column from doy and year
# first ungroup temp
temp = temp %>% ungroup()

temp <- temp %>%
  mutate(date =as.Date(doy - 1, origin = paste0(year, "-01-01")))

# pivot longer to be rLakeAnalyzer format
temp_wider <- temp %>% pivot_wider(names_from = depth,  values_from = temp, names_prefix = "wtr_")

# loop through and calculate thermoclines for all of the lakes
lakes = unique(temp_wider$lake)
years = unique(temp_wider$year)


for(i in 1:length(lakes)){
  for(j in 1:length(years)){
    
    cur.lake = lakes[i]
    cur.year = years[j]
    
    data = temp_wider %>% 
      filter(lake == cur.lake & year == cur.year) %>% 
      rename(datetime = date) %>% 
      select(-lake, -year, -doy)
    
    thermoclines = ts.thermo.depth(data, seasonal = TRUE)
    metalimnion = ts.meta.depths(data, slope = 0.5)
    
    thermoclines$lake = cur.lake
    thermoclines$year = cur.year
    
    metalimnion$lake = cur.lake
    metalimnion$year = cur.year
    
    if(i == 1 & j == 1){
      all.thermo = thermoclines
      all.meta = metalimnion
    }else{all.thermo = rbind(all.thermo, thermoclines)
          all.meta = rbind(all.meta, metalimnion)
    }
    
    
    
    
    
  }
}

# create a doy column
all.thermo = all.thermo %>% mutate(doy = yday(datetime))
  
ggplot(all.thermo, aes(x = doy, y = thermo.depth, color = as.factor(year)))+
  geom_line(size = 1)+
  facet_grid(~lake)+
  scale_y_reverse()
  #geom_smooth(method = "lm", se = FALSE, style = "dashed")

all.meta = all.meta %>% mutate(doy = yday(datetime))

ggplot(all.meta, aes(x = doy, y = bottom, color = as.factor(year)))+
  geom_line(size = 1)+
  geom_line(aes(x = doy, y = top))+
  facet_grid(~lake)+
  scale_y_reverse()
#geom_smooth(method = "lm", se = FALSE, style = "dashed")


# summarize all.meta by lake and year
meta.avg = all.meta %>% 
  group_by(lake, year) %>% 
  summarize(mean.bottom = mean(bottom, na.rm = TRUE),
            mean.top = mean(top, na.rm = TRUE))

