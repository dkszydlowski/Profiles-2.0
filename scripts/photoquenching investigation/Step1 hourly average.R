#### investigate photoquenching ####

# first, just take average across all summer data for the summer by hour and plot

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

# read in the Paul surface data 
paul.surf = read.csv("G:/My Drive/Projects and Papers/PhD/Profiles 2.0/Profiles-2.0/data/raw data/surface sonde data 2024/Paul FINAL 2024-10-14.csv")

# convert datetime to a datetime format
paul.surf = paul.surf %>% mutate(datetime = ymd_hms(datetime))

# calculate the hour
paul.surf = paul.surf %>% mutate(hour = hour(datetime))

paul.surf = remove.flagged.data(paul.surf)

# summarize by hour
paul.avg = paul.surf %>%
  group_by(hour) %>% 
  summarize(chl.avg = mean(Chl_HYLB, na.rm = TRUE), phyco.avg = mean(BGA_HYLB, na.rm = TRUE),
            chl.median = median(Chl_HYLB, na.rm = TRUE), phyco.median = median(BGA_HYLB, na.rm = TRUE))

# average chlorophyll
avg.chl.plot = ggplot(paul.avg, aes(x = hour, y = chl.avg)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "avg hourly chl (ug/L)", x = "")+
  theme_classic()

# median chlorophyll
median.chl.plot = ggplot(paul.avg, aes(x = hour, y = chl.median)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "median hourly chl (ug/L)", x = "hour of the day")+
  theme_classic()

# average phyco
avg.phyco.plot = ggplot(paul.avg, aes(x = hour, y = phyco.avg)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "avg hourly phyco (cells/mL)", x = "")+
  theme_classic()


# median phyco
median.phyco.plot = ggplot(paul.avg, aes(x = hour, y = phyco.median)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "median hourly phyco (cells/mL)", x = "hour of the day")+
  theme_classic()

ggarrange(avg.chl.plot, avg.phyco.plot, median.chl.plot, median.phyco.plot, nrow = 2, ncol = 2)





# read in the Tuesday surface data 
tuesday.surf = read.csv("G:/My Drive/Projects and Papers/PhD/Profiles 2.0/Profiles-2.0/data/raw data/surface sonde data 2024/Tuesday FINAL 2024-10-14.csv")

# convert datetime to a datetime format
tuesday.surf = tuesday.surf %>% mutate(datetime = ymd_hms(datetime))

# calculate the hour
tuesday.surf = tuesday.surf %>% mutate(hour = hour(datetime))

tuesday.surf = remove.flagged.data(tuesday.surf)

# summarize by hour
tuesday.avg = tuesday.surf %>%
  group_by(hour) %>% 
  summarize(chl.avg = mean(Chl_HYLB, na.rm = TRUE), phyco.avg = mean(BGA_HYLB, na.rm = TRUE),
            chl.median = median(Chl_HYLB, na.rm = TRUE), phyco.median = median(BGA_HYLB, na.rm = TRUE))

# average chlorophyll
avg.chl.plot = ggplot(tuesday.avg, aes(x = hour, y = chl.avg)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "avg hourly chl (ug/L)", x = "")+
  theme_classic()

# median chlorophyll
median.chl.plot = ggplot(tuesday.avg, aes(x = hour, y = chl.median)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "median hourly chl (ug/L)", x = "hour of the day")+
  theme_classic()

# average phyco
avg.phyco.plot = ggplot(tuesday.avg, aes(x = hour, y = phyco.avg)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "avg hourly phyco (cells/mL)", x = "")+
  theme_classic()


# median phyco
median.phyco.plot = ggplot(tuesday.avg, aes(x = hour, y = phyco.median)) +
  geom_rect(aes(xmin = 5, xmax = 20, ymin = -Inf, ymax = Inf), 
            fill = "lightyellow", alpha = 0.9)+
  geom_rect(aes(xmin = -Inf, xmax = 5, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_rect(aes(xmin = 20, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "steelblue", alpha = 0.9)+
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "median hourly phyco (cells/mL)", x = "hour of the day")+
  theme_classic()

ggarrange(avg.chl.plot, avg.phyco.plot, median.chl.plot, median.phyco.plot, nrow = 2, ncol = 2)
