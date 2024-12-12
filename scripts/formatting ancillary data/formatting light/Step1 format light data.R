### format and combine light data for 2022 and 2024 ###

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(readxl)) install.packages('readxl')
library(readxl)

# read in the light data
light.22 = read_xlsx("./data/raw data/light/LimnoData_2022.xlsx", sheet = 1)

light.24 = read.csv("./data/raw data/light/2024 light profiles.csv")


# rename columns to match our standard format
light.24 = light.24 %>% rename(lake = Lake, date = Date, doy = DOY, year = Year, depth = Depth)

light.22 = light.22 %>% rename(lake = Lake, date = Date, doy = DoY, year = Year, depth = Depth)

# re-order 2024 names so they match 2022
light.24 = light.24 %>% select(names(light.22))

# format date type so they match
light.24 = light.24 %>% mutate(date = mdy(date))
#light.22 = light.22 %>% mutate(date = mdy(date))

# combine the two datasets
light.22.24 = rbind(light.22, light.24)


# save the datasets as both an R file and a csv
write.csv(light.22.24, "./data/formatted data/light/light profiles 2022 and 2024.csv", row.names = FALSE)
save(light.22.24, file = "./data/formatted data/light/light profiles 2022 and 2024.RData")


# make a version that is all depths and dates with light calculated light maps



