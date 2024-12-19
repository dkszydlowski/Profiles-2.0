# format the nutrient data for 2022 and 2024
library(readxl)
library(tidyverse)

# read in the 2022 data
total.nut22 = read_xlsx("./data/raw data/nutrients/1_8_24_TNTP_Cascade2022.xlsx", sheet = 1)

# reformat with column headings
total.nut22[5,6] = "TN"
total.nut22[5,8] = "TP"

total.nut22 = total.nut22[, -c(1, 5, 7)]

names(total.nut22) = total.nut22[5,]

# read in the 2022 run log
total.nut22.log = read_xlsx("./data/raw data/nutrients/Cascade TN_TP_sample_log_2022.xlsx", sheet = 1)

total.nut22 = total.nut22 %>% rename(SID = Identifier)

total.nut22.log = total.nut22.log %>% mutate(SID = as.character(SID))

# join data to run log
total.nut22.log = total.nut22.log %>% left_join(total.nut22, by = c("SID"))

# mutate to fix many decimal places
total.nut22.log = total.nut22.log %>% mutate(TN = as.numeric(TN), TP = as.numeric(TP)) %>% 
  mutate(TP = round(TP, 3), TN = round(TN, 3)) %>% 
  filter(Location != "DHB")


ggplot(total.nut22.log, aes(x = Sample, y = TP))+
  geom_boxplot()+
  facet_wrap(~Location)

ggplot(total.nut22.log, aes(x = Sample, y = TN))+
  geom_boxplot()+
  facet_wrap(~Location)

# read in the 2024 data
total.nut24 = read_xlsx("./data/raw data/nutrients/12_17_24_TNTP_Cascade.xlsx", sheet = 1)

# read in the 2024 run log
total.nut24.log = read.csv("./data/raw data/nutrients/2024_TNTP_cascade sample list.csv")

total.nut24[5,7] = "TN"
total.nut24[5,9] = "TP"

total.nut24 = total.nut24[, -c(1, 5, 6, 8)]

names(total.nut24) = total.nut24[5,]


total.nut24 = total.nut24 %>% rename(SID = Identifier)

total.nut24.log = total.nut24.log %>% mutate(SID = run_sid)

total.nut24.log = total.nut24.log %>% mutate(SID = toupper(as.character(SID)))

# join data to run log
total.nut24.log = total.nut24.log %>% left_join(total.nut24, by = c("SID"))

# mutate to fix many decimal places
total.nut24.log = total.nut24.log %>% mutate(TN = as.numeric(TN), TP = as.numeric(TP)) %>% 
  mutate(TP = round(TP, 3), TN = round(TN, 3)) 

ggplot(total.nut24.log, aes(x = depth, y = TN))+
  geom_boxplot()+
  facet_wrap(~lake)


# combine the datasets
# remove columns we don't need and rename those we want

# just want lake, depth, date, doy, TN, TP
total.nut24.log = total.nut24.log %>% select(lake, depth, date, doy, TN, TP)

total.nut24.log = total.nut24.log %>% mutate(date = mdy(date))

total.nut22.log = total.nut22.log %>% rename(lake = Location, depth = Sample, date = Date) %>% 
  mutate(date = ymd(date)) %>% 
  #doy = yday(date) %>% 
  select(lake, depth, date,TN, TP )


total.nut22.log = total.nut22.log %>% 
  mutate(doy = yday(date)) %>% 
  select(lake, depth, date, doy, TN, TP)

# combine with rbind
total.nuts = rbind(total.nut22.log, total.nut24.log) %>% mutate(year = year(date))

# standardize lake names
total.nuts = total.nuts %>% select(lake, year, date, doy, depth, TN, TP)

total.nuts = total.nuts %>% mutate(lake = replace(lake, lake == "Paul Lake", "L"),
                                   lake = replace(lake, lake == "Peter Lake", "R"),
                                   lake = replace(lake, lake == "Tuesday Lake", "T"))

# add a flag column and flag doy 233 in 2024 for Paul, which is extremely high
total.nuts = total.nuts %>% mutate(flag = NA) %>% 
  mutate(flag = replace(flag, lake == "L" & year == 2024 & doy == 233, 1))

ggplot(total.nuts, aes(x = depth, y = TP))+
  geom_boxplot()+
  facet_wrap(year~lake)

ggplot(total.nuts %>% filter(is.na(flag)), aes(x = depth, y = TN))+
  geom_boxplot()+
  facet_wrap(year~lake)


total.nuts24 = total.nuts %>% filter(year == 2024)

write.csv(total.nuts24, "./data/formatted data/2024 TN TP.csv", row.names = FALSE)
