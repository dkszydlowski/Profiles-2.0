### first step is to combine all of the profiles data for 2022 and 2024
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(xlsx)) install.packages('xlsx')
library(xlsx)


###### READ IN THE DATA ######
##### 2022 #####

# Depths were entered by hand using the start sampling times and end sampling times
# that were recorded while taking the profiles
profiles = list.files(path = "./data/raw data/profiles/2022")


# tidy the dataframe for each lake and append to a master dataframe
for(i in 1:length(profiles)){
  
  # read in each profile in the data folder
  profile = read.xlsx(paste("./data/raw data/profiles/2022/",profiles[i], sep = ""), sheetIndex = 1) #load in the profile
  
  # rename the columns to make them tidy
  profile = profile %>% rename(chl_ugL = CHL..µg.l.) %>% 
    rename(do_mgL = LDO..mg.l.) %>% 
    rename(do_percent = LDO...Sat.) %>% 
    rename(temp = Temp...C.) %>% 
    rename(pH = pH..Units.) %>% 
    rename(date_time = Date...Time) %>% 
    rename(phyco_cells = PCY..cell.mL.)
  
  # if there is no conductivity, fill that column with NA
  if(!("SpCond..µS.cm." %in% names(profile))){
    profile$SPC = NA
  }
  # if there is conductivity, rename to make tidy
  if("SpCond..µS.cm." %in% names(profile)){
    profile = profile %>% rename(SPC = SpCond..µS.cm.)
  }
  
  # convert date_time to a character, then just cut the date out  
  profile$date_time = as.character(profile$date_time)
  profile$date = strsplit(profile$date_time[1], " ")[[1]][1]
  
  # reorder the columns so they are standardized
  profile = profile %>% select(date_time, date, lake, depth, chl_ugL, do_mgL, do_percent, temp, pH, phyco_cells, SPC) # just the columns I want
  
  # remove points when the sensors were out of water
  profile = profile %>% filter(!is.na(depth)) 
  
  # add to the 'master' dataframe with all of the data
  # if first instance, allProfiles is profile
  # otherwise, cumulatively append to that first instance
  if(i == 1){
    allProfiles22 = profile
  }
  if(i >1){
    allProfiles22 = rbind(allProfiles22, profile)
  }
  
  
}

# replace all 2,000,000 values, where the sonde had an error
allProfiles22 = allProfiles22 %>%  mutate(chl_ugL = replace(chl_ugL, chl_ugL == 2000000, NA))

# remove values from the trash can experiment
allProfiles22 = allProfiles22 %>% filter(!(lake %in% c("trash1", "trash2", "trash3")))

##### 2024 #####
prof.24 = list.files("./data/raw data/profiles/2024")

# we don't want to read in the one file that is formatted as a csv because it is
# duplicated, and we don't need the file that has warnings about the sensors
prof.24 = prof.24[-(grep("csv", prof.24))]
prof.24 = prof.24[-(grep("warning", prof.24))]


for(i in 1:length(prof.24)){
  
  # if(grepl("csv", prof.24[i])){
  #   cur.prof = read.csv(paste("R:/Cascade/Data/2024 Ancillary Sensors/2024 Profile/profiles data/day",
  #                              prof.24[i], sep = "/"), fileEncoding = "UTF-8")
  # }
  cur.prof = read.xlsx(paste("./data/raw data/profiles/2024",
                             prof.24[i], sep = "/"), sheetIndex = 1)
  
  cur.prof = cur.prof %>% filter(!is.na(depth))
  
  cur.prof = cur.prof %>%
    mutate_all(~ replace(., . == 2000000, NA))
  
  # rename the columns to make them tidy
  cur.prof = cur.prof %>% rename(chl_ugL = CHL..µg.l.) %>% 
    rename(do_mgL = LDO..mg.l.) %>% 
    rename(do_percent = LDO...Sat.) %>% 
    rename(temp = Temp...C.) %>% 
    rename(pH = pH..Units.) %>% 
    rename(date_time = Date...Time) %>% 
    rename(phyco_cells = PCY..cell.mL.) %>% 
    rename(SPC = SpCond..µS.cm.)
  
  
  cur.prof = cur.prof %>% mutate(doy = yday(date_time))
  
  
  cur.prof = cur.prof %>% select(lake, depth, date_time, doy, chl_ugL, do_mgL, do_percent, temp, 
                                 pH, phyco_cells, SPC)
  
  
  if(i == 1){all.prof24 = cur.prof}
  if(i > 1){all.prof24 = rbind(all.prof24, cur.prof)}
  
  
}



###### combine profiles ######

# add a year column
all.prof24$year = 2024
allProfiles22$year = 2022

# add a doy column to 2022
allProfiles22$doy = yday(allProfiles22$date_time)

# convert 2024 datetime to character to match
all.prof24 = all.prof24 %>% mutate(date_time = as.character(date_time))

# reorder columns so they match
all.prof24 = all.prof24 %>% select(lake, year, depth, date_time, doy, chl_ugL,
                                   do_mgL, do_percent, temp, pH, phyco_cells, SPC)

allProfiles22 = allProfiles22 %>% select(lake, year, depth, date_time, doy, chl_ugL,
                                   do_mgL, do_percent, temp, pH, phyco_cells, SPC)

# combine the profiles iunto a raw version
# the only cleaning that has been done is converting 2,000,000 values to NA
allProfilesCombined = rbind(allProfiles22, all.prof24)


# save the combined files
# one version as a csv and one as an R file
write.csv(allProfilesCombined, "./data/raw data/profiles/profiles 2022 and 2024 raw.csv", row.names = FALSE)
save(allProfilesCombined, file = "./data/raw data/profiles/profiles 2022 and 2024 raw.RData")

