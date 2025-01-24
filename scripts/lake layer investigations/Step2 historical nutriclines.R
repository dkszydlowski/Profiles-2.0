# historical nutriclines


if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


## read in the nutrient data from EDI ##

# Package ID: knb-lter-ntl.351.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Nutrients 1991 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/351/4/99c3d1eb81740bcea565e09f1d56df2b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "lakename",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth_id",     
                 "depth",     
                 "tn_ug",     
                 "tp_ug",     
                 "nh34",     
                 "no23",     
                 "po4",     
                 "comments"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$tn_ug)=="factor") dt1$tn_ug <-as.numeric(levels(dt1$tn_ug))[as.integer(dt1$tn_ug) ]               
if (class(dt1$tn_ug)=="character") dt1$tn_ug <-as.numeric(dt1$tn_ug)
if (class(dt1$tp_ug)=="factor") dt1$tp_ug <-as.numeric(levels(dt1$tp_ug))[as.integer(dt1$tp_ug) ]               
if (class(dt1$tp_ug)=="character") dt1$tp_ug <-as.numeric(dt1$tp_ug)
if (class(dt1$nh34)=="factor") dt1$nh34 <-as.numeric(levels(dt1$nh34))[as.integer(dt1$nh34) ]               
if (class(dt1$nh34)=="character") dt1$nh34 <-as.numeric(dt1$nh34)
if (class(dt1$no23)=="factor") dt1$no23 <-as.numeric(levels(dt1$no23))[as.integer(dt1$no23) ]               
if (class(dt1$no23)=="character") dt1$no23 <-as.numeric(dt1$no23)
if (class(dt1$po4)=="factor") dt1$po4 <-as.numeric(levels(dt1$po4))[as.integer(dt1$po4) ]               
if (class(dt1$po4)=="character") dt1$po4 <-as.numeric(dt1$po4)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$tn_ug <- ifelse((trimws(as.character(dt1$tn_ug))==trimws("NA")),NA,dt1$tn_ug)               
suppressWarnings(dt1$tn_ug <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tn_ug))==as.character(as.numeric("NA"))),NA,dt1$tn_ug))
dt1$tp_ug <- ifelse((trimws(as.character(dt1$tp_ug))==trimws("NA")),NA,dt1$tp_ug)               
suppressWarnings(dt1$tp_ug <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tp_ug))==as.character(as.numeric("NA"))),NA,dt1$tp_ug))
dt1$nh34 <- ifelse((trimws(as.character(dt1$nh34))==trimws("NA")),NA,dt1$nh34)               
suppressWarnings(dt1$nh34 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$nh34))==as.character(as.numeric("NA"))),NA,dt1$nh34))
dt1$no23 <- ifelse((trimws(as.character(dt1$no23))==trimws("NA")),NA,dt1$no23)               
suppressWarnings(dt1$no23 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$no23))==as.character(as.numeric("NA"))),NA,dt1$no23))
dt1$po4 <- ifelse((trimws(as.character(dt1$po4))==trimws("NA")),NA,dt1$po4)               
suppressWarnings(dt1$po4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$po4))==as.character(as.numeric("NA"))),NA,dt1$po4))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(lakename)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth_id)
summary(depth)
summary(tn_ug)
summary(tp_ug)
summary(nh34)
summary(no23)
summary(po4)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$depth_id)) 
summary(as.factor(dt1$comments))
detach(dt1)               



nuts = dt1



# filter to just be LRT
nuts = nuts %>% filter(lakeid %in% c("L", "R", "T"))

# summarize by year, lake, and depthid
nuts.avg = nuts %>% group_by(lakeid, year4, depth_id) %>% 
  summarize(mean.tp = mean(tp_ug, na.rm = TRUE), mean.tn = mean(tn_ug, na.rm = TRUE))


ggplot(nuts.avg, aes(x = mean.tp, y = as.numeric(depth_id), color = as.factor(year4)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  theme_classic()+
  facet_grid(~lakeid)


ggplot(nuts.avg %>% filter(year4 < 2000), aes(x = log10(mean.tn), y = as.numeric(depth_id), color = as.factor(year4)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  theme_classic()+
  facet_grid(~lakeid)

nuts.avg = nuts.avg %>% group_by(depth_id, lakeid) %>%
  summarize(mean.tp = mean(mean.tp, na.rm = TRUE), mean.tn = mean(mean.tn, na.rm = TRUE))


ggplot(nuts.avg, aes(x = log10(mean.tn), y = as.numeric(depth_id)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  theme_classic()+
  facet_grid(~lakeid)


# calculate the mean depth of each depth_id

depths = nuts %>% group_by(lakeid, depth_id) %>% 
  summarize(mean.depth = mean(depth, na.rm = TRUE))

# add the depths to nuts.avg
nuts.avg = nuts.avg %>% left_join(depths, by = c("lakeid", "depth_id"))


ggplot(nuts.avg %>% filter(depth_id != -2 & depth_id != -1), aes(x = (mean.tn), y = as.numeric(mean.depth)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  theme_classic()+
  facet_grid(~lakeid)


ggplot(nuts.avg %>% filter(depth_id != -2 & depth_id != -1), aes(x = (mean.tp), y = as.numeric(mean.depth)))+
  geom_point()+
  geom_path(size = 1)+
  scale_y_reverse()+
  theme_classic()+
  facet_grid(~lakeid)


# view nutrients over time
ggplot(nuts %>% filter(lakeid == "R" & depth > 8), aes(x = year4, y = tp_ug))+
  geom_point()



