

##### 2024 #####
# calculate manual chlorophyll for 2024 from the fluorescence values

# equations for converting fluoresence to chl a (ug/L)
# chl a = (Fb - Fa) *Q

# where Fb is the fluorescence before acidification
# and Fa is the fluorescence after acidification

# Q is m * (R/(R-1)) * extraction volume / filter volume
# m is the slope of the calibration curve
# R is the acid ratio from the calibration curve

# read in the data
chl24 = read.csv("./data/raw data/manual chlorophyll/2024_routine_chlorophyll_copy2.csv")


# rename columns so they match the dailies chl dataset
chl24 = chl24 %>% rename(Methanol_blank = methanol_blank_before)


#chl.rfu = read.csv("R:/Cascade/Data/2024 Dailies/2024_daily_chlorophyll.csv")
#chl.rfu = read.csv("Copy_2024_daily_chlorophyll.csv")

# values from Dat+Grace 2024-06-25 calibration curve 
#R = 1.9670762
#m = 0.0008

# values from Dat 2024-06-24 calibration curve 
R = 2.0100576
m = 0.0007

chl24$Chlorophyll <- ((chl24$Fb-chl24$Methanol_blank) - (chl24$Fa-chl24$Methanol_blank)) * (m*(R/(R-1))*(25/chl24$Filter_volume))

#chl24$Date <- mdy(chl24$Date)
chl24$DOY <- as.integer(yday(chl24$Date))



# calculate mean chlorophyll across reps
chl.avg = chl24 %>% filter(Depth <= 3) %>% group_by(Lake) %>% summarize(avg.chl = mean(Chlorophyll))

# save the file as both R file and csv
write.csv(chl24, "./data/formatted data/manual chlorophyll/routines chl 2024.csv", row.names = FALSE)
save(chl24, file = "./data/formatted data/manual chlorophyll/routines chl 2024.RData")

