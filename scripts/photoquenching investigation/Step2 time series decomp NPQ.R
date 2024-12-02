#### time series decomposition ####

# decompose the surface sonde time series to pick up on diel cycles that may be partially
# attributable to non-photochemical quenching


if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

if (!require(zoo)) install.packages('zoo')
library(zoo)

# read in the data 

# read in the Paul surface data 
paul.surf = read.csv("G:/My Drive/Projects and Papers/PhD/Profiles 2.0/Profiles-2.0/data/raw data/surface sonde data 2024/Paul FINAL 2024-10-14.csv")

# convert datetime to a datetime format
paul.surf = paul.surf %>% mutate(datetime = ymd_hms(datetime))

# calculate the hour
paul.surf = paul.surf %>% mutate(hour = hour(datetime))

paul.surf = remove.flagged.data(paul.surf)

# read in the Tuesday surface data 
tuesday.surf = read.csv("G:/My Drive/Projects and Papers/PhD/Profiles 2.0/Profiles-2.0/data/raw data/surface sonde data 2024/Tuesday FINAL 2024-10-14.csv")

# convert datetime to a datetime format
tuesday.surf = tuesday.surf %>% mutate(datetime = ymd_hms(datetime))

# calculate the hour
tuesday.surf = tuesday.surf %>% mutate(hour = hour(datetime))

tuesday.surf = remove.flagged.data(tuesday.surf)



#### TIME SERIES DECOMP #####
# take hourly average throughout the summer
paul.surf.hourly = paul.surf %>% group_by(doy, hour) %>% 
  summarize(mean.chl = mean(Chl_HYLB, na.rm= TRUE))

paul.surf.hourly = paul.surf.hourly %>% mutate(datetime = ymd(paste(year, "01", "01", sep = "-")) + days(doy - 1) + hours(hour))

spectrum(na.approx(paul.surf$Chl_HYLB, rule = 2))

spectrum(na.approx(tuesday.surf$Chl_HYLB, rule = 2))

spectrum(na.approx(paul.surf.hourly$mean.chl, rule = 2))






##### wavelets #####






#### Example code from Paul Hanson #####


# Script to explore your reserve's data
# Zoo 955, Spring 2024

###########################
# Begin user input section

# Set the working directory to wherever this script is located
setwd("/Users/paul/Dropbox/Hanson/Teaching/955 2024")

# Identify the meteorological (met), nutrient (nut), and water quality (WQ) data files for your reserve
metFile = "./APA/met_APA.csv"
nutFile = "./APA/nut_APA.csv"
wqFile  = "./APA/wq_APA.csv"

# End user input section
###########################

# Load the data into separate data frames
datMet = read.csv(metFile)
datNut = read.csv(nutFile)
datWQ  = read.csv(wqFile)

# Create a handy year fraction variable for each data frame for plotting
datMet$YearFrac = datMet$year + datMet$month/12
datNut$YearFrac = datNut$year + datNut$month/12
datWQ$YearFrac  = datWQ$year + datWQ$month/12

# Each reserve can have multiple sampling stations
# Determine the number of stations for each data frame
uMet = unique(datMet$station)
uNut = unique(datNut$station)
uWQ  = unique(datWQ$station)

# Print out the unique stations for each data frame
cat('Unique met stations: ',uMet,'\n')
cat('Unique nut stations: ',uNut,'\n')
cat('Unique WQ  stations: ',uWQ,'\n')

# Select the nth station
nSta = 2
# Select the nth variable
nCol = 43 # 65 is turb, 37 is DO mean, 43 is depth
whichRows = which(datWQ$station==uWQ[nSta])
# Plot the original data
par(mfrow=c(2,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)
myDS = data.frame(YearFrac=datWQ$YearFrac[whichRows],myData = datWQ[whichRows,nCol])
plot(myDS,type='l',xlab = 'Year',ylab=colnames(datWQ[nCol]))
# Create a timeseries object
myTS <- ts(myDS[,2], myDS[1,1], frequency=12)
# find and replace NAs
iNA = which(is.na(myTS))
myTS[iNA] = mean(myTS,na.rm=TRUE)
# Plot the time series to compare with the original data
plot(myTS)
# Decompose timeseries
myTSdecomposed = decompose(myTS)
# Plot the decomposed timeseries
plot(decompose(myTS))

# Run autocorrelation function for various components
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)
acf(na.omit(myTSdecomposed$trend),ylab='ACF Trend')
acf(na.omit(myTSdecomposed$seasonal),ylab='ACF Seasonal')
acf(na.omit(myTSdecomposed$random),ylab='ACF Random')


