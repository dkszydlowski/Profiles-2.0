### try calculating thermocline from the profiles data using Steve's code

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

lake = "R"

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

# filter to one lake for testing and create datetime column
temp_wider = temp_wider %>% 
  filter(lake == "R" & year == "2024") %>% 
  rename(datetime = date) %>% 
  select(-lake, -year, -doy)
  
  # format the data so it matches Steve's code
  temp_wider = temp_wider %>% mutate(DoY = decimal_date(as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")), RoundDoY = yday(datetime))
  
  
  # Function ================================================================================================================
  # Use linear interpolation to find depth where slope = -2 degrees / meter
  #
  # https://stackoverflow.com/questions/52655729/get-x-value-given-y-value-general-root-finding-for-linear-non-linear-interpol
  #
  ## given (x, y) data, find x where the linear interpolation crosses y = y0
  ## the default value y0 = 0 implies root finding
  ## since linear interpolation is just a linear spline interpolation
  ## the function is named RootSpline1
  #
  # The same website has Rootspline3 for cubic splines
  # =========================================================================================================================
  RootSpline1 <- function (x, y, y0, verbose = TRUE) {
    if (is.unsorted(x)) {
      ind <- order(x)
      x <- x[ind]; y <- y[ind]
    }
    z <- y - y0
    ## which piecewise linear segment crosses zero?
    k <- which(z[-1] * z[-length(z)] <= 0)
    ## analytical root finding
    xr <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
    ## make a plot?
    if (verbose) {
      windows()
      plot(x, y, "l"); abline(h = y0, lty = 2)
      points(xr, rep.int(y0, length(xr)))
    }
    ## return roots
    return(unname(xr))
  }
  # ================================================================================================
  
  # Read data
  
  # tchain.data is all rows of Tchain data for Peter 2015
  # Zchain is depths (m) for for tchain.data[,6:14]
  #save(tchain.data,Zchain,file='Peter2015_Tchain.Rdata')
  #load(file='Peter2015_Tchain.Rdata')
  
  tchain.data = temp_wider
  
  Zchain = seq(0, 8, 0.25)
  
  tchain.data = tchain.data %>% mutate(hour = hour(datetime))
  
  # select day range
  doy0 = min(tchain.data$RoundDoY) # 1st day of season
  doyN = max(tchain.data$RoundDoY) # current date, subtract 1 because usually downloaded in morning
  doyvec = c(doy0:doyN)
  ndoy = length(doyvec)
  
  # Select time window within each day
  #tchain.data = tchain.data %>% filter(hour >= 12 & hour <= 16)
  
  # vectors for results
  ZT1a = rep(0,ndoy)
  ZT1b = rep(0,ndoy)
  ZT2 = rep(0,ndoy)
  id = 1
  
  error.days = c()
  
  for(id in 1:ndoy)  {   # LOOP OVER id = DOY counter ****************************
    today = doyvec[id]
    # extract the rows for today
    # Tc = subset(tchain.data,subset=(DoY <= (today + tstop) & DoY >= (today + tstart)) )
    # use profiles only
    Tc = tchain.data %>% filter(RoundDoY == today)
    
    Tprof = colMeans(Tc[ ,2:10],na.rm=T)
    
    # linear first derivative, use first point for surface
    dT = diff(Tprof)
    d1zchain = (Zchain[1:8] + Zchain[2:9])/2  # midpoints
    
    # Thermocline by -2 degrees/m rule
    twotest = RootSpline1(x=d1zchain,y=dT,y0=-2,verbose=F) 
    if(length(twotest) == 1) {
      ZT1a[id] = twotest
      ZT1b[id] = twotest
    }
    if(length(twotest) == 2) {
      ZT1a[id] = twotest[1]
      ZT1b[id] = twotest[2]
    }
    
    # linear second derivative, most negative at steepest slope
    d2T = diff(dT)
    d2zchain = (d1zchain[1:7] + d1zchain[2:8])/2
    # find index of minima
    imin = which.min(d2T)
    
    
    tryCatch({
      # This is the line that might produce an error
      ZT2[id] <- d2zchain[imin]
    }, error = function(e) {
      # Print a message indicating which id caused the error
      message(paste("Error with id", id, ": ", e$message))
      
      error.days <<- c(error.days, id)
      
    })
    
  }  # end loop over DOY *************************************************************
  # 
  # plot thermocline indicators versus day of year
  # windows(width=10,height=6)
  # par(mfrow=c(1,1),mar=c(4, 4.2, 1, 2) + 0.1,cex.axis=1.6,cex.lab=1.8)
  # yrange = range(c(ZT1a,ZT1b,ZT2))
  # plot(doyvec,ZT1a,ylim=yrange,type='b',lwd=2,pch=16,col='deepskyblue',xlab='Day of 2024',ylab='Thermocline (m)',
  #      main = paste(lake, "Lake thermocline"))
  # points(doyvec,ZT1b,ylim=yrange,type='b',lwd=2,pch=16,col='blue')
  # points(doyvec,ZT2,type='b',lwd=2,pch=16,col='red')
  # legend('bottomright',cex=1.5,bty='n',
  #        legend=c('shallow -2 line','deep or only -2 line','steepest slope'),
  #        lwd=c(2,2,2),pch=c(16,16,16),col=c('deepskyblue','blue','red'))
  
  
  
  # make a dataframe of the values from Steve
  thermos = data.frame(doy = doyvec, ZT1a = ZT1a, ZT1b = ZT1b, ZT2 = ZT2)
  
  thermos = thermos %>% pivot_longer(cols = c("ZT1a", "ZT1b", "ZT2"), names_to = "thermocline", values_to = "depth")
  
  # convert error days to doy
  error.days = error.days + doy0-1
  
  thermos = thermos %>% filter(!doy %in% error.days)
  
  # replace all values where the thermocline was not calculated with NA
  
  # zt1a = "shallow -2 line"
  # zt1b = "deep or only -2 line"
  # zt2 = "steepest slope"
  
  
  
  
  ggplot(thermos, aes(x = doy, y = depth, color = thermocline))+
    geom_point()+
    geom_line()+
    theme_classic()+
    scale_y_reverse(limits = c(5, 0))+
    xlim(136, max(thermos$doy))+
    labs(title = paste0(lake, " thermocline"), x = "Day of 2024", y = "depth (m)")+
    scale_colour_manual(labels = c("ZT1a" = "shallow -2 line", "ZT1b" = "deep or only -2 line", "ZT2" = "steepest slope"),
                        values = c("steelblue1", "darkblue", "red")) +   
    theme(legend.position="right")
  





