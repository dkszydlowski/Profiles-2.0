# identify where carbon to chlorophyll measures are available across both years #

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


# the chlorophyll dataframe has depths for chl samples taken at different light depths

man.chl = get(load("./data/formatted data/manual chlorophyll/routines chl 2024 and 2022.RData"))


### plot where samples were taken on top of profiles ###
# read in the profiles data

prof = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))


print(ggplot(prof, aes_string("doy", "depth", fill = "log10(chl_ugL)"))+
        geom_tile()+
        #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
        theme_classic()+
        scale_y_reverse()+
        facet_wrap(year~lake)+
        geom_point(data = man.chl, aes(x = doy, y = depth), pch = 21, color = "thistle", stroke = 0.9, fill = "forestgreen", inherit.aes = FALSE)+
        #scale_fill_distiller(palette = "BrBG")
        scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse"))
