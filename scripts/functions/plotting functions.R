# functions for plotting profiles data

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)



plot_profiles_heatmap = function(data, plot.year, plot.variable, plot.lake, log.trans = FALSE){
  
  # filter data to our desired data, and exclude couple of points when we tested
  # sampling at deeper depths
  
  if(plot.lake != "all"){
  data = data %>% filter(year == plot.year, lake == plot.lake, depth <= 8)
  
  ggplot(data, aes_string("doy", "depth", fill = plot.variable))+
    geom_tile()+
    #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
    theme_classic()+
    scale_y_reverse()+
    #facet_wrap(~lake)+
    #scale_fill_distiller(palette = "BrBG")
    scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")
  }
  
  
  if(plot.lake == "all"){
    
    data = data %>% filter(year == plot.year, depth <= 8)
    
    ggplot(data, aes_string("doy", "depth", fill = plot.variable))+
      geom_tile()+
      #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
      theme_classic()+
      scale_y_reverse()+
      facet_wrap(~lake)+
      #scale_fill_distiller(palette = "BrBG")
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")
    
  }
}


